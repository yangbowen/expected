#pragma once

#include <cstddef>
#include <concepts>
#include <type_traits>
#include <tuple>
#include <utility>
#include <functional>
#include <optional>
#include <expected>
#include <exception>
#include <coroutine>

namespace details {
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_invocable<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> invocable_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Ret, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_invocable_r<T_Ret, T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> invocable_r_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_nothrow_invocable<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> nothrow_invocable_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::invoke_result<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> invoke_result_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_constructible<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> constructible_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args> requires(!::std::is_void_v<T>)
	inline consteval ::std::is_default_constructible<T> constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args> requires(::std::is_void_v<T>)
	inline consteval ::std::true_type constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_nothrow_constructible<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> nothrow_constructible_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args> requires(!::std::is_void_v<T>)
	inline consteval ::std::is_nothrow_default_constructible<T> nothrow_constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args> requires(::std::is_void_v<T>)
	inline consteval ::std::true_type nothrow_constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline consteval ::std::is_trivially_constructible<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...> trivially_constructible_from_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T> requires(!::std::is_void_v<T>)
	inline consteval ::std::is_trivially_default_constructible<T> trivially_constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T> requires(::std::is_void_v<T>)
	inline consteval ::std::true_type trivially_constructible_from_tuple_impl(::std::index_sequence<>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T, ::std::size_t... idx_args>
	inline consteval ::std::false_type convertible_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T>
	inline consteval auto convertible_tuple_impl(::std::index_sequence<> idxseq) noexcept { return constructible_from_tuple_impl<T, T_Tuple_Args>(idxseq); }
	template<typename T_Tuple_Args, typename T>
	inline consteval ::std::is_convertible<decltype(::std::get<0>(::std::declval<T_Tuple_Args&&>())), T> convertible_tuple_impl(::std::index_sequence<0>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T, ::std::size_t... idx_args>
	inline consteval ::std::false_type nothrow_convertible_tuple_impl(::std::index_sequence<idx_args...>) noexcept { return{}; }
	template<typename T_Tuple_Args, typename T>
	inline consteval auto nothrow_convertible_tuple_impl(::std::index_sequence<> idxseq) noexcept { return nothrow_constructible_from_tuple_impl<T, T_Tuple_Args>(idxseq); }
	template<typename T_Tuple_Args, typename T>
	inline consteval ::std::is_nothrow_convertible<decltype(::std::get<0>(::std::declval<T_Tuple_Args&&>())), T> nothrow_convertible_tuple_impl(::std::index_sequence<0>) noexcept { return{}; }
	template<typename T_Tuple, ::std::size_t... idx>
	inline constexpr auto forward_tuple_impl(T_Tuple&& tuple, ::std::index_sequence<idx...>) noexcept {
		return ::std::forward_as_tuple(::std::get<idx>(::std::forward<T_Tuple>(tuple))...);
	}
	template<typename T, typename T_Tuple, ::std::size_t... idx>
	inline constexpr auto forward_like_tuple_impl(T_Tuple&& tuple, ::std::index_sequence<idx...>) noexcept {
		return ::std::forward_as_tuple(::std::forward_like<T>(::std::get<idx>(::std::forward<T_Tuple>(tuple)))...);
	}
}

template<typename T, typename T_Tuple_Args>
concept invocable_from_tuple = details::invocable_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T, typename T_Ret, typename T_Tuple_Args>
concept invocable_r_from_tuple = invocable_from_tuple<T, T_Tuple_Args> && details::invocable_r_from_tuple_impl<T, T_Ret, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T, typename T_Tuple_Args>
concept nothrow_invocable_from_tuple = invocable_from_tuple<T, T_Tuple_Args> && details::nothrow_invocable_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T, typename T_Ret, typename T_Tuple_Args>
concept nothrow_invocable_r_from_tuple = nothrow_invocable_from_tuple<T, T_Tuple_Args> && invocable_r_from_tuple<T, T_Ret, T_Tuple_Args>;
template<typename T, typename T_Tuple_Args>
using invoke_result_from_tuple_t = typename decltype(details::invoke_result_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}))::type;
template<typename T, typename T_Tuple_Args>
concept constructible_from_tuple = details::constructible_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T, typename T_Tuple_Args>
concept nothrow_constructible_from_tuple = constructible_from_tuple<T, T_Tuple_Args> && details::nothrow_constructible_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T, typename T_Tuple_Args>
concept trivially_constructible_from_tuple = nothrow_constructible_from_tuple<T, T_Tuple_Args> && details::trivially_constructible_from_tuple_impl<T, T_Tuple_Args>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T_Tuple_Args, typename T>
concept convertible_tuple_v = details::convertible_tuple_impl<T_Tuple_Args, T>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
template<typename T_Tuple_Args, typename T>
concept nothrow_convertible_tuple_v = convertible_tuple_v<T_Tuple_Args, T> && details::nothrow_convertible_tuple_impl<T_Tuple_Args, T>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{}).value;
inline constexpr auto forward_tuple(auto&& tuple) noexcept
	requires requires { ::std::tuple_size_v<::std::remove_reference_t<decltype(tuple)>>; } {
	return details::forward_tuple_impl(::std::forward<decltype(tuple)>(tuple), ::std::make_index_sequence<::std::tuple_size_v<::std::remove_reference_t<decltype(tuple)>>>{});
}
template<typename T>
inline constexpr auto forward_tuple_like(auto&& tuple) noexcept
	requires requires { ::std::tuple_size_v<::std::remove_reference_t<decltype(tuple)>>; } {
	return details::forward_tuple_like_impl<T>(::std::forward<decltype(tuple)>(tuple), ::std::make_index_sequence<::std::tuple_size_v<::std::remove_reference_t<decltype(tuple)>>>{});
}

namespace placeholders {
	namespace details {
		struct _none_t {};
		struct _all_t {};
		template<int idx> requires (idx > 0)
		struct _i_t {};

		template<int idx> requires (idx > 0)
		struct ::std::is_placeholder<_i_t<idx>> : ::std::integral_constant<int, idx> {};
	}
	inline constexpr details::_none_t _none{};
	inline constexpr details::_all_t _all{};
	template<int idx> requires (idx > 0)
	inline constexpr details::_i_t<idx> _i{};
}

template<typename T>
struct is_multi_placeholder {
	static constexpr bool value = false;
};
template<typename T> requires ::std::is_placeholder_v<T>
struct is_multi_placeholder<T> {
	static constexpr bool value = true;
	template<typename T_Tuple> requires (::std::is_placeholder_v<T> <= ::std::tuple_size_v<::std::remove_reference_t<T_Tuple>>)
	static constexpr auto operator()(T_Tuple&& tuple) noexcept {
		return ::std::forward_as_tuple(::std::get<::std::is_placeholder_v<T> - 1>(::std::forward<T_Tuple>(tuple)));
	}
};
template<>
struct is_multi_placeholder<placeholders::details::_none_t> {
	static constexpr bool value = true;
	template<typename T_Tuple> requires requires { ::std::tuple_size_v<::std::remove_reference_t<T_Tuple>>; }
	static constexpr ::std::tuple<> operator()(T_Tuple&& tuple) noexcept { return {}; }
};
template<>
struct is_multi_placeholder<placeholders::details::_all_t> {
	static constexpr bool value = true;
	template<typename T_Tuple> requires requires { ::std::tuple_size_v<::std::remove_reference_t<T_Tuple>>; }
	static constexpr auto operator()(T_Tuple&& tuple) noexcept { return forward_tuple(::std::forward<T_Tuple>(tuple)); }
};

namespace details {
	template<typename T, typename T_Tuple_Args, ::std::size_t... idx_args>
	inline constexpr T* construct_from_tuple_at_impl(T* ptr, T_Tuple_Args&& tuple_args, ::std::index_sequence<idx_args...>)
		noexcept(::std::is_nothrow_constructible_v<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...>)
		requires(::std::is_constructible_v<T, decltype(::std::get<idx_args>(::std::declval<T_Tuple_Args&&>()))...>) {
		return ::std::construct_at<T>(ptr, ::std::get<idx_args>(::std::forward<T_Tuple_Args&&>(tuple_args))...);
	}
	template<typename T, typename T_Tuple_Args>
	struct construct_from_tuple_at_t {
		static constexpr T* operator()(T* ptr, T_Tuple_Args&& tuple_args)
			noexcept(nothrow_constructible_from_tuple<T, T_Tuple_Args>)
			requires(constructible_from_tuple<T, T_Tuple_Args>) {
			return details::construct_from_tuple_at_impl<T, T_Tuple_Args>(ptr, ::std::forward<T_Tuple_Args>(tuple_args), ::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{});
		}
	};
	template<>
	struct construct_from_tuple_at_t<void, void> {
		template<typename T, typename T_Tuple_Args>
		static constexpr T* operator()(T* ptr, T_Tuple_Args&& tuple_args)
			noexcept(nothrow_constructible_from_tuple<T, T_Tuple_Args>)
			requires(constructible_from_tuple<T, T_Tuple_Args>) {
			return details::construct_from_tuple_at_impl<T, T_Tuple_Args>(ptr, ::std::forward<T_Tuple_Args>(tuple_args), ::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T_Tuple_Args>>>{});
		}
	};

	template<typename T>
	struct translate_multi_placeholder {
		template<typename T_Tuple> requires requires { ::std::tuple_size_v<::std::remove_reference_t<T_Tuple>>; }
		static constexpr auto operator()(T_Tuple&&, T&& x) noexcept { return ::std::forward_as_tuple(::std::forward<T>(x)); }
	};
	template<typename T> requires is_multi_placeholder<::std::remove_reference_t<T>>::value
	struct translate_multi_placeholder {
		template<typename T_Tuple>
		static constexpr auto operator()(T_Tuple&& tuple, T&&) noexcept
			requires requires { ::std::declval<is_multi_placeholder<::std::remove_reference_t<T>>>()(::std::declval<T_Tuple&&>()); } {
			return is_multi_placeholder<::std::remove_reference_t<T>>{}(::std::forward<T_Tuple>(tuple));
		}
	};
	template<typename T_Fn, typename... T_Args>
	struct bind_ref_obj_t {
		T_Fn&& m_ref_fn;
		::std::tuple<T_Args&&...> m_tuple_ref_args;
		explicit constexpr bind_ref_obj_t(T_Fn&& fn, ::std::tuple<T_Args&&...>&& tuple_ref_args) noexcept : m_ref_fn(::std::forward<T_Fn>(fn)), m_tuple_ref_args(::std::move(tuple_ref_args)) {}
		template<typename T_Tuple_Args_Unbound>
		constexpr auto translate_args(T_Tuple_Args_Unbound&& tuple_args_unbound) noexcept
			requires requires { ::std::declval<translate_multi_placeholder<T_Args>>()(::std::declval<T_Tuple_Args_Unbound>(), ::std::declval<T_Args&&>())...; } {
			return this->translate_args_impl(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound));
		}
		template<typename... T_Args_Unbound>
		constexpr decltype(auto) operator()(T_Args_Unbound&&... args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward_as_tuple(::std::forward<T_Args_Unbound>(args_unbound)...)));
		}
		template<typename T_Tuple_Args_Unbound>
		constexpr decltype(auto) apply(T_Tuple_Args_Unbound&& tuple_args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound)));
		}
	private:
		using idxseq_args = ::std::make_index_sequence<sizeof...(T_Args)>;
		template<typename T_Tuple_Args_Unbound, ::std::size_t... idx_args>
		constexpr auto translate_args_impl(T_Tuple_Args_Unbound&& tuple_args_unbound, ::std::index_sequence<idx_args...> = idxseq_args{}) noexcept
			requires requires { ::std::declval<translate_multi_placeholder<T_Args>>()(::std::declval<T_Tuple_Args_Unbound>(), ::std::declval<T_Args&&>())...; } {
			return ::std::tuple_cat(
				translate_multi_placeholder<T_Args>{}(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound), ::std::get<idx_args>(::std::move(m_tuple_ref_args)))...
			);
		}
	};
	template<typename T_Fn, typename... T_Args>
	struct bind_ref_t {
		static constexpr bind_ref_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};
	template<>
	struct bind_ref_t<void> {
		template<typename T_Fn, typename... T_Args>
		static constexpr bind_ref_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};

	template<typename T_Fn, typename... T_Args>
	struct bind_ref_front_obj_t {
		T_Fn&& m_ref_fn;
		::std::tuple<T_Args&&...> m_tuple_ref_args;
		explicit constexpr bind_ref_front_obj_t(T_Fn&& fn, ::std::tuple<T_Args&&...>&& tuple_ref_args) noexcept : m_ref_fn(::std::forward<T_Fn>(fn)), m_tuple_ref_args(::std::move(tuple_ref_args)) {}
		template<typename T_Tuple_Args_Unbound>
		constexpr auto translate_args(T_Tuple_Args_Unbound&& tuple_args_unbound) noexcept {
			return ::std::tuple_cat(::std::move(m_tuple_ref_args), ::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound));
		}
		template<typename... T_Args_Unbound>
		constexpr decltype(auto) operator()(T_Args_Unbound&&... args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward_as_tuple(::std::forward<T_Args_Unbound>(args_unbound)...)));
		}
		template<typename T_Tuple_Args_Unbound>
		constexpr decltype(auto) apply(T_Tuple_Args_Unbound&& tuple_args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound)));
		}
	};
	template<typename T_Fn, typename... T_Args>
	struct bind_ref_front_t {
		static constexpr bind_ref_front_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_front_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};
	template<>
	struct bind_ref_front_t<void> {
		template<typename T_Fn, typename... T_Args>
		static constexpr bind_ref_front_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_front_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};

	template<typename T_Fn, typename... T_Args>
	struct bind_ref_back_obj_t {
		T_Fn&& m_ref_fn;
		::std::tuple<T_Args&&...> m_tuple_ref_args;
		explicit constexpr bind_ref_back_obj_t(T_Fn&& fn, ::std::tuple<T_Args&&...>&& tuple_ref_args) noexcept : m_ref_fn(::std::forward<T_Fn>(fn)), m_tuple_ref_args(::std::move(tuple_ref_args)) {}
		template<typename T_Tuple_Args_Unbound>
		constexpr auto translate_args(T_Tuple_Args_Unbound&& tuple_args_unbound) noexcept {
			return ::std::tuple_cat(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound), ::std::move(m_tuple_ref_args));
		}
		template<typename... T_Args_Unbound>
		constexpr decltype(auto) operator()(T_Args_Unbound&&... args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<::std::tuple<T_Args_Unbound&&...>>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward_as_tuple(::std::forward<T_Args_Unbound>(args_unbound)...)));
		}
		template<typename T_Tuple_Args_Unbound>
		constexpr decltype(auto) apply(T_Tuple_Args_Unbound&& tuple_args_unbound)
			noexcept(nothrow_invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>)
			requires(invocable_from_tuple<T_Fn, decltype(this->translate_args(::std::declval<T_Tuple_Args_Unbound&&>()))>) {
			return ::std::apply(::std::forward<T_Fn>(m_ref_fn), this->translate_args(::std::forward<T_Tuple_Args_Unbound>(tuple_args_unbound)));
		}
	};
	template<typename T_Fn, typename... T_Args>
	struct bind_ref_back_t {
		static constexpr bind_ref_back_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_back_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};
	template<>
	struct bind_ref_back_t<void> {
		template<typename T_Fn, typename... T_Args>
		static constexpr bind_ref_back_obj_t<T_Fn, T_Args...> operator()(T_Fn&& fn, T_Args&&... args) noexcept {
			return bind_ref_back_obj_t<T_Fn, T_Args...>(::std::forward<T_Fn>(fn), ::std::forward<T_Args>(args)...);
		}
	};
}

template<typename T = void, typename T_Tuple_Args = void>
inline constexpr details::construct_from_tuple_at_t<T, T_Tuple_Args> construct_from_tuple_at{};

template<typename T_Fn = void, typename... T_Args>
inline constexpr details::bind_ref_t<T_Fn, T_Args...> bind_ref{};
template<typename T_Fn = void, typename... T_Args>
inline constexpr details::bind_ref_front_t<T_Fn, T_Args...> bind_ref_front{};
template<typename T_Fn = void, typename... T_Args>
inline constexpr details::bind_ref_back_t<T_Fn, T_Args...> bind_ref_back{};

template<typename... T_Args>
inline constexpr void dummy_func(T_Args&&...) noexcept {}

template<typename... T_Refs>
struct assign_to {
	explicit constexpr assign_to(T_Refs&&... refs) noexcept : m_tuple_ref(::std::forward_as_tuple(::std::forward<T_Refs>(refs)...)) {}
	template<typename... T_Args> requires(::std::is_assignable_v<T_Refs&&, T_Args&&> && ...)
	constexpr void operator()(T_Args&&... args) noexcept(::std::conjunction_v<::std::is_nothrow_assignable<T_Refs&&, T_Args&&>...>) {
		impl(::std::forward<T_Args>(args)..., ::std::make_index_sequence<sizeof...(T_Refs)>{});
	}
private:
	::std::tuple<T_Refs&&...> m_tuple_ref;
	template<typename... T_Args, ::std::size_t... idx_args> requires(::std::is_assignable_v<T_Refs&&, T_Args&&> && ...)
	constexpr void impl(T_Args&&... args, ::std::index_sequence<idx_args...>) noexcept(::std::conjunction_v<::std::is_nothrow_assignable<T_Refs&&, T_Args&&>...>) {
		dummy_func((::std::get<idx_args>(::std::forward<::std::tuple<T_Refs&&...>>(m_tuple_ref)) = ::std::forward<T_Args>(args), 0)...);
	}
};
template<typename T_Ref>
struct assign_to<T_Ref> {
	explicit constexpr assign_to(T_Ref&& ref) noexcept : m_ref(::std::forward<T_Ref>(ref)) {}
	template<typename T_Arg> requires(::std::is_assignable_v<T_Ref&&, T_Arg&&>)
	constexpr decltype(auto) operator()(T_Arg&& arg) noexcept(::std::is_nothrow_assignable_v<T_Ref&&, T_Arg&&>) {
		return (::std::forward<T_Ref>(m_ref) = ::std::forward<T_Arg>(arg));
	}
private:
	T_Ref&& m_ref;
};
template<typename... T_Refs>
explicit assign_to(T_Refs&&...) noexcept->assign_to<T_Refs...>;

template<typename T>
struct ValueWrapper {
	using value_type = T;
	using pointer_type = T*;
	template<typename T_Like>
	using ref_like_type = decltype(::std::forward_like<T_Like>(::std::declval<T>()));
	template<typename T_Like>
	using tuple_ref_like_type = decltype(::std::forward_as_tuple(::std::forward_like<T_Like>(::std::declval<T>())));
	constexpr ValueWrapper()
		noexcept(::std::is_nothrow_default_constructible_v<value_type>)
		requires(::std::is_default_constructible_v<value_type>) = default;
	constexpr ValueWrapper(const ValueWrapper&)
		noexcept(::std::is_nothrow_copy_constructible_v<value_type>)
		requires(::std::is_copy_constructible_v<value_type>) = default;
	constexpr ValueWrapper(ValueWrapper&&)
		noexcept(::std::is_nothrow_move_constructible_v<value_type>)
		requires(::std::is_move_constructible_v<value_type>) = default;
	constexpr ValueWrapper& operator=(const ValueWrapper&)
		noexcept(::std::is_nothrow_copy_assignable_v<value_type>)
		requires(::std::is_copy_assignable_v<value_type>) = default;
	constexpr ValueWrapper& operator=(ValueWrapper&&)
		noexcept(::std::is_nothrow_move_assignable_v<value_type>)
		requires(::std::is_move_assignable_v<value_type>) = default;
	template<typename... T_Args>
	constexpr ValueWrapper(T_Args&&... args)
		noexcept(::std::is_nothrow_constructible_v<value_type, T_Args&&...>)
		requires(::std::is_constructible_v<value_type, T_Args&&...>)
	: m(::std::forward<T_Args>(args)...) {}
	template<typename T_Arg>
	explicit(!::std::is_convertible_v<T_Arg&&, value_type>)
	constexpr ValueWrapper(T_Arg&& arg)
		noexcept(::std::is_nothrow_constructible_v<value_type, T_Arg&&>)
		requires(::std::is_constructible_v<value_type, T_Arg&&>)
	: m(::std::forward<T_Arg>(arg)) {}
	template<typename T_Self>
	constexpr decltype(auto) operator*(this T_Self&& self) noexcept { return ::std::forward_like<T_Self>(self.m); }
	template<typename T_Self>
	constexpr auto tuple_ref(this T_Self&& self) noexcept { return ::std::forward_as_tuple(::std::forward_like<T_Self>(self.m)); }
private:
	value_type m;
};
template<typename T>
struct ValueWrapper<T&> {
	using value_type = T&;
	using pointer_type = T*;
	template<typename T_Like>
	using ref_like_type = decltype(::std::forward_like<T_Like&>(::std::declval<T&>()));
	template<typename T_Like>
	using tuple_ref_like_type = decltype(::std::forward_as_tuple(::std::forward_like<T_Like&>(::std::declval<T&>())));
	constexpr ValueWrapper(const ValueWrapper&) noexcept = default;
	constexpr ValueWrapper(ValueWrapper&&) noexcept = default;
	constexpr ValueWrapper& operator=(const ValueWrapper&) noexcept = default;
	constexpr ValueWrapper& operator=(ValueWrapper&&) noexcept = default;
	template<typename T_Arg>
	explicit(!::std::is_convertible_v<T_Arg&&, value_type>)
	constexpr ValueWrapper(T_Arg&& arg)
		noexcept(::std::is_nothrow_constructible_v<value_type, T_Arg&&>)
		requires(::std::is_constructible_v<value_type, T_Arg&&>) {
		value_type value(::std::forward<T_Arg>(arg));
		m_ptr = ::std::addressof(value);
	}
	template<typename T_Self>
	constexpr decltype(auto) operator*(this T_Self&& self) noexcept { return ::std::forward_like<T_Self&>(*self.m_ptr); }
	template<typename T_Self>
	constexpr auto tuple_ref(this T_Self&& self) noexcept { return ::std::forward_as_tuple(::std::forward_like<T_Self&>(*self.m_ptr)); }
private:
	pointer_type m_ptr;
};
template<typename T>
struct ValueWrapper<T&&> {
	using value_type = T&&;
	using pointer_type = T*;
	template<typename T_Like>
	using ref_like_type = decltype(::std::forward_like<T_Like>(::std::declval<T>()));
	template<typename T_Like>
	using tuple_ref_like_type = decltype(::std::forward_as_tuple(::std::forward_like<T_Like>(::std::declval<T>())));
	constexpr ValueWrapper(const ValueWrapper&) noexcept = default;
	constexpr ValueWrapper(ValueWrapper&&) noexcept = default;
	constexpr ValueWrapper& operator=(const ValueWrapper&) noexcept = default;
	constexpr ValueWrapper& operator=(ValueWrapper&&) noexcept = default;
	template<typename T_Arg>
	explicit(!::std::is_convertible_v<T_Arg&&, value_type>)
	constexpr ValueWrapper(T_Arg&& arg)
		noexcept(::std::is_nothrow_constructible_v<value_type, T_Arg&&>)
		requires(::std::is_constructible_v<value_type, T_Arg&&>) {
		value_type value(::std::forward<T_Arg>(arg));
		m_ptr = ::std::addressof(static_cast<T&>(value));
	}
	template<typename T_Self>
	constexpr decltype(auto) operator*(this T_Self&& self) noexcept { return ::std::forward_like<T_Self>(*self.m_ptr); }
	template<typename T_Self>
	constexpr auto tuple_ref(this T_Self&& self) noexcept { return ::std::forward_as_tuple(::std::forward_like<T_Self>(*self.m_ptr)); }
private:
	pointer_type m_ptr;
};
template<typename T> requires(::std::is_void_v<T>)
struct ValueWrapper<T> {
	using value_type = void;
	template<typename T_Like>
	using ref_like_type = void;
	template<typename T_Like>
	using tuple_ref_like_type = ::std::tuple<>;
	template<typename T_Self>
	constexpr void operator*(this T_Self&&) noexcept {}
	template<typename T_Self>
	constexpr ::std::tuple<> tuple_ref(this T_Self&&) noexcept { return {}; }
};

namespace details {
	struct tag_uninitialized_t {};
	inline constexpr tag_uninitialized_t tag_uninitialized{};
	template<typename T_Value, typename T_Error>
	class CoroutinePromise_Expected;
}

template<typename T_Value, typename T_Error>
class Expected {
public:
	template<typename T_Value_Other, typename T_Error_Other>
	friend class Expected;
	template<typename T_Value_Other, typename T_Error_Other>
	friend class details::CoroutinePromise_Expected;
	using value_type = T_Value;
	using error_type = T_Error;
private:
	using value_storage_type = ValueWrapper<T_Value>;
	using error_storage_type = ValueWrapper<T_Error>;
	union {
		value_storage_type m_value;
		error_storage_type m_error;
	};
	bool m_is_unexpected{};
	bool m_is_uninitialized{};
	template<typename T_Fn, typename T_Self> requires(invocable_from_tuple<T_Fn, typename value_storage_type::template tuple_ref_like_type<T_Self>>)
	using invoke_value_result_type = invoke_result_from_tuple_t<T_Fn, typename value_storage_type::template tuple_ref_like_type<T_Self>>;
	template<typename T_Fn, typename T_Self> requires(invocable_from_tuple<T_Fn, typename error_storage_type::template tuple_ref_like_type<T_Self>>)
	using invoke_error_result_type = invoke_result_from_tuple_t<T_Fn, typename error_storage_type::template tuple_ref_like_type<T_Self>>;
public:
	template<typename T_Value_Other, typename T_Error_Other = T_Error>
	using rebind_type = Expected<T_Value_Other, T_Error_Other>;
	constexpr Expected()
		noexcept(::std::is_nothrow_default_constructible_v<value_storage_type>)
		requires(::std::is_default_constructible_v<value_storage_type>)
	: m_is_unexpected{}, m_value{} {}
	constexpr Expected(const Expected& x)
		noexcept(::std::is_nothrow_copy_constructible_v<value_storage_type> && ::std::is_nothrow_copy_assignable_v<error_storage_type>)
		requires(::std::is_copy_constructible_v<value_storage_type> && ::std::is_copy_constructible_v<error_storage_type> && !(::std::is_trivially_copy_constructible_v<value_storage_type> && ::std::is_trivially_copy_constructible_v<error_storage_type>))
	: m_is_unexpected(x.m_is_unexpected), m_is_uninitialized(x.m_is_uninitialized) {
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), x.m_error);
			} else {
				::std::construct_at(::std::addressof(m_value), x.m_value);
			}
		}
	}
	constexpr Expected(const Expected& x) noexcept requires(::std::is_trivially_copy_constructible_v<value_storage_type> && ::std::is_trivially_copy_constructible_v<error_storage_type>) = default;
	constexpr Expected(Expected&& x)
		noexcept(::std::is_nothrow_move_constructible_v<value_storage_type> && ::std::is_nothrow_move_assignable_v<error_storage_type>)
		requires(::std::is_move_constructible_v<value_storage_type> && ::std::is_move_constructible_v<error_storage_type> && !(::std::is_trivially_move_constructible_v<value_storage_type> && ::std::is_trivially_move_constructible_v<error_storage_type>))
	: m_is_unexpected(x.m_is_unexpected), m_is_uninitialized(x.m_is_uninitialized) {
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), ::std::move(x.m_error));
			} else {
				::std::construct_at(::std::addressof(m_value), ::std::move(x.m_value));
			}
		}
	}
	constexpr Expected(Expected&& x) noexcept requires(::std::is_trivially_move_constructible_v<value_storage_type> && ::std::is_trivially_move_constructible_v<error_storage_type>) = default;
	template<typename T_Value_Other, typename T_Error_Other>
	explicit(!convertible_tuple_v<typename ValueWrapper<T_Value_Other>::template tuple_ref_like_type<const int&>, value_storage_type> || !convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<const int&>, error_storage_type>)
	constexpr Expected(const Expected<T_Value_Other, T_Error_Other>& x)
		noexcept(nothrow_constructible_from_tuple<value_storage_type, typename ValueWrapper<T_Value_Other>::template tuple_ref_like_type<const int&>> && nothrow_constructible_from_tuple<error_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<const int&>>)
		requires(constructible_from_tuple<value_storage_type, typename ValueWrapper<T_Value_Other>::template tuple_ref_like_type<const int&>> && constructible_from_tuple<error_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<const int&>>)
	: m_is_unexpected(x.m_is_unexpected), m_is_uninitialized(x.m_is_uninitialized) {
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), *::std::forward_like<decltype(x)>(x.m_error));
			} else {
				::std::construct_at(::std::addressof(m_value), *::std::forward_like<decltype(x)>(x.m_value));
			}
		}
	}
	template<typename T_Value_Other, typename T_Error_Other>
	explicit(!convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>, value_storage_type> || !convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>, error_storage_type>)
	constexpr Expected(Expected<T_Value_Other, T_Error_Other>&& x)
		noexcept(nothrow_constructible_from_tuple<value_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>> && nothrow_constructible_from_tuple<error_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>>)
		requires(constructible_from_tuple<value_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>> && constructible_from_tuple<error_storage_type, typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>>)
	: m_is_unexpected(x.m_is_unexpected), m_is_uninitialized(x.m_is_uninitialized) {
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), *::std::forward_like<decltype(x)>(x.m_error));
			} else {
				::std::construct_at(::std::addressof(m_value), *::std::forward_like<decltype(x)>(x.m_value));
			}
		}
	}
	template<typename T_Arg = value_type>
	explicit(!::std::is_convertible_v<T_Arg&&, value_storage_type>)
	constexpr Expected(T_Arg&& arg)
		noexcept(::std::is_nothrow_constructible_v<value_storage_type, T_Arg&&>)
		requires(::std::is_constructible_v<value_storage_type, T_Arg&&>)
	: m_is_unexpected{} {
		::std::construct_at(::std::addressof(m_value), ::std::forward<T_Arg>(arg));
	}
	template<typename... T_Args>
	constexpr Expected(::std::in_place_t, T_Args&&... args)
		noexcept(::std::is_nothrow_constructible_v<value_storage_type, T_Args&&...>)
		requires(::std::is_constructible_v<value_storage_type, T_Args&&...>)
	: m_is_unexpected{} {
		::std::construct_at(::std::addressof(m_value), ::std::forward<T_Args>(args)...);
	}
	constexpr Expected(::std::in_place_t)
		noexcept(::std::is_nothrow_default_constructible_v<value_storage_type>)
		requires(::std::is_default_constructible_v<value_storage_type>)
	: m_is_unexpected{} {
		::std::construct_at(::std::addressof(m_value));
	}
	template<typename T_Tuple_Args>
	constexpr Expected(::std::in_place_t, ::std::piecewise_construct_t, T_Tuple_Args&& tuple_args)
		noexcept(noexcept(construct_from_tuple_at(::std::declval<value_storage_type*>(), ::std::declval<T_Tuple_Args&&>())))
		requires requires(value_storage_type* ptr, T_Tuple_Args&& tuple_args) { construct_from_tuple_at(ptr, ::std::forward<T_Tuple_Args>(tuple_args)); }
	: m_is_unexpected{} {
		construct_from_tuple_at(::std::addressof(m_value), ::std::forward<T_Tuple_Args>(tuple_args));
	}
	template<typename... T_Args>
	constexpr Expected(::std::unexpect_t, T_Args&&... args)
		noexcept(::std::is_nothrow_constructible_v<error_storage_type, T_Args&&...>)
		requires(::std::is_constructible_v<error_storage_type, T_Args&&...>)
	: m_is_unexpected{ true } {
		::std::construct_at(::std::addressof(m_error), ::std::forward<T_Args>(args)...);
	}
	constexpr Expected(::std::unexpect_t)
		noexcept(::std::is_nothrow_default_constructible_v<error_storage_type>)
		requires(::std::is_default_constructible_v<error_storage_type>)
	: m_is_unexpected{ true } {
		::std::construct_at(::std::addressof(m_error));
	}
	template<typename T_Tuple_Args>
	constexpr Expected(::std::unexpect_t, ::std::piecewise_construct_t, T_Tuple_Args&& tuple_args)
		noexcept(noexcept(construct_from_tuple_at(::std::declval<error_storage_type*>(), ::std::declval<T_Tuple_Args&&>())))
		requires requires(error_storage_type* ptr, T_Tuple_Args&& tuple_args) { construct_from_tuple_at(ptr, ::std::forward<T_Tuple_Args>(tuple_args)); }
	: m_is_unexpected{ true } {
		construct_from_tuple_at(::std::addressof(m_error), ::std::forward<T_Tuple_Args>(tuple_args));
	}
	constexpr ~Expected() noexcept {
		uninitialize();
	}
	constexpr ~Expected() noexcept requires(::std::is_trivially_destructible_v<value_storage_type> && ::std::is_trivially_destructible_v<error_storage_type>) = default;
	constexpr Expected& operator=(const Expected& x)
		noexcept(::std::is_nothrow_copy_constructible_v<value_storage_type> && ::std::is_nothrow_copy_constructible_v<error_storage_type>)
		requires(::std::is_copy_constructible_v<value_storage_type> && ::std::is_copy_constructible_v<error_storage_type>) {
		if constexpr (
			::std::is_copy_assignable_v<value_storage_type> && ::std::is_copy_assignable_v<error_storage_type>
			&& ((::std::is_nothrow_copy_assignable_v<value_storage_type> && ::std::is_nothrow_copy_assignable_v<error_storage_type>) || !(::std::is_nothrow_copy_constructible_v<value_storage_type> && ::std::is_nothrow_copy_constructible_v<error_storage_type>))
			) {
			if (!m_is_uninitialized && !x.m_is_uninitialized && m_is_unexpected == x.m_is_unexpected) {
				if (m_is_unexpected) {
					m_error = x.m_error;
				} else {
					m_value = x.m_value;
				}
				return *this;
			}
		}
		uninitialize();
		m_is_unexpected = x.m_is_unexpected;
		m_is_uninitialized = x.m_is_uninitialized;
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), x.m_error);
			} else {
				::std::construct_at(::std::addressof(m_value), x.m_value);
			}
		}
		return *this;
	}
	constexpr Expected& operator=(Expected&& x)
		noexcept(::std::is_nothrow_move_constructible_v<value_storage_type> && ::std::is_nothrow_move_constructible_v<error_storage_type>)
		requires(::std::is_move_constructible_v<value_storage_type> && ::std::is_move_constructible_v<error_storage_type>) {
		if constexpr (
			::std::is_move_assignable_v<value_storage_type> && ::std::is_move_assignable_v<error_storage_type>
			&& ((::std::is_nothrow_move_assignable_v<value_storage_type> && ::std::is_nothrow_move_assignable_v<error_storage_type>) || !(::std::is_nothrow_move_constructible_v<value_storage_type> && ::std::is_nothrow_move_constructible_v<error_storage_type>))
			) {
			if (!m_is_uninitialized && !x.m_is_uninitialized && m_is_unexpected == x.m_is_unexpected) {
				if (m_is_unexpected) {
					m_error = ::std::move(x.m_error);
				} else {
					m_value = ::std::move(x.m_value);
				}
				return *this;
			}
		}
		uninitialize();
		m_is_unexpected = x.m_is_unexpected;
		m_is_uninitialized = x.m_is_uninitialized;
		if (!m_is_uninitialized) {
			if (m_is_unexpected) {
				::std::construct_at(::std::addressof(m_error), ::std::move(x.m_error));
			} else {
				::std::construct_at(::std::addressof(m_value), ::std::move(x.m_value));
			}
		}
		return *this;
	}
	explicit constexpr operator bool() noexcept { return !m_is_unexpected; }
	constexpr bool has_value() noexcept { return !m_is_unexpected; }
	template<typename T_Self>
	decltype(auto) operator*(this T_Self&& self) noexcept { return *::std::forward_like<T_Self>(self.m_value); }
	template<typename T_Self>
	decltype(auto) value(this T_Self&& self) {
		if (self.m_is_unexpected) throw ::std::bad_expected_access<error_type>{ self.error() };
		return *::std::forward_like<T_Self>(self.m_value);
	}
	template<typename T_Self>
	auto operator->(this T_Self&& self) noexcept
		requires requires { typename value_storage_type::pointer_type; } {
		return ::std::addressof(*::std::forward_like<T_Self&>(self.m_value));
	}
	template<typename T_Self>
	decltype(auto) error(this T_Self&& self) noexcept { return *::std::forward_like<T_Self>(self.m_error); }
	template<typename T_Value_Default, typename T_Self> requires(!::std::is_void_v<value_type>)
	typename value_storage_type::template ref_like_type<T_Self> value_or(this T_Self&& self, T_Value_Default&& value_default)
		noexcept(::std::is_nothrow_convertible_v<T_Value_Default&&, typename value_storage_type::template ref_like_type<T_Self>>)
		requires(::std::is_convertible_v<T_Value_Default&&, typename value_storage_type::template ref_like_type<T_Self>>) {
		if (self.m_is_unexpected) {
			typename value_storage_type::template ref_like_type<T_Self> ref(::std::forward<T_Value_Default>(value_default));
			return ::std::forward_like<T_Self>(ref);
		} else {
			return *::std::forward_like<T_Self>(self.m_value);
		}
	}
	template<typename T_Value_Default, typename T_Self> requires(!::std::is_void_v<value_type>)
	::std::remove_cvref_t<value_type> value_or(this T_Self&& self, T_Value_Default&& value_default)
		noexcept(::std::is_nothrow_convertible_v<T_Value_Default&&, ::std::remove_cvref_t<value_type>>)
		requires(::std::is_convertible_v<T_Value_Default&&, ::std::remove_cvref_t<value_type>> && !::std::is_convertible_v<T_Value_Default&&, typename value_storage_type::template ref_like_type<T_Self>>) {
		if (self.m_is_unexpected) {
			return static_cast<::std::remove_cvref_t<value_type>>(::std::forward<T_Value_Default>(value_default));
		} else {
			return *::std::forward_like<T_Self>(self.m_value);
		}
	}
	template<typename T_Error_Default, typename T_Self> requires(!::std::is_void_v<error_type>)
	typename error_storage_type::template ref_like_type<T_Self> error_or(this T_Self&& self, T_Error_Default&& error_default)
		noexcept(::std::is_nothrow_convertible_v<T_Error_Default&&, typename error_storage_type::template ref_like_type<T_Self>>)
		requires(::std::is_convertible_v<T_Error_Default&&, typename error_storage_type::template ref_like_type<T_Self>>) {
		if (!self.m_is_unexpected) {
			typename error_storage_type::template ref_like_type<T_Self> ref(::std::forward<T_Error_Default>(error_default));
			return ::std::forward_like<T_Self>(ref);
		} else {
			return *::std::forward_like<T_Self>(self.m_error);
		}
	}
	template<typename T_Error_Default, typename T_Self> requires(!::std::is_void_v<error_type>)
	::std::remove_cvref_t<error_type> error_or(this T_Self&& self, T_Error_Default&& error_default)
		noexcept(::std::is_nothrow_convertible_v<T_Error_Default&&, ::std::remove_cvref_t<error_type>>)
		requires(::std::is_convertible_v<T_Error_Default&&, ::std::remove_cvref_t<error_type>> && !::std::is_convertible_v<T_Error_Default&&, typename error_storage_type::template ref_like_type<T_Self>>) {
		if (!self.m_is_unexpected) {
			return static_cast<::std::remove_cvref_t<error_type>>(::std::forward<T_Error_Default>(error_default));
		} else {
			return *::std::forward_like<T_Self>(self.m_error);
		}
	}
	template<typename T_Fn, typename T_Self>
	Expected<invoke_value_result_type<T_Fn, T_Self>, error_type> transform(this T_Self&& self, T_Fn&& fn)
		noexcept(
			nothrow_invocable_from_tuple<T_Fn, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			&& (::std::is_nothrow_move_constructible_v<invoke_value_result_type<T_Fn, T_Self>> || ::std::is_void_v<invoke_value_result_type<T_Fn, T_Self>>)
			&& nothrow_constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			)
		requires(
			invocable_from_tuple<T_Fn, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			&& (::std::is_move_constructible_v<invoke_value_result_type<T_Fn, T_Self>> || ::std::is_void_v<invoke_value_result_type<T_Fn, T_Self>>)
			&& constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			) {
		if (self.m_is_unexpected) {
			return Expected<invoke_value_result_type<T_Fn, T_Self>, error_type>(
				::std::unexpect,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_error).tuple_ref()
				);
		} else {
			if constexpr (::std::is_void_v<invoke_value_result_type<T_Fn, T_Self>>) {
				::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_value).tuple_ref());
				return Expected<invoke_value_result_type<T_Fn, T_Self>, error_type>(::std::in_place);
			} else {
				return Expected<invoke_value_result_type<T_Fn, T_Self>, error_type>(
					::std::in_place,
					::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_value).tuple_ref())
					);
			}
		}
	}
	template<typename T_Result, typename T_Self>
	Expected<T_Result, error_type> transform(this T_Self&& self, T_Result&& result)
		noexcept(
			::std::is_nothrow_move_constructible_v<T_Result>
			&& nothrow_constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			)
		requires(
			!invocable_from_tuple<T_Result&&, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			&& ::std::is_move_constructible_v<T_Result>
			&& constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			) {
		if (self.m_is_unexpected) {
			return Expected<T_Result, error_type>(
				::std::unexpect,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_error).tuple_ref()
				);
		} else {
			return Expected<T_Result, error_type>(
				::std::in_place,
				::std::forward<T_Result>(result)
				);
		}
	}
	template<typename T_Self>
	Expected<void, error_type> transform(this T_Self&& self)
		noexcept(nothrow_constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>)
		requires(constructible_from_tuple<error_type, typename error_storage_type::template tuple_ref_like_type<T_Self>>) {
		if (self.m_is_unexpected) {
			return Expected<void, error_type>(
				::std::unexpect,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_error).tuple_ref()
				);
		} else {
			return Expected<void, error_type>(::std::in_place);
		}
	}
	template<typename T_Fn, typename T_Self>
	Expected<value_type, invoke_error_result_type<T_Fn, T_Self>> transform_error(this T_Self&& self, T_Fn&& fn)
		noexcept(
			nothrow_invocable_from_tuple<T_Fn, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			&& (::std::is_nothrow_move_constructible_v<invoke_error_result_type<T_Fn, T_Self>> || ::std::is_void_v<invoke_error_result_type<T_Fn, T_Self>>)
			&& nothrow_constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			)
		requires(
			invocable_from_tuple<T_Fn, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			&& (::std::is_move_constructible_v<invoke_error_result_type<T_Fn, T_Self>> || ::std::is_void_v<invoke_error_result_type<T_Fn, T_Self>>)
			&& constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			) {
		if (self.m_is_unexpected) {
			if constexpr (::std::is_void_v<invoke_error_result_type<T_Fn, T_Self>>) {
				::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_error).tuple_ref());
				return Expected<value_type, invoke_error_result_type<T_Fn, T_Self>>(::std::unexpect);
			} else {
				return Expected<value_type, invoke_error_result_type<T_Fn, T_Self>>(
					::std::unexpect,
					::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_error).tuple_ref())
					);
			}
		} else {
			return Expected<value_type, invoke_error_result_type<T_Fn, T_Self>>(
				::std::in_place,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_value).tuple_ref()
				);
		}
	}
	template<typename T_Result, typename T_Self>
	Expected<value_type, T_Result> transform_error(this T_Self&& self, T_Result&& result)
		noexcept(
			::std::is_nothrow_move_constructible_v<T_Result>
			&& nothrow_constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			)
		requires(
			!invocable_from_tuple<T_Result&&, typename error_storage_type::template tuple_ref_like_type<T_Self>>
			&& ::std::is_move_constructible_v<T_Result>
			&& constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>
			) {
		if (self.m_is_unexpected) {
			return Expected<value_type, T_Result>(
				::std::unexpect,
				::std::forward<T_Result>(result)
				);
		} else {
			return Expected<value_type, T_Result>(
				::std::in_place,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_value).tuple_ref()
				);
		}
	}
	template<typename T_Self>
	Expected<value_type, void> transform_error(this T_Self&& self)
		noexcept(nothrow_constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>)
		requires(constructible_from_tuple<value_type, typename value_storage_type::template tuple_ref_like_type<T_Self>>) {
		if (self.m_is_unexpected) {
			return Expected<value_type, void>(::std::unexpect);
		} else {
			return Expected<value_type, void>(
				::std::in_place,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_value).tuple_ref()
				);
		}
	}
	template<typename T_Fn, typename T_Self>
	invoke_value_result_type<T_Fn, T_Self> and_then(this T_Self&& self, T_Fn&& fn) {// TODO: noexcept, requires
		if (self.m_is_unexpected) {
			return invoke_value_result_type<T_Fn, T_Self>(
				::std::unexpect,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_error).tuple_ref()
				);
		} else {
			return ::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_value).tuple_ref());
		}
	}
	template<typename T_Fn, typename T_Self>
	invoke_error_result_type<T_Fn, T_Self> or_else(this T_Self&& self, T_Fn&& fn) {// TODO: noexcept, requires
		if (self.m_is_unexpected) {
			return ::std::apply(::std::forward<T_Fn>(fn), ::std::forward_like<T_Self>(self.m_error).tuple_ref());
		} else {
			return invoke_error_result_type<T_Fn, T_Self>(
				::std::in_place,
				::std::piecewise_construct,
				::std::forward_like<T_Self>(self.m_value).tuple_ref()
				);
		}
	}
	template<typename... T_Args>
	constexpr void emplace(T_Args&&... args) noexcept
		requires(::std::is_nothrow_constructible_v<value_storage_type, T_Args&&...>) {
		uninitialize();
		m_is_unexpected = false;
		::std::construct_at(::std::addressof(m_value), ::std::forward<T_Args>(args)...);
	}
	template<typename T_Tuple_Args>
	constexpr void emplace_from_tuple(T_Tuple_Args&& tuple_args) noexcept
		requires(nothrow_constructible_from_tuple<value_storage_type, T_Tuple_Args&&>) {
		uninitialize();
		m_is_unexpected = false;
		construct_from_tuple_at(::std::addressof(m_value), ::std::forward<T_Tuple_Args>(tuple_args));
	}
	template<typename... T_Args>
	constexpr void emplace_error(T_Args&&... args) noexcept
		requires(::std::is_nothrow_constructible_v<error_storage_type, T_Args&&...>) {
		uninitialize();
		m_is_unexpected = true;
		::std::construct_at(::std::addressof(m_error), ::std::forward<T_Args>(args)...);
	}
	template<typename T_Tuple_Args>
	constexpr void emplace_error_from_tuple(T_Tuple_Args&& tuple_args) noexcept
		requires(nothrow_constructible_from_tuple<error_storage_type, T_Tuple_Args&&>) {
		uninitialize();
		m_is_unexpected = true;
		construct_from_tuple_at(::std::addressof(m_error), ::std::forward<T_Tuple_Args>(tuple_args));
	}
private:
	constexpr Expected(details::tag_uninitialized_t, Expected*& ref_ptr) noexcept : m_is_uninitialized{ true } {
		ref_ptr = this;
	}
	constexpr void uninitialize() noexcept {
		if (!m_is_uninitialized) {
			m_is_uninitialized = true;
			if (m_is_unexpected) {
				::std::destroy_at(::std::addressof(m_error));
			} else {
				::std::destroy_at(::std::addressof(m_value));
			}
		}
	}
	template<typename T_Tuple_Args>
	constexpr void emplace_uninitialized_from_tuple(T_Tuple_Args&& tuple_args)
		noexcept(nothrow_constructible_from_tuple<value_storage_type, T_Tuple_Args&&>)
		requires(constructible_from_tuple<value_storage_type, T_Tuple_Args&&>) {
		m_is_unexpected = false;
		construct_from_tuple_at(::std::addressof(m_value), ::std::forward<T_Tuple_Args>(tuple_args));
	}
	template<typename T_Tuple_Args>
	constexpr void emplace_uninitialized_error_from_tuple(T_Tuple_Args&& tuple_args)
		noexcept(nothrow_constructible_from_tuple<error_storage_type, T_Tuple_Args&&>)
		requires(constructible_from_tuple<error_storage_type, T_Tuple_Args&&>) {
		m_is_unexpected = true;
		construct_from_tuple_at(::std::addressof(m_error), ::std::forward<T_Tuple_Args>(tuple_args));
	}
};

template<typename T_Fn_Predicate = ::std::identity, typename T_Fn_Projection = ::std::identity>
struct ExpectsIf final {
	T_Fn_Predicate&& m_fn_predicate;
	T_Fn_Projection&& m_fn_projection;
	explicit constexpr ExpectsIf(T_Fn_Predicate&& fn_predicate = {}, T_Fn_Projection&& fn_projection = {}) noexcept
		: m_fn_predicate(::std::forward<T_Fn_Predicate>(fn_predicate)), m_fn_projection(::std::forward<T_Fn_Projection>(fn_projection)) {}
	template<typename T> requires requires {
		{ ::std::invoke(::std::declval<T_Fn_Predicate&&>(), ::std::invoke(::std::declval<T_Fn_Projection&&>(), ::std::declval<T&&>())) } -> ::std::convertible_to<bool>;
	}
	constexpr Expected<T, T> operator()(T&& value) &&
		noexcept(noexcept(static_cast<bool>(::std::invoke(::std::declval<T_Fn_Predicate&&>(), ::std::invoke(::std::declval<T_Fn_Projection&&>(), ::std::declval<T&&>()))))) {
		bool condition = ::std::invoke(::std::forward<T_Fn_Predicate>(m_fn_predicate), ::std::invoke(::std::forward<T_Fn_Projection>(m_fn_projection), ::std::forward<T>(value)));
		if (condition) {
			return { ::std::forward<T>(value) };
		} else {
			return { ::std::unexpect, ::std::forward<T>(value) };
		}
	}
};
template<typename T_Fn_Predicate = ::std::identity, typename T_Fn_Projection = ::std::identity>
explicit ExpectsIf(T_Fn_Predicate&& fn_predicate = {}, T_Fn_Projection&& fn_projection = {}) noexcept -> ExpectsIf<T_Fn_Predicate, T_Fn_Projection>;
template<typename T_Fn_Predicate = ::std::identity, typename T_Fn_Projection = ::std::identity>
struct UnexpectsIf final {
	T_Fn_Predicate&& m_fn_predicate;
	T_Fn_Projection&& m_fn_projection;
	explicit constexpr UnexpectsIf(T_Fn_Predicate&& fn_predicate = {}, T_Fn_Projection&& fn_projection = {}) noexcept
		: m_fn_predicate(::std::forward<T_Fn_Predicate>(fn_predicate)), m_fn_projection(::std::forward<T_Fn_Projection>(fn_projection)) {}
	template<typename T> requires requires {
		{ ::std::invoke(::std::declval<T_Fn_Predicate&&>(), ::std::invoke(::std::declval<T_Fn_Projection&&>(), ::std::declval<T&&>())) } -> ::std::convertible_to<bool>;
	}
	constexpr Expected<T, T> operator()(T&& value) &&
		noexcept(noexcept(static_cast<bool>(::std::invoke(::std::declval<T_Fn_Predicate&&>(), ::std::invoke(::std::declval<T_Fn_Projection&&>(), ::std::declval<T&&>()))))) {
		bool condition = ::std::invoke(::std::forward<T_Fn_Predicate>(m_fn_predicate), ::std::invoke(::std::forward<T_Fn_Projection>(m_fn_projection), ::std::forward<T>(value)));
		if (condition) {
			return { ::std::unexpect, ::std::forward<T>(value) };
		} else {
			return { ::std::forward<T>(value) };
		}
	}
};
template<typename T_Fn_Predicate = ::std::identity, typename T_Fn_Projection = ::std::identity>
explicit UnexpectsIf(T_Fn_Predicate&& fn_predicate = {}, T_Fn_Projection&& fn_projection = {}) noexcept -> UnexpectsIf<T_Fn_Predicate, T_Fn_Projection>;

template<typename T, typename T_Fn_Predicate, typename T_Fn_Projection> requires(::std::invocable<ExpectsIf<T_Fn_Predicate, T_Fn_Projection>&&, T&&>)
inline Expected<T, T> operator|(T&& value, ExpectsIf<T_Fn_Predicate, T_Fn_Projection>&& op)
	noexcept(::std::is_nothrow_invocable_v<ExpectsIf<T_Fn_Predicate, T_Fn_Projection>&&, T&&>) {
	return ::std::move(op)(::std::forward<T>(value));
}
template<typename T, typename T_Fn_Predicate, typename T_Fn_Projection> requires(::std::invocable<UnexpectsIf<T_Fn_Predicate, T_Fn_Projection>&&, T&&>)
inline Expected<T, T> operator|(T&& value, UnexpectsIf<T_Fn_Predicate, T_Fn_Projection>&& op)
	noexcept(::std::is_nothrow_invocable_v<UnexpectsIf<T_Fn_Predicate, T_Fn_Projection>&&, T&&>) {
	return ::std::move(op)(::std::forward<T>(value));
}

namespace details {
	template<typename T_Value, typename T_Error>
	class CoroutinePromise_Expected {
	public:
		using value_type = T_Value;
		using error_type = T_Error;
		using return_type = Expected<value_type, error_type>;
		constexpr return_type get_return_object() noexcept { return return_type(tag_uninitialized, m_return_object); }
		constexpr ::std::suspend_never initial_suspend() noexcept { return {}; }
		constexpr ::std::suspend_never final_suspend() noexcept { return {}; }
		[[noreturn]] constexpr void unhandled_exception() noexcept(false) {
			::std::rethrow_exception(::std::current_exception());
		}
		constexpr void return_value(const return_type& arg)
			noexcept(::std::is_nothrow_copy_constructible_v<return_type>)
			requires(::std::is_copy_constructible_v<return_type>) {
			*m_return_object = arg;
		}
		constexpr void return_value(return_type&& arg)
			noexcept(::std::is_nothrow_move_constructible_v<return_type>)
			requires(::std::is_move_constructible_v<return_type>) {
			*m_return_object = ::std::move(arg);
		}
		template<typename T_Arg>
		constexpr T_Arg&& await_transform(T_Arg&& arg) noexcept { return ::std::forward<T_Arg>(arg); }
		template<typename T_Value_Other, typename T_Error_Other>
		constexpr auto await_transform(const Expected<T_Value_Other, T_Error_Other>& arg)
			noexcept(nothrow_convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<const int&>, typename return_type::error_storage_type>)
			requires(convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<const int&>, typename return_type::error_storage_type>) {
			struct Awaiter final {
				using ref_type = typename ValueWrapper<T_Value_Other>::template ref_like_type<const int&>;
				::std::optional<ValueWrapper<ref_type>> m_optional;
				constexpr Awaiter() noexcept = default;
				constexpr Awaiter(const ValueWrapper<T_Value_Other>& value) noexcept {
					if constexpr (::std::is_void_v<T_Value_Other>) {
						m_optional.emplace();
					} else {
						m_optional.emplace(*value);
					}
				}
				constexpr bool await_ready() noexcept { return static_cast<bool>(m_optional); }
				constexpr void await_suspend(::std::coroutine_handle<> corohandle) noexcept { corohandle.destroy(); }
				constexpr decltype(auto) await_resume() noexcept {
					return **m_optional;
				}
			};
			if (arg) {
				return Awaiter{ arg.m_value };
			} else {
				m_return_object->emplace_uninitialized_error_from_tuple(arg.m_error.tuple_ref());
				return Awaiter{};
			}
		}
		template<typename T_Value_Other, typename T_Error_Other>
		constexpr auto await_transform(Expected<T_Value_Other, T_Error_Other>&& arg)
			noexcept(nothrow_convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>, typename return_type::error_storage_type>)
			requires(convertible_tuple_v<typename ValueWrapper<T_Error_Other>::template tuple_ref_like_type<int&&>, typename return_type::error_storage_type>) {
			struct Awaiter final {
				using ref_type = typename ValueWrapper<T_Value_Other>::template ref_like_type<int&&>;
				::std::optional<ValueWrapper<ref_type>> m_optional;
				constexpr Awaiter() noexcept = default;
				constexpr Awaiter(ValueWrapper<T_Value_Other>&& value) noexcept {
					if constexpr (::std::is_void_v<T_Value_Other>) {
						m_optional.emplace();
					} else {
						m_optional.emplace(*::std::move(value));
					}
				}
				constexpr bool await_ready() noexcept { return static_cast<bool>(m_optional); }
				constexpr void await_suspend(::std::coroutine_handle<> corohandle) noexcept { corohandle.destroy(); }
				constexpr decltype(auto) await_resume() noexcept {
					return **m_optional;
				}
			};
			if (arg) {
				return Awaiter{ ::std::move(arg.m_value) };
			} else {
				m_return_object->emplace_uninitialized_error_from_tuple(::std::move(arg.m_error).tuple_ref());
				return Awaiter{};
			}
		}
	private:
		return_type* m_return_object{};
	};
}

template<typename T_Value, typename T_Error, typename... T_Args>
struct ::std::coroutine_traits<Expected<T_Value, T_Error>, T_Args...> {
	using promise_type = details::CoroutinePromise_Expected<T_Value, T_Error>;
};
