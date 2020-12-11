-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MethodSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodSetting
  ( MethodSetting (..),

    -- * Smart constructor
    mkMethodSetting,

    -- * Lenses
    msCacheTtlInSeconds,
    msDataTraceEnabled,
    msThrottlingBurstLimit,
    msCacheDataEncrypted,
    msLoggingLevel,
    msRequireAuthorizationForCacheControl,
    msCachingEnabled,
    msMetricsEnabled,
    msThrottlingRateLimit,
    msUnauthorizedCacheControlHeaderStrategy,
  )
where

import Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the method setting properties.
--
-- /See:/ 'mkMethodSetting' smart constructor.
data MethodSetting = MethodSetting'
  { cacheTtlInSeconds ::
      Lude.Maybe Lude.Int,
    dataTraceEnabled :: Lude.Maybe Lude.Bool,
    throttlingBurstLimit :: Lude.Maybe Lude.Int,
    cacheDataEncrypted :: Lude.Maybe Lude.Bool,
    loggingLevel :: Lude.Maybe Lude.Text,
    requireAuthorizationForCacheControl :: Lude.Maybe Lude.Bool,
    cachingEnabled :: Lude.Maybe Lude.Bool,
    metricsEnabled :: Lude.Maybe Lude.Bool,
    throttlingRateLimit :: Lude.Maybe Lude.Double,
    unauthorizedCacheControlHeaderStrategy ::
      Lude.Maybe UnauthorizedCacheControlHeaderStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MethodSetting' with the minimum fields required to make a request.
--
-- * 'cacheDataEncrypted' - Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
-- * 'cacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
-- * 'cachingEnabled' - Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
-- * 'dataTraceEnabled' - Specifies whether data trace logging is enabled for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
-- * 'loggingLevel' - Specifies the logging level for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ . Choose @ERROR@ to write only error-level entries to CloudWatch Logs, or choose @INFO@ to include all @ERROR@ events as well as extra informational events.
-- * 'metricsEnabled' - Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
-- * 'requireAuthorizationForCacheControl' - Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
-- * 'throttlingBurstLimit' - Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
-- * 'throttlingRateLimit' - Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
-- * 'unauthorizedCacheControlHeaderStrategy' - Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
mkMethodSetting ::
  MethodSetting
mkMethodSetting =
  MethodSetting'
    { cacheTtlInSeconds = Lude.Nothing,
      dataTraceEnabled = Lude.Nothing,
      throttlingBurstLimit = Lude.Nothing,
      cacheDataEncrypted = Lude.Nothing,
      loggingLevel = Lude.Nothing,
      requireAuthorizationForCacheControl = Lude.Nothing,
      cachingEnabled = Lude.Nothing,
      metricsEnabled = Lude.Nothing,
      throttlingRateLimit = Lude.Nothing,
      unauthorizedCacheControlHeaderStrategy = Lude.Nothing
    }

-- | Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
--
-- /Note:/ Consider using 'cacheTtlInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCacheTtlInSeconds :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Int)
msCacheTtlInSeconds = Lens.lens (cacheTtlInSeconds :: MethodSetting -> Lude.Maybe Lude.Int) (\s a -> s {cacheTtlInSeconds = a} :: MethodSetting)
{-# DEPRECATED msCacheTtlInSeconds "Use generic-lens or generic-optics with 'cacheTtlInSeconds' instead." #-}

-- | Specifies whether data trace logging is enabled for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'dataTraceEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msDataTraceEnabled :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Bool)
msDataTraceEnabled = Lens.lens (dataTraceEnabled :: MethodSetting -> Lude.Maybe Lude.Bool) (\s a -> s {dataTraceEnabled = a} :: MethodSetting)
{-# DEPRECATED msDataTraceEnabled "Use generic-lens or generic-optics with 'dataTraceEnabled' instead." #-}

-- | Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
--
-- /Note:/ Consider using 'throttlingBurstLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msThrottlingBurstLimit :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Int)
msThrottlingBurstLimit = Lens.lens (throttlingBurstLimit :: MethodSetting -> Lude.Maybe Lude.Int) (\s a -> s {throttlingBurstLimit = a} :: MethodSetting)
{-# DEPRECATED msThrottlingBurstLimit "Use generic-lens or generic-optics with 'throttlingBurstLimit' instead." #-}

-- | Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'cacheDataEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCacheDataEncrypted :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Bool)
msCacheDataEncrypted = Lens.lens (cacheDataEncrypted :: MethodSetting -> Lude.Maybe Lude.Bool) (\s a -> s {cacheDataEncrypted = a} :: MethodSetting)
{-# DEPRECATED msCacheDataEncrypted "Use generic-lens or generic-optics with 'cacheDataEncrypted' instead." #-}

-- | Specifies the logging level for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ . Choose @ERROR@ to write only error-level entries to CloudWatch Logs, or choose @INFO@ to include all @ERROR@ events as well as extra informational events.
--
-- /Note:/ Consider using 'loggingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msLoggingLevel :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Text)
msLoggingLevel = Lens.lens (loggingLevel :: MethodSetting -> Lude.Maybe Lude.Text) (\s a -> s {loggingLevel = a} :: MethodSetting)
{-# DEPRECATED msLoggingLevel "Use generic-lens or generic-optics with 'loggingLevel' instead." #-}

-- | Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'requireAuthorizationForCacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRequireAuthorizationForCacheControl :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Bool)
msRequireAuthorizationForCacheControl = Lens.lens (requireAuthorizationForCacheControl :: MethodSetting -> Lude.Maybe Lude.Bool) (\s a -> s {requireAuthorizationForCacheControl = a} :: MethodSetting)
{-# DEPRECATED msRequireAuthorizationForCacheControl "Use generic-lens or generic-optics with 'requireAuthorizationForCacheControl' instead." #-}

-- | Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'cachingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCachingEnabled :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Bool)
msCachingEnabled = Lens.lens (cachingEnabled :: MethodSetting -> Lude.Maybe Lude.Bool) (\s a -> s {cachingEnabled = a} :: MethodSetting)
{-# DEPRECATED msCachingEnabled "Use generic-lens or generic-optics with 'cachingEnabled' instead." #-}

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'metricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMetricsEnabled :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Bool)
msMetricsEnabled = Lens.lens (metricsEnabled :: MethodSetting -> Lude.Maybe Lude.Bool) (\s a -> s {metricsEnabled = a} :: MethodSetting)
{-# DEPRECATED msMetricsEnabled "Use generic-lens or generic-optics with 'metricsEnabled' instead." #-}

-- | Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
--
-- /Note:/ Consider using 'throttlingRateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msThrottlingRateLimit :: Lens.Lens' MethodSetting (Lude.Maybe Lude.Double)
msThrottlingRateLimit = Lens.lens (throttlingRateLimit :: MethodSetting -> Lude.Maybe Lude.Double) (\s a -> s {throttlingRateLimit = a} :: MethodSetting)
{-# DEPRECATED msThrottlingRateLimit "Use generic-lens or generic-optics with 'throttlingRateLimit' instead." #-}

-- | Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
--
-- /Note:/ Consider using 'unauthorizedCacheControlHeaderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msUnauthorizedCacheControlHeaderStrategy :: Lens.Lens' MethodSetting (Lude.Maybe UnauthorizedCacheControlHeaderStrategy)
msUnauthorizedCacheControlHeaderStrategy = Lens.lens (unauthorizedCacheControlHeaderStrategy :: MethodSetting -> Lude.Maybe UnauthorizedCacheControlHeaderStrategy) (\s a -> s {unauthorizedCacheControlHeaderStrategy = a} :: MethodSetting)
{-# DEPRECATED msUnauthorizedCacheControlHeaderStrategy "Use generic-lens or generic-optics with 'unauthorizedCacheControlHeaderStrategy' instead." #-}

instance Lude.FromJSON MethodSetting where
  parseJSON =
    Lude.withObject
      "MethodSetting"
      ( \x ->
          MethodSetting'
            Lude.<$> (x Lude..:? "cacheTtlInSeconds")
            Lude.<*> (x Lude..:? "dataTraceEnabled")
            Lude.<*> (x Lude..:? "throttlingBurstLimit")
            Lude.<*> (x Lude..:? "cacheDataEncrypted")
            Lude.<*> (x Lude..:? "loggingLevel")
            Lude.<*> (x Lude..:? "requireAuthorizationForCacheControl")
            Lude.<*> (x Lude..:? "cachingEnabled")
            Lude.<*> (x Lude..:? "metricsEnabled")
            Lude.<*> (x Lude..:? "throttlingRateLimit")
            Lude.<*> (x Lude..:? "unauthorizedCacheControlHeaderStrategy")
      )
