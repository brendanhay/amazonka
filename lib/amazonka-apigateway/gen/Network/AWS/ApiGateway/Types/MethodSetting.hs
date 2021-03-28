{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.MethodSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.MethodSetting
  ( MethodSetting (..)
  -- * Smart constructor
  , mkMethodSetting
  -- * Lenses
  , msCacheDataEncrypted
  , msCacheTtlInSeconds
  , msCachingEnabled
  , msDataTraceEnabled
  , msLoggingLevel
  , msMetricsEnabled
  , msRequireAuthorizationForCacheControl
  , msThrottlingBurstLimit
  , msThrottlingRateLimit
  , msUnauthorizedCacheControlHeaderStrategy
  ) where

import qualified Network.AWS.ApiGateway.Types.UnauthorizedCacheControlHeaderStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the method setting properties.
--
-- /See:/ 'mkMethodSetting' smart constructor.
data MethodSetting = MethodSetting'
  { cacheDataEncrypted :: Core.Maybe Core.Bool
    -- ^ Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
  , cacheTtlInSeconds :: Core.Maybe Core.Int
    -- ^ Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
  , cachingEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
  , dataTraceEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether data trace logging is enabled for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
  , loggingLevel :: Core.Maybe Core.Text
    -- ^ Specifies the logging level for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ . Choose @ERROR@ to write only error-level entries to CloudWatch Logs, or choose @INFO@ to include all @ERROR@ events as well as extra informational events.
  , metricsEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
  , requireAuthorizationForCacheControl :: Core.Maybe Core.Bool
    -- ^ Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
  , throttlingBurstLimit :: Core.Maybe Core.Int
    -- ^ Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
  , throttlingRateLimit :: Core.Maybe Core.Double
    -- ^ Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
  , unauthorizedCacheControlHeaderStrategy :: Core.Maybe Types.UnauthorizedCacheControlHeaderStrategy
    -- ^ Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MethodSetting' value with any optional fields omitted.
mkMethodSetting
    :: MethodSetting
mkMethodSetting
  = MethodSetting'{cacheDataEncrypted = Core.Nothing,
                   cacheTtlInSeconds = Core.Nothing, cachingEnabled = Core.Nothing,
                   dataTraceEnabled = Core.Nothing, loggingLevel = Core.Nothing,
                   metricsEnabled = Core.Nothing,
                   requireAuthorizationForCacheControl = Core.Nothing,
                   throttlingBurstLimit = Core.Nothing,
                   throttlingRateLimit = Core.Nothing,
                   unauthorizedCacheControlHeaderStrategy = Core.Nothing}

-- | Specifies whether the cached responses are encrypted. The PATCH path for this setting is @/{method_setting_key}/caching/dataEncrypted@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'cacheDataEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCacheDataEncrypted :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
msCacheDataEncrypted = Lens.field @"cacheDataEncrypted"
{-# INLINEABLE msCacheDataEncrypted #-}
{-# DEPRECATED cacheDataEncrypted "Use generic-lens or generic-optics with 'cacheDataEncrypted' instead"  #-}

-- | Specifies the time to live (TTL), in seconds, for cached responses. The higher the TTL, the longer the response will be cached. The PATCH path for this setting is @/{method_setting_key}/caching/ttlInSeconds@ , and the value is an integer.
--
-- /Note:/ Consider using 'cacheTtlInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCacheTtlInSeconds :: Lens.Lens' MethodSetting (Core.Maybe Core.Int)
msCacheTtlInSeconds = Lens.field @"cacheTtlInSeconds"
{-# INLINEABLE msCacheTtlInSeconds #-}
{-# DEPRECATED cacheTtlInSeconds "Use generic-lens or generic-optics with 'cacheTtlInSeconds' instead"  #-}

-- | Specifies whether responses should be cached and returned for requests. A cache cluster must be enabled on the stage for responses to be cached. The PATCH path for this setting is @/{method_setting_key}/caching/enabled@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'cachingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCachingEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
msCachingEnabled = Lens.field @"cachingEnabled"
{-# INLINEABLE msCachingEnabled #-}
{-# DEPRECATED cachingEnabled "Use generic-lens or generic-optics with 'cachingEnabled' instead"  #-}

-- | Specifies whether data trace logging is enabled for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/dataTrace@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'dataTraceEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msDataTraceEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
msDataTraceEnabled = Lens.field @"dataTraceEnabled"
{-# INLINEABLE msDataTraceEnabled #-}
{-# DEPRECATED dataTraceEnabled "Use generic-lens or generic-optics with 'dataTraceEnabled' instead"  #-}

-- | Specifies the logging level for this method, which affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path for this setting is @/{method_setting_key}/logging/loglevel@ , and the available levels are @OFF@ , @ERROR@ , and @INFO@ . Choose @ERROR@ to write only error-level entries to CloudWatch Logs, or choose @INFO@ to include all @ERROR@ events as well as extra informational events.
--
-- /Note:/ Consider using 'loggingLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msLoggingLevel :: Lens.Lens' MethodSetting (Core.Maybe Core.Text)
msLoggingLevel = Lens.field @"loggingLevel"
{-# INLINEABLE msLoggingLevel #-}
{-# DEPRECATED loggingLevel "Use generic-lens or generic-optics with 'loggingLevel' instead"  #-}

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method. The PATCH path for this setting is @/{method_setting_key}/metrics/enabled@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'metricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMetricsEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
msMetricsEnabled = Lens.field @"metricsEnabled"
{-# INLINEABLE msMetricsEnabled #-}
{-# DEPRECATED metricsEnabled "Use generic-lens or generic-optics with 'metricsEnabled' instead"  #-}

-- | Specifies whether authorization is required for a cache invalidation request. The PATCH path for this setting is @/{method_setting_key}/caching/requireAuthorizationForCacheControl@ , and the value is a Boolean.
--
-- /Note:/ Consider using 'requireAuthorizationForCacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRequireAuthorizationForCacheControl :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
msRequireAuthorizationForCacheControl = Lens.field @"requireAuthorizationForCacheControl"
{-# INLINEABLE msRequireAuthorizationForCacheControl #-}
{-# DEPRECATED requireAuthorizationForCacheControl "Use generic-lens or generic-optics with 'requireAuthorizationForCacheControl' instead"  #-}

-- | Specifies the throttling burst limit. The PATCH path for this setting is @/{method_setting_key}/throttling/burstLimit@ , and the value is an integer.
--
-- /Note:/ Consider using 'throttlingBurstLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msThrottlingBurstLimit :: Lens.Lens' MethodSetting (Core.Maybe Core.Int)
msThrottlingBurstLimit = Lens.field @"throttlingBurstLimit"
{-# INLINEABLE msThrottlingBurstLimit #-}
{-# DEPRECATED throttlingBurstLimit "Use generic-lens or generic-optics with 'throttlingBurstLimit' instead"  #-}

-- | Specifies the throttling rate limit. The PATCH path for this setting is @/{method_setting_key}/throttling/rateLimit@ , and the value is a double.
--
-- /Note:/ Consider using 'throttlingRateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msThrottlingRateLimit :: Lens.Lens' MethodSetting (Core.Maybe Core.Double)
msThrottlingRateLimit = Lens.field @"throttlingRateLimit"
{-# INLINEABLE msThrottlingRateLimit #-}
{-# DEPRECATED throttlingRateLimit "Use generic-lens or generic-optics with 'throttlingRateLimit' instead"  #-}

-- | Specifies how to handle unauthorized requests for cache invalidation. The PATCH path for this setting is @/{method_setting_key}/caching/unauthorizedCacheControlHeaderStrategy@ , and the available values are @FAIL_WITH_403@ , @SUCCEED_WITH_RESPONSE_HEADER@ , @SUCCEED_WITHOUT_RESPONSE_HEADER@ .
--
-- /Note:/ Consider using 'unauthorizedCacheControlHeaderStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msUnauthorizedCacheControlHeaderStrategy :: Lens.Lens' MethodSetting (Core.Maybe Types.UnauthorizedCacheControlHeaderStrategy)
msUnauthorizedCacheControlHeaderStrategy = Lens.field @"unauthorizedCacheControlHeaderStrategy"
{-# INLINEABLE msUnauthorizedCacheControlHeaderStrategy #-}
{-# DEPRECATED unauthorizedCacheControlHeaderStrategy "Use generic-lens or generic-optics with 'unauthorizedCacheControlHeaderStrategy' instead"  #-}

instance Core.FromJSON MethodSetting where
        parseJSON
          = Core.withObject "MethodSetting" Core.$
              \ x ->
                MethodSetting' Core.<$>
                  (x Core..:? "cacheDataEncrypted") Core.<*>
                    x Core..:? "cacheTtlInSeconds"
                    Core.<*> x Core..:? "cachingEnabled"
                    Core.<*> x Core..:? "dataTraceEnabled"
                    Core.<*> x Core..:? "loggingLevel"
                    Core.<*> x Core..:? "metricsEnabled"
                    Core.<*> x Core..:? "requireAuthorizationForCacheControl"
                    Core.<*> x Core..:? "throttlingBurstLimit"
                    Core.<*> x Core..:? "throttlingRateLimit"
                    Core.<*> x Core..:? "unauthorizedCacheControlHeaderStrategy"
