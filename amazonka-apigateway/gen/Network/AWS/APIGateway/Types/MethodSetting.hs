{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MethodSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodSetting where

import Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the method setting properties.
--
-- /See:/ 'newMethodSetting' smart constructor.
data MethodSetting = MethodSetting'
  { -- | Specifies whether data trace logging is enabled for this method, which
    -- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
    -- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
    -- the value is a Boolean.
    dataTraceEnabled :: Core.Maybe Core.Bool,
    -- | Specifies whether authorization is required for a cache invalidation
    -- request. The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
    -- and the value is a Boolean.
    requireAuthorizationForCacheControl :: Core.Maybe Core.Bool,
    -- | Specifies whether the cached responses are encrypted. The PATCH path for
    -- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
    -- the value is a Boolean.
    cacheDataEncrypted :: Core.Maybe Core.Bool,
    -- | Specifies the throttling rate limit. The PATCH path for this setting is
    -- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
    -- double.
    throttlingRateLimit :: Core.Maybe Core.Double,
    -- | Specifies the throttling burst limit. The PATCH path for this setting is
    -- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
    -- integer.
    throttlingBurstLimit :: Core.Maybe Core.Int,
    -- | Specifies the time to live (TTL), in seconds, for cached responses. The
    -- higher the TTL, the longer the response will be cached. The PATCH path
    -- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
    -- the value is an integer.
    cacheTtlInSeconds :: Core.Maybe Core.Int,
    -- | Specifies whether responses should be cached and returned for requests.
    -- A cache cluster must be enabled on the stage for responses to be cached.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
    cachingEnabled :: Core.Maybe Core.Bool,
    -- | Specifies how to handle unauthorized requests for cache invalidation.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
    -- and the available values are @FAIL_WITH_403@,
    -- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
    unauthorizedCacheControlHeaderStrategy :: Core.Maybe UnauthorizedCacheControlHeaderStrategy,
    -- | Specifies the logging level for this method, which affects the log
    -- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
    -- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
    -- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
    -- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
    -- all @ERROR@ events as well as extra informational events.
    loggingLevel :: Core.Maybe Core.Text,
    -- | Specifies whether Amazon CloudWatch metrics are enabled for this method.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
    metricsEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MethodSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTraceEnabled', 'methodSetting_dataTraceEnabled' - Specifies whether data trace logging is enabled for this method, which
-- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
-- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
-- the value is a Boolean.
--
-- 'requireAuthorizationForCacheControl', 'methodSetting_requireAuthorizationForCacheControl' - Specifies whether authorization is required for a cache invalidation
-- request. The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
-- and the value is a Boolean.
--
-- 'cacheDataEncrypted', 'methodSetting_cacheDataEncrypted' - Specifies whether the cached responses are encrypted. The PATCH path for
-- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
-- the value is a Boolean.
--
-- 'throttlingRateLimit', 'methodSetting_throttlingRateLimit' - Specifies the throttling rate limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
-- double.
--
-- 'throttlingBurstLimit', 'methodSetting_throttlingBurstLimit' - Specifies the throttling burst limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
-- integer.
--
-- 'cacheTtlInSeconds', 'methodSetting_cacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response will be cached. The PATCH path
-- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
-- the value is an integer.
--
-- 'cachingEnabled', 'methodSetting_cachingEnabled' - Specifies whether responses should be cached and returned for requests.
-- A cache cluster must be enabled on the stage for responses to be cached.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
--
-- 'unauthorizedCacheControlHeaderStrategy', 'methodSetting_unauthorizedCacheControlHeaderStrategy' - Specifies how to handle unauthorized requests for cache invalidation.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
-- and the available values are @FAIL_WITH_403@,
-- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
--
-- 'loggingLevel', 'methodSetting_loggingLevel' - Specifies the logging level for this method, which affects the log
-- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
-- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
-- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
-- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
-- all @ERROR@ events as well as extra informational events.
--
-- 'metricsEnabled', 'methodSetting_metricsEnabled' - Specifies whether Amazon CloudWatch metrics are enabled for this method.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
newMethodSetting ::
  MethodSetting
newMethodSetting =
  MethodSetting'
    { dataTraceEnabled = Core.Nothing,
      requireAuthorizationForCacheControl = Core.Nothing,
      cacheDataEncrypted = Core.Nothing,
      throttlingRateLimit = Core.Nothing,
      throttlingBurstLimit = Core.Nothing,
      cacheTtlInSeconds = Core.Nothing,
      cachingEnabled = Core.Nothing,
      unauthorizedCacheControlHeaderStrategy =
        Core.Nothing,
      loggingLevel = Core.Nothing,
      metricsEnabled = Core.Nothing
    }

-- | Specifies whether data trace logging is enabled for this method, which
-- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
-- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
-- the value is a Boolean.
methodSetting_dataTraceEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
methodSetting_dataTraceEnabled = Lens.lens (\MethodSetting' {dataTraceEnabled} -> dataTraceEnabled) (\s@MethodSetting' {} a -> s {dataTraceEnabled = a} :: MethodSetting)

-- | Specifies whether authorization is required for a cache invalidation
-- request. The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
-- and the value is a Boolean.
methodSetting_requireAuthorizationForCacheControl :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
methodSetting_requireAuthorizationForCacheControl = Lens.lens (\MethodSetting' {requireAuthorizationForCacheControl} -> requireAuthorizationForCacheControl) (\s@MethodSetting' {} a -> s {requireAuthorizationForCacheControl = a} :: MethodSetting)

-- | Specifies whether the cached responses are encrypted. The PATCH path for
-- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
-- the value is a Boolean.
methodSetting_cacheDataEncrypted :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
methodSetting_cacheDataEncrypted = Lens.lens (\MethodSetting' {cacheDataEncrypted} -> cacheDataEncrypted) (\s@MethodSetting' {} a -> s {cacheDataEncrypted = a} :: MethodSetting)

-- | Specifies the throttling rate limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
-- double.
methodSetting_throttlingRateLimit :: Lens.Lens' MethodSetting (Core.Maybe Core.Double)
methodSetting_throttlingRateLimit = Lens.lens (\MethodSetting' {throttlingRateLimit} -> throttlingRateLimit) (\s@MethodSetting' {} a -> s {throttlingRateLimit = a} :: MethodSetting)

-- | Specifies the throttling burst limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
-- integer.
methodSetting_throttlingBurstLimit :: Lens.Lens' MethodSetting (Core.Maybe Core.Int)
methodSetting_throttlingBurstLimit = Lens.lens (\MethodSetting' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@MethodSetting' {} a -> s {throttlingBurstLimit = a} :: MethodSetting)

-- | Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response will be cached. The PATCH path
-- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
-- the value is an integer.
methodSetting_cacheTtlInSeconds :: Lens.Lens' MethodSetting (Core.Maybe Core.Int)
methodSetting_cacheTtlInSeconds = Lens.lens (\MethodSetting' {cacheTtlInSeconds} -> cacheTtlInSeconds) (\s@MethodSetting' {} a -> s {cacheTtlInSeconds = a} :: MethodSetting)

-- | Specifies whether responses should be cached and returned for requests.
-- A cache cluster must be enabled on the stage for responses to be cached.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
methodSetting_cachingEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
methodSetting_cachingEnabled = Lens.lens (\MethodSetting' {cachingEnabled} -> cachingEnabled) (\s@MethodSetting' {} a -> s {cachingEnabled = a} :: MethodSetting)

-- | Specifies how to handle unauthorized requests for cache invalidation.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
-- and the available values are @FAIL_WITH_403@,
-- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
methodSetting_unauthorizedCacheControlHeaderStrategy :: Lens.Lens' MethodSetting (Core.Maybe UnauthorizedCacheControlHeaderStrategy)
methodSetting_unauthorizedCacheControlHeaderStrategy = Lens.lens (\MethodSetting' {unauthorizedCacheControlHeaderStrategy} -> unauthorizedCacheControlHeaderStrategy) (\s@MethodSetting' {} a -> s {unauthorizedCacheControlHeaderStrategy = a} :: MethodSetting)

-- | Specifies the logging level for this method, which affects the log
-- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
-- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
-- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
-- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
-- all @ERROR@ events as well as extra informational events.
methodSetting_loggingLevel :: Lens.Lens' MethodSetting (Core.Maybe Core.Text)
methodSetting_loggingLevel = Lens.lens (\MethodSetting' {loggingLevel} -> loggingLevel) (\s@MethodSetting' {} a -> s {loggingLevel = a} :: MethodSetting)

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
methodSetting_metricsEnabled :: Lens.Lens' MethodSetting (Core.Maybe Core.Bool)
methodSetting_metricsEnabled = Lens.lens (\MethodSetting' {metricsEnabled} -> metricsEnabled) (\s@MethodSetting' {} a -> s {metricsEnabled = a} :: MethodSetting)

instance Core.FromJSON MethodSetting where
  parseJSON =
    Core.withObject
      "MethodSetting"
      ( \x ->
          MethodSetting'
            Core.<$> (x Core..:? "dataTraceEnabled")
            Core.<*> (x Core..:? "requireAuthorizationForCacheControl")
            Core.<*> (x Core..:? "cacheDataEncrypted")
            Core.<*> (x Core..:? "throttlingRateLimit")
            Core.<*> (x Core..:? "throttlingBurstLimit")
            Core.<*> (x Core..:? "cacheTtlInSeconds")
            Core.<*> (x Core..:? "cachingEnabled")
            Core.<*> (x Core..:? "unauthorizedCacheControlHeaderStrategy")
            Core.<*> (x Core..:? "loggingLevel")
            Core.<*> (x Core..:? "metricsEnabled")
      )

instance Core.Hashable MethodSetting

instance Core.NFData MethodSetting
