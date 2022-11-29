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
-- Module      : Amazonka.APIGateway.Types.MethodSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.MethodSetting where

import Amazonka.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the method setting properties.
--
-- /See:/ 'newMethodSetting' smart constructor.
data MethodSetting = MethodSetting'
  { -- | Specifies the throttling rate limit. The PATCH path for this setting is
    -- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
    -- double.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double,
    -- | Specifies the logging level for this method, which affects the log
    -- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
    -- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
    -- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
    -- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
    -- all @ERROR@ events as well as extra informational events.
    loggingLevel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the throttling burst limit. The PATCH path for this setting is
    -- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
    -- integer.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether Amazon CloudWatch metrics are enabled for this method.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
    metricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether authorization is required for a cache invalidation
    -- request. The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
    -- and the value is a Boolean.
    requireAuthorizationForCacheControl :: Prelude.Maybe Prelude.Bool,
    -- | Specifies how to handle unauthorized requests for cache invalidation.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
    -- and the available values are @FAIL_WITH_403@,
    -- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
    unauthorizedCacheControlHeaderStrategy :: Prelude.Maybe UnauthorizedCacheControlHeaderStrategy,
    -- | Specifies the time to live (TTL), in seconds, for cached responses. The
    -- higher the TTL, the longer the response will be cached. The PATCH path
    -- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
    -- the value is an integer.
    cacheTtlInSeconds :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether data trace logging is enabled for this method, which
    -- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
    -- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
    -- the value is a Boolean.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether responses should be cached and returned for requests.
    -- A cache cluster must be enabled on the stage for responses to be cached.
    -- The PATCH path for this setting is
    -- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
    cachingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the cached responses are encrypted. The PATCH path for
    -- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
    -- the value is a Boolean.
    cacheDataEncrypted :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MethodSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throttlingRateLimit', 'methodSetting_throttlingRateLimit' - Specifies the throttling rate limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
-- double.
--
-- 'loggingLevel', 'methodSetting_loggingLevel' - Specifies the logging level for this method, which affects the log
-- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
-- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
-- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
-- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
-- all @ERROR@ events as well as extra informational events.
--
-- 'throttlingBurstLimit', 'methodSetting_throttlingBurstLimit' - Specifies the throttling burst limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
-- integer.
--
-- 'metricsEnabled', 'methodSetting_metricsEnabled' - Specifies whether Amazon CloudWatch metrics are enabled for this method.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
--
-- 'requireAuthorizationForCacheControl', 'methodSetting_requireAuthorizationForCacheControl' - Specifies whether authorization is required for a cache invalidation
-- request. The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
-- and the value is a Boolean.
--
-- 'unauthorizedCacheControlHeaderStrategy', 'methodSetting_unauthorizedCacheControlHeaderStrategy' - Specifies how to handle unauthorized requests for cache invalidation.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
-- and the available values are @FAIL_WITH_403@,
-- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
--
-- 'cacheTtlInSeconds', 'methodSetting_cacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response will be cached. The PATCH path
-- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
-- the value is an integer.
--
-- 'dataTraceEnabled', 'methodSetting_dataTraceEnabled' - Specifies whether data trace logging is enabled for this method, which
-- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
-- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
-- the value is a Boolean.
--
-- 'cachingEnabled', 'methodSetting_cachingEnabled' - Specifies whether responses should be cached and returned for requests.
-- A cache cluster must be enabled on the stage for responses to be cached.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
--
-- 'cacheDataEncrypted', 'methodSetting_cacheDataEncrypted' - Specifies whether the cached responses are encrypted. The PATCH path for
-- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
-- the value is a Boolean.
newMethodSetting ::
  MethodSetting
newMethodSetting =
  MethodSetting'
    { throttlingRateLimit =
        Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      metricsEnabled = Prelude.Nothing,
      requireAuthorizationForCacheControl =
        Prelude.Nothing,
      unauthorizedCacheControlHeaderStrategy =
        Prelude.Nothing,
      cacheTtlInSeconds = Prelude.Nothing,
      dataTraceEnabled = Prelude.Nothing,
      cachingEnabled = Prelude.Nothing,
      cacheDataEncrypted = Prelude.Nothing
    }

-- | Specifies the throttling rate limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/rateLimit@, and the value is a
-- double.
methodSetting_throttlingRateLimit :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Double)
methodSetting_throttlingRateLimit = Lens.lens (\MethodSetting' {throttlingRateLimit} -> throttlingRateLimit) (\s@MethodSetting' {} a -> s {throttlingRateLimit = a} :: MethodSetting)

-- | Specifies the logging level for this method, which affects the log
-- entries pushed to Amazon CloudWatch Logs. The PATCH path for this
-- setting is @\/{method_setting_key}\/logging\/loglevel@, and the
-- available levels are @OFF@, @ERROR@, and @INFO@. Choose @ERROR@ to write
-- only error-level entries to CloudWatch Logs, or choose @INFO@ to include
-- all @ERROR@ events as well as extra informational events.
methodSetting_loggingLevel :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Text)
methodSetting_loggingLevel = Lens.lens (\MethodSetting' {loggingLevel} -> loggingLevel) (\s@MethodSetting' {} a -> s {loggingLevel = a} :: MethodSetting)

-- | Specifies the throttling burst limit. The PATCH path for this setting is
-- @\/{method_setting_key}\/throttling\/burstLimit@, and the value is an
-- integer.
methodSetting_throttlingBurstLimit :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Int)
methodSetting_throttlingBurstLimit = Lens.lens (\MethodSetting' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@MethodSetting' {} a -> s {throttlingBurstLimit = a} :: MethodSetting)

-- | Specifies whether Amazon CloudWatch metrics are enabled for this method.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/metrics\/enabled@, and the value is a Boolean.
methodSetting_metricsEnabled :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Bool)
methodSetting_metricsEnabled = Lens.lens (\MethodSetting' {metricsEnabled} -> metricsEnabled) (\s@MethodSetting' {} a -> s {metricsEnabled = a} :: MethodSetting)

-- | Specifies whether authorization is required for a cache invalidation
-- request. The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/requireAuthorizationForCacheControl@,
-- and the value is a Boolean.
methodSetting_requireAuthorizationForCacheControl :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Bool)
methodSetting_requireAuthorizationForCacheControl = Lens.lens (\MethodSetting' {requireAuthorizationForCacheControl} -> requireAuthorizationForCacheControl) (\s@MethodSetting' {} a -> s {requireAuthorizationForCacheControl = a} :: MethodSetting)

-- | Specifies how to handle unauthorized requests for cache invalidation.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/unauthorizedCacheControlHeaderStrategy@,
-- and the available values are @FAIL_WITH_403@,
-- @SUCCEED_WITH_RESPONSE_HEADER@, @SUCCEED_WITHOUT_RESPONSE_HEADER@.
methodSetting_unauthorizedCacheControlHeaderStrategy :: Lens.Lens' MethodSetting (Prelude.Maybe UnauthorizedCacheControlHeaderStrategy)
methodSetting_unauthorizedCacheControlHeaderStrategy = Lens.lens (\MethodSetting' {unauthorizedCacheControlHeaderStrategy} -> unauthorizedCacheControlHeaderStrategy) (\s@MethodSetting' {} a -> s {unauthorizedCacheControlHeaderStrategy = a} :: MethodSetting)

-- | Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response will be cached. The PATCH path
-- for this setting is @\/{method_setting_key}\/caching\/ttlInSeconds@, and
-- the value is an integer.
methodSetting_cacheTtlInSeconds :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Int)
methodSetting_cacheTtlInSeconds = Lens.lens (\MethodSetting' {cacheTtlInSeconds} -> cacheTtlInSeconds) (\s@MethodSetting' {} a -> s {cacheTtlInSeconds = a} :: MethodSetting)

-- | Specifies whether data trace logging is enabled for this method, which
-- affects the log entries pushed to Amazon CloudWatch Logs. The PATCH path
-- for this setting is @\/{method_setting_key}\/logging\/dataTrace@, and
-- the value is a Boolean.
methodSetting_dataTraceEnabled :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Bool)
methodSetting_dataTraceEnabled = Lens.lens (\MethodSetting' {dataTraceEnabled} -> dataTraceEnabled) (\s@MethodSetting' {} a -> s {dataTraceEnabled = a} :: MethodSetting)

-- | Specifies whether responses should be cached and returned for requests.
-- A cache cluster must be enabled on the stage for responses to be cached.
-- The PATCH path for this setting is
-- @\/{method_setting_key}\/caching\/enabled@, and the value is a Boolean.
methodSetting_cachingEnabled :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Bool)
methodSetting_cachingEnabled = Lens.lens (\MethodSetting' {cachingEnabled} -> cachingEnabled) (\s@MethodSetting' {} a -> s {cachingEnabled = a} :: MethodSetting)

-- | Specifies whether the cached responses are encrypted. The PATCH path for
-- this setting is @\/{method_setting_key}\/caching\/dataEncrypted@, and
-- the value is a Boolean.
methodSetting_cacheDataEncrypted :: Lens.Lens' MethodSetting (Prelude.Maybe Prelude.Bool)
methodSetting_cacheDataEncrypted = Lens.lens (\MethodSetting' {cacheDataEncrypted} -> cacheDataEncrypted) (\s@MethodSetting' {} a -> s {cacheDataEncrypted = a} :: MethodSetting)

instance Core.FromJSON MethodSetting where
  parseJSON =
    Core.withObject
      "MethodSetting"
      ( \x ->
          MethodSetting'
            Prelude.<$> (x Core..:? "throttlingRateLimit")
            Prelude.<*> (x Core..:? "loggingLevel")
            Prelude.<*> (x Core..:? "throttlingBurstLimit")
            Prelude.<*> (x Core..:? "metricsEnabled")
            Prelude.<*> (x Core..:? "requireAuthorizationForCacheControl")
            Prelude.<*> (x Core..:? "unauthorizedCacheControlHeaderStrategy")
            Prelude.<*> (x Core..:? "cacheTtlInSeconds")
            Prelude.<*> (x Core..:? "dataTraceEnabled")
            Prelude.<*> (x Core..:? "cachingEnabled")
            Prelude.<*> (x Core..:? "cacheDataEncrypted")
      )

instance Prelude.Hashable MethodSetting where
  hashWithSalt _salt MethodSetting' {..} =
    _salt `Prelude.hashWithSalt` throttlingRateLimit
      `Prelude.hashWithSalt` loggingLevel
      `Prelude.hashWithSalt` throttlingBurstLimit
      `Prelude.hashWithSalt` metricsEnabled
      `Prelude.hashWithSalt` requireAuthorizationForCacheControl
      `Prelude.hashWithSalt` unauthorizedCacheControlHeaderStrategy
      `Prelude.hashWithSalt` cacheTtlInSeconds
      `Prelude.hashWithSalt` dataTraceEnabled
      `Prelude.hashWithSalt` cachingEnabled
      `Prelude.hashWithSalt` cacheDataEncrypted

instance Prelude.NFData MethodSetting where
  rnf MethodSetting' {..} =
    Prelude.rnf throttlingRateLimit
      `Prelude.seq` Prelude.rnf loggingLevel
      `Prelude.seq` Prelude.rnf throttlingBurstLimit
      `Prelude.seq` Prelude.rnf metricsEnabled
      `Prelude.seq` Prelude.rnf requireAuthorizationForCacheControl
      `Prelude.seq` Prelude.rnf unauthorizedCacheControlHeaderStrategy
      `Prelude.seq` Prelude.rnf cacheTtlInSeconds
      `Prelude.seq` Prelude.rnf dataTraceEnabled
      `Prelude.seq` Prelude.rnf cachingEnabled
      `Prelude.seq` Prelude.rnf cacheDataEncrypted
