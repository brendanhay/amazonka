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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines settings for a method for the stage.
--
-- /See:/ 'newAwsApiGatewayMethodSettings' smart constructor.
data AwsApiGatewayMethodSettings = AwsApiGatewayMethodSettings'
  { -- | The throttling rate limit for the method.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double,
    -- | The logging level for this method. The logging level affects the log
    -- entries that are pushed to CloudWatch Logs.
    --
    -- If the logging level is @ERROR@, then the logs only include error-level
    -- entries.
    --
    -- If the logging level is @INFO@, then the logs include both @ERROR@
    -- events and extra informational events.
    --
    -- Valid values: @OFF@ | @ERROR@ | @INFO@
    loggingLevel :: Prelude.Maybe Prelude.Text,
    -- | The throttling burst limit for the method.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | The resource path for this method. Forward slashes (\/) are encoded as
    -- ~1 . The initial slash must include a forward slash.
    --
    -- For example, the path value @\/resource\/subresource@ must be encoded as
    -- @\/~1resource~1subresource@.
    --
    -- To specify the root path, use only a slash (\/). You can use an asterisk
    -- (*) as a wildcard to apply method settings to multiple methods.
    resourcePath :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether CloudWatch metrics are enabled for the method.
    metricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether authorization is required for a cache invalidation
    -- request.
    requireAuthorizationForCacheControl :: Prelude.Maybe Prelude.Bool,
    -- | Indicates how to handle unauthorized requests for cache invalidation.
    --
    -- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
    -- @SUCCEED_WITHOUT_RESPONSE_HEADER@
    unauthorizedCacheControlHeaderStrategy :: Prelude.Maybe Prelude.Text,
    -- | The HTTP method. You can use an asterisk (*) as a wildcard to apply
    -- method settings to multiple methods.
    httpMethod :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time to live (TTL), in seconds, for cached responses. The
    -- higher the TTL, the longer the response is cached.
    cacheTtlInSeconds :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether data trace logging is enabled for the method. Data
    -- trace logging affects the log entries that are pushed to CloudWatch
    -- Logs.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether responses are cached and returned for requests. For
    -- responses to be cached, a cache cluster must be enabled on the stage.
    cachingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the cached responses are encrypted.
    cacheDataEncrypted :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayMethodSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'throttlingRateLimit', 'awsApiGatewayMethodSettings_throttlingRateLimit' - The throttling rate limit for the method.
--
-- 'loggingLevel', 'awsApiGatewayMethodSettings_loggingLevel' - The logging level for this method. The logging level affects the log
-- entries that are pushed to CloudWatch Logs.
--
-- If the logging level is @ERROR@, then the logs only include error-level
-- entries.
--
-- If the logging level is @INFO@, then the logs include both @ERROR@
-- events and extra informational events.
--
-- Valid values: @OFF@ | @ERROR@ | @INFO@
--
-- 'throttlingBurstLimit', 'awsApiGatewayMethodSettings_throttlingBurstLimit' - The throttling burst limit for the method.
--
-- 'resourcePath', 'awsApiGatewayMethodSettings_resourcePath' - The resource path for this method. Forward slashes (\/) are encoded as
-- ~1 . The initial slash must include a forward slash.
--
-- For example, the path value @\/resource\/subresource@ must be encoded as
-- @\/~1resource~1subresource@.
--
-- To specify the root path, use only a slash (\/). You can use an asterisk
-- (*) as a wildcard to apply method settings to multiple methods.
--
-- 'metricsEnabled', 'awsApiGatewayMethodSettings_metricsEnabled' - Indicates whether CloudWatch metrics are enabled for the method.
--
-- 'requireAuthorizationForCacheControl', 'awsApiGatewayMethodSettings_requireAuthorizationForCacheControl' - Indicates whether authorization is required for a cache invalidation
-- request.
--
-- 'unauthorizedCacheControlHeaderStrategy', 'awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy' - Indicates how to handle unauthorized requests for cache invalidation.
--
-- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
-- @SUCCEED_WITHOUT_RESPONSE_HEADER@
--
-- 'httpMethod', 'awsApiGatewayMethodSettings_httpMethod' - The HTTP method. You can use an asterisk (*) as a wildcard to apply
-- method settings to multiple methods.
--
-- 'cacheTtlInSeconds', 'awsApiGatewayMethodSettings_cacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response is cached.
--
-- 'dataTraceEnabled', 'awsApiGatewayMethodSettings_dataTraceEnabled' - Indicates whether data trace logging is enabled for the method. Data
-- trace logging affects the log entries that are pushed to CloudWatch
-- Logs.
--
-- 'cachingEnabled', 'awsApiGatewayMethodSettings_cachingEnabled' - Indicates whether responses are cached and returned for requests. For
-- responses to be cached, a cache cluster must be enabled on the stage.
--
-- 'cacheDataEncrypted', 'awsApiGatewayMethodSettings_cacheDataEncrypted' - Indicates whether the cached responses are encrypted.
newAwsApiGatewayMethodSettings ::
  AwsApiGatewayMethodSettings
newAwsApiGatewayMethodSettings =
  AwsApiGatewayMethodSettings'
    { throttlingRateLimit =
        Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      resourcePath = Prelude.Nothing,
      metricsEnabled = Prelude.Nothing,
      requireAuthorizationForCacheControl =
        Prelude.Nothing,
      unauthorizedCacheControlHeaderStrategy =
        Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      cacheTtlInSeconds = Prelude.Nothing,
      dataTraceEnabled = Prelude.Nothing,
      cachingEnabled = Prelude.Nothing,
      cacheDataEncrypted = Prelude.Nothing
    }

-- | The throttling rate limit for the method.
awsApiGatewayMethodSettings_throttlingRateLimit :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Double)
awsApiGatewayMethodSettings_throttlingRateLimit = Lens.lens (\AwsApiGatewayMethodSettings' {throttlingRateLimit} -> throttlingRateLimit) (\s@AwsApiGatewayMethodSettings' {} a -> s {throttlingRateLimit = a} :: AwsApiGatewayMethodSettings)

-- | The logging level for this method. The logging level affects the log
-- entries that are pushed to CloudWatch Logs.
--
-- If the logging level is @ERROR@, then the logs only include error-level
-- entries.
--
-- If the logging level is @INFO@, then the logs include both @ERROR@
-- events and extra informational events.
--
-- Valid values: @OFF@ | @ERROR@ | @INFO@
awsApiGatewayMethodSettings_loggingLevel :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_loggingLevel = Lens.lens (\AwsApiGatewayMethodSettings' {loggingLevel} -> loggingLevel) (\s@AwsApiGatewayMethodSettings' {} a -> s {loggingLevel = a} :: AwsApiGatewayMethodSettings)

-- | The throttling burst limit for the method.
awsApiGatewayMethodSettings_throttlingBurstLimit :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayMethodSettings_throttlingBurstLimit = Lens.lens (\AwsApiGatewayMethodSettings' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@AwsApiGatewayMethodSettings' {} a -> s {throttlingBurstLimit = a} :: AwsApiGatewayMethodSettings)

-- | The resource path for this method. Forward slashes (\/) are encoded as
-- ~1 . The initial slash must include a forward slash.
--
-- For example, the path value @\/resource\/subresource@ must be encoded as
-- @\/~1resource~1subresource@.
--
-- To specify the root path, use only a slash (\/). You can use an asterisk
-- (*) as a wildcard to apply method settings to multiple methods.
awsApiGatewayMethodSettings_resourcePath :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_resourcePath = Lens.lens (\AwsApiGatewayMethodSettings' {resourcePath} -> resourcePath) (\s@AwsApiGatewayMethodSettings' {} a -> s {resourcePath = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether CloudWatch metrics are enabled for the method.
awsApiGatewayMethodSettings_metricsEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_metricsEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {metricsEnabled} -> metricsEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {metricsEnabled = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether authorization is required for a cache invalidation
-- request.
awsApiGatewayMethodSettings_requireAuthorizationForCacheControl :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_requireAuthorizationForCacheControl = Lens.lens (\AwsApiGatewayMethodSettings' {requireAuthorizationForCacheControl} -> requireAuthorizationForCacheControl) (\s@AwsApiGatewayMethodSettings' {} a -> s {requireAuthorizationForCacheControl = a} :: AwsApiGatewayMethodSettings)

-- | Indicates how to handle unauthorized requests for cache invalidation.
--
-- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
-- @SUCCEED_WITHOUT_RESPONSE_HEADER@
awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy = Lens.lens (\AwsApiGatewayMethodSettings' {unauthorizedCacheControlHeaderStrategy} -> unauthorizedCacheControlHeaderStrategy) (\s@AwsApiGatewayMethodSettings' {} a -> s {unauthorizedCacheControlHeaderStrategy = a} :: AwsApiGatewayMethodSettings)

-- | The HTTP method. You can use an asterisk (*) as a wildcard to apply
-- method settings to multiple methods.
awsApiGatewayMethodSettings_httpMethod :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_httpMethod = Lens.lens (\AwsApiGatewayMethodSettings' {httpMethod} -> httpMethod) (\s@AwsApiGatewayMethodSettings' {} a -> s {httpMethod = a} :: AwsApiGatewayMethodSettings)

-- | Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response is cached.
awsApiGatewayMethodSettings_cacheTtlInSeconds :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayMethodSettings_cacheTtlInSeconds = Lens.lens (\AwsApiGatewayMethodSettings' {cacheTtlInSeconds} -> cacheTtlInSeconds) (\s@AwsApiGatewayMethodSettings' {} a -> s {cacheTtlInSeconds = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether data trace logging is enabled for the method. Data
-- trace logging affects the log entries that are pushed to CloudWatch
-- Logs.
awsApiGatewayMethodSettings_dataTraceEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_dataTraceEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {dataTraceEnabled} -> dataTraceEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {dataTraceEnabled = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether responses are cached and returned for requests. For
-- responses to be cached, a cache cluster must be enabled on the stage.
awsApiGatewayMethodSettings_cachingEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_cachingEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {cachingEnabled} -> cachingEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {cachingEnabled = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether the cached responses are encrypted.
awsApiGatewayMethodSettings_cacheDataEncrypted :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_cacheDataEncrypted = Lens.lens (\AwsApiGatewayMethodSettings' {cacheDataEncrypted} -> cacheDataEncrypted) (\s@AwsApiGatewayMethodSettings' {} a -> s {cacheDataEncrypted = a} :: AwsApiGatewayMethodSettings)

instance Core.FromJSON AwsApiGatewayMethodSettings where
  parseJSON =
    Core.withObject
      "AwsApiGatewayMethodSettings"
      ( \x ->
          AwsApiGatewayMethodSettings'
            Prelude.<$> (x Core..:? "ThrottlingRateLimit")
            Prelude.<*> (x Core..:? "LoggingLevel")
            Prelude.<*> (x Core..:? "ThrottlingBurstLimit")
            Prelude.<*> (x Core..:? "ResourcePath")
            Prelude.<*> (x Core..:? "MetricsEnabled")
            Prelude.<*> (x Core..:? "RequireAuthorizationForCacheControl")
            Prelude.<*> (x Core..:? "UnauthorizedCacheControlHeaderStrategy")
            Prelude.<*> (x Core..:? "HttpMethod")
            Prelude.<*> (x Core..:? "CacheTtlInSeconds")
            Prelude.<*> (x Core..:? "DataTraceEnabled")
            Prelude.<*> (x Core..:? "CachingEnabled")
            Prelude.<*> (x Core..:? "CacheDataEncrypted")
      )

instance Prelude.Hashable AwsApiGatewayMethodSettings where
  hashWithSalt _salt AwsApiGatewayMethodSettings' {..} =
    _salt `Prelude.hashWithSalt` throttlingRateLimit
      `Prelude.hashWithSalt` loggingLevel
      `Prelude.hashWithSalt` throttlingBurstLimit
      `Prelude.hashWithSalt` resourcePath
      `Prelude.hashWithSalt` metricsEnabled
      `Prelude.hashWithSalt` requireAuthorizationForCacheControl
      `Prelude.hashWithSalt` unauthorizedCacheControlHeaderStrategy
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` cacheTtlInSeconds
      `Prelude.hashWithSalt` dataTraceEnabled
      `Prelude.hashWithSalt` cachingEnabled
      `Prelude.hashWithSalt` cacheDataEncrypted

instance Prelude.NFData AwsApiGatewayMethodSettings where
  rnf AwsApiGatewayMethodSettings' {..} =
    Prelude.rnf throttlingRateLimit
      `Prelude.seq` Prelude.rnf loggingLevel
      `Prelude.seq` Prelude.rnf throttlingBurstLimit
      `Prelude.seq` Prelude.rnf resourcePath
      `Prelude.seq` Prelude.rnf metricsEnabled
      `Prelude.seq` Prelude.rnf requireAuthorizationForCacheControl
      `Prelude.seq` Prelude.rnf unauthorizedCacheControlHeaderStrategy
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf cacheTtlInSeconds
      `Prelude.seq` Prelude.rnf dataTraceEnabled
      `Prelude.seq` Prelude.rnf cachingEnabled
      `Prelude.seq` Prelude.rnf cacheDataEncrypted

instance Core.ToJSON AwsApiGatewayMethodSettings where
  toJSON AwsApiGatewayMethodSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ThrottlingRateLimit" Core..=)
              Prelude.<$> throttlingRateLimit,
            ("LoggingLevel" Core..=) Prelude.<$> loggingLevel,
            ("ThrottlingBurstLimit" Core..=)
              Prelude.<$> throttlingBurstLimit,
            ("ResourcePath" Core..=) Prelude.<$> resourcePath,
            ("MetricsEnabled" Core..=)
              Prelude.<$> metricsEnabled,
            ("RequireAuthorizationForCacheControl" Core..=)
              Prelude.<$> requireAuthorizationForCacheControl,
            ("UnauthorizedCacheControlHeaderStrategy" Core..=)
              Prelude.<$> unauthorizedCacheControlHeaderStrategy,
            ("HttpMethod" Core..=) Prelude.<$> httpMethod,
            ("CacheTtlInSeconds" Core..=)
              Prelude.<$> cacheTtlInSeconds,
            ("DataTraceEnabled" Core..=)
              Prelude.<$> dataTraceEnabled,
            ("CachingEnabled" Core..=)
              Prelude.<$> cachingEnabled,
            ("CacheDataEncrypted" Core..=)
              Prelude.<$> cacheDataEncrypted
          ]
      )
