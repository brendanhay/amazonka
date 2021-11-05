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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayMethodSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines settings for a method for the stage.
--
-- /See:/ 'newAwsApiGatewayMethodSettings' smart constructor.
data AwsApiGatewayMethodSettings = AwsApiGatewayMethodSettings'
  { -- | Indicates whether data trace logging is enabled for the method. Data
    -- trace logging affects the log entries that are pushed to CloudWatch
    -- Logs.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time to live (TTL), in seconds, for cached responses. The
    -- higher the TTL, the longer the response is cached.
    cacheTtlInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The HTTP method. You can use an asterisk (*) as a wildcard to apply
    -- method settings to multiple methods.
    httpMethod :: Prelude.Maybe Prelude.Text,
    -- | The throttling burst limit for the method.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the cached responses are encrypted.
    cacheDataEncrypted :: Prelude.Maybe Prelude.Bool,
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
    -- | Indicates whether authorization is required for a cache invalidation
    -- request.
    requireAuthorizationForCacheControl :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether responses are cached and returned for requests. For
    -- responses to be cached, a cache cluster must be enabled on the stage.
    cachingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The resource path for this method. Forward slashes (\/) are encoded as
    -- ~1 . The initial slash must include a forward slash.
    --
    -- For example, the path value @\/resource\/subresource@ must be encoded as
    -- @\/~1resource~1subresource@.
    --
    -- To specify the root path, use only a slash (\/). You can use an asterisk
    -- (*) as a wildcard to apply method settings to multiple methods.
    resourcePath :: Prelude.Maybe Prelude.Text,
    -- | The throttling rate limit for the method.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether CloudWatch metrics are enabled for the method.
    metricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates how to handle unauthorized requests for cache invalidation.
    --
    -- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
    -- @SUCCEED_WITHOUT_RESPONSE_HEADER@
    unauthorizedCacheControlHeaderStrategy :: Prelude.Maybe Prelude.Text
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
-- 'dataTraceEnabled', 'awsApiGatewayMethodSettings_dataTraceEnabled' - Indicates whether data trace logging is enabled for the method. Data
-- trace logging affects the log entries that are pushed to CloudWatch
-- Logs.
--
-- 'cacheTtlInSeconds', 'awsApiGatewayMethodSettings_cacheTtlInSeconds' - Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response is cached.
--
-- 'httpMethod', 'awsApiGatewayMethodSettings_httpMethod' - The HTTP method. You can use an asterisk (*) as a wildcard to apply
-- method settings to multiple methods.
--
-- 'throttlingBurstLimit', 'awsApiGatewayMethodSettings_throttlingBurstLimit' - The throttling burst limit for the method.
--
-- 'cacheDataEncrypted', 'awsApiGatewayMethodSettings_cacheDataEncrypted' - Indicates whether the cached responses are encrypted.
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
-- 'requireAuthorizationForCacheControl', 'awsApiGatewayMethodSettings_requireAuthorizationForCacheControl' - Indicates whether authorization is required for a cache invalidation
-- request.
--
-- 'cachingEnabled', 'awsApiGatewayMethodSettings_cachingEnabled' - Indicates whether responses are cached and returned for requests. For
-- responses to be cached, a cache cluster must be enabled on the stage.
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
-- 'throttlingRateLimit', 'awsApiGatewayMethodSettings_throttlingRateLimit' - The throttling rate limit for the method.
--
-- 'metricsEnabled', 'awsApiGatewayMethodSettings_metricsEnabled' - Indicates whether CloudWatch metrics are enabled for the method.
--
-- 'unauthorizedCacheControlHeaderStrategy', 'awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy' - Indicates how to handle unauthorized requests for cache invalidation.
--
-- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
-- @SUCCEED_WITHOUT_RESPONSE_HEADER@
newAwsApiGatewayMethodSettings ::
  AwsApiGatewayMethodSettings
newAwsApiGatewayMethodSettings =
  AwsApiGatewayMethodSettings'
    { dataTraceEnabled =
        Prelude.Nothing,
      cacheTtlInSeconds = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      cacheDataEncrypted = Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      requireAuthorizationForCacheControl =
        Prelude.Nothing,
      cachingEnabled = Prelude.Nothing,
      resourcePath = Prelude.Nothing,
      throttlingRateLimit = Prelude.Nothing,
      metricsEnabled = Prelude.Nothing,
      unauthorizedCacheControlHeaderStrategy =
        Prelude.Nothing
    }

-- | Indicates whether data trace logging is enabled for the method. Data
-- trace logging affects the log entries that are pushed to CloudWatch
-- Logs.
awsApiGatewayMethodSettings_dataTraceEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_dataTraceEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {dataTraceEnabled} -> dataTraceEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {dataTraceEnabled = a} :: AwsApiGatewayMethodSettings)

-- | Specifies the time to live (TTL), in seconds, for cached responses. The
-- higher the TTL, the longer the response is cached.
awsApiGatewayMethodSettings_cacheTtlInSeconds :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayMethodSettings_cacheTtlInSeconds = Lens.lens (\AwsApiGatewayMethodSettings' {cacheTtlInSeconds} -> cacheTtlInSeconds) (\s@AwsApiGatewayMethodSettings' {} a -> s {cacheTtlInSeconds = a} :: AwsApiGatewayMethodSettings)

-- | The HTTP method. You can use an asterisk (*) as a wildcard to apply
-- method settings to multiple methods.
awsApiGatewayMethodSettings_httpMethod :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_httpMethod = Lens.lens (\AwsApiGatewayMethodSettings' {httpMethod} -> httpMethod) (\s@AwsApiGatewayMethodSettings' {} a -> s {httpMethod = a} :: AwsApiGatewayMethodSettings)

-- | The throttling burst limit for the method.
awsApiGatewayMethodSettings_throttlingBurstLimit :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayMethodSettings_throttlingBurstLimit = Lens.lens (\AwsApiGatewayMethodSettings' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@AwsApiGatewayMethodSettings' {} a -> s {throttlingBurstLimit = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether the cached responses are encrypted.
awsApiGatewayMethodSettings_cacheDataEncrypted :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_cacheDataEncrypted = Lens.lens (\AwsApiGatewayMethodSettings' {cacheDataEncrypted} -> cacheDataEncrypted) (\s@AwsApiGatewayMethodSettings' {} a -> s {cacheDataEncrypted = a} :: AwsApiGatewayMethodSettings)

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

-- | Indicates whether authorization is required for a cache invalidation
-- request.
awsApiGatewayMethodSettings_requireAuthorizationForCacheControl :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_requireAuthorizationForCacheControl = Lens.lens (\AwsApiGatewayMethodSettings' {requireAuthorizationForCacheControl} -> requireAuthorizationForCacheControl) (\s@AwsApiGatewayMethodSettings' {} a -> s {requireAuthorizationForCacheControl = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether responses are cached and returned for requests. For
-- responses to be cached, a cache cluster must be enabled on the stage.
awsApiGatewayMethodSettings_cachingEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_cachingEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {cachingEnabled} -> cachingEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {cachingEnabled = a} :: AwsApiGatewayMethodSettings)

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

-- | The throttling rate limit for the method.
awsApiGatewayMethodSettings_throttlingRateLimit :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Double)
awsApiGatewayMethodSettings_throttlingRateLimit = Lens.lens (\AwsApiGatewayMethodSettings' {throttlingRateLimit} -> throttlingRateLimit) (\s@AwsApiGatewayMethodSettings' {} a -> s {throttlingRateLimit = a} :: AwsApiGatewayMethodSettings)

-- | Indicates whether CloudWatch metrics are enabled for the method.
awsApiGatewayMethodSettings_metricsEnabled :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayMethodSettings_metricsEnabled = Lens.lens (\AwsApiGatewayMethodSettings' {metricsEnabled} -> metricsEnabled) (\s@AwsApiGatewayMethodSettings' {} a -> s {metricsEnabled = a} :: AwsApiGatewayMethodSettings)

-- | Indicates how to handle unauthorized requests for cache invalidation.
--
-- Valid values: @FAIL_WITH_403@ | @SUCCEED_WITH_RESPONSE_HEADER@ |
-- @SUCCEED_WITHOUT_RESPONSE_HEADER@
awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy :: Lens.Lens' AwsApiGatewayMethodSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayMethodSettings_unauthorizedCacheControlHeaderStrategy = Lens.lens (\AwsApiGatewayMethodSettings' {unauthorizedCacheControlHeaderStrategy} -> unauthorizedCacheControlHeaderStrategy) (\s@AwsApiGatewayMethodSettings' {} a -> s {unauthorizedCacheControlHeaderStrategy = a} :: AwsApiGatewayMethodSettings)

instance Core.FromJSON AwsApiGatewayMethodSettings where
  parseJSON =
    Core.withObject
      "AwsApiGatewayMethodSettings"
      ( \x ->
          AwsApiGatewayMethodSettings'
            Prelude.<$> (x Core..:? "DataTraceEnabled")
            Prelude.<*> (x Core..:? "CacheTtlInSeconds")
            Prelude.<*> (x Core..:? "HttpMethod")
            Prelude.<*> (x Core..:? "ThrottlingBurstLimit")
            Prelude.<*> (x Core..:? "CacheDataEncrypted")
            Prelude.<*> (x Core..:? "LoggingLevel")
            Prelude.<*> (x Core..:? "RequireAuthorizationForCacheControl")
            Prelude.<*> (x Core..:? "CachingEnabled")
            Prelude.<*> (x Core..:? "ResourcePath")
            Prelude.<*> (x Core..:? "ThrottlingRateLimit")
            Prelude.<*> (x Core..:? "MetricsEnabled")
            Prelude.<*> ( x
                            Core..:? "UnauthorizedCacheControlHeaderStrategy"
                        )
      )

instance Prelude.Hashable AwsApiGatewayMethodSettings

instance Prelude.NFData AwsApiGatewayMethodSettings

instance Core.ToJSON AwsApiGatewayMethodSettings where
  toJSON AwsApiGatewayMethodSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataTraceEnabled" Core..=)
              Prelude.<$> dataTraceEnabled,
            ("CacheTtlInSeconds" Core..=)
              Prelude.<$> cacheTtlInSeconds,
            ("HttpMethod" Core..=) Prelude.<$> httpMethod,
            ("ThrottlingBurstLimit" Core..=)
              Prelude.<$> throttlingBurstLimit,
            ("CacheDataEncrypted" Core..=)
              Prelude.<$> cacheDataEncrypted,
            ("LoggingLevel" Core..=) Prelude.<$> loggingLevel,
            ("RequireAuthorizationForCacheControl" Core..=)
              Prelude.<$> requireAuthorizationForCacheControl,
            ("CachingEnabled" Core..=)
              Prelude.<$> cachingEnabled,
            ("ResourcePath" Core..=) Prelude.<$> resourcePath,
            ("ThrottlingRateLimit" Core..=)
              Prelude.<$> throttlingRateLimit,
            ("MetricsEnabled" Core..=)
              Prelude.<$> metricsEnabled,
            ("UnauthorizedCacheControlHeaderStrategy" Core..=)
              Prelude.<$> unauthorizedCacheControlHeaderStrategy
          ]
      )
