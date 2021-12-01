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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains route settings for a stage.
--
-- /See:/ 'newAwsApiGatewayV2RouteSettings' smart constructor.
data AwsApiGatewayV2RouteSettings = AwsApiGatewayV2RouteSettings'
  { -- | Indicates whether data trace logging is enabled. Data trace logging
    -- affects the log entries that are pushed to CloudWatch Logs. Supported
    -- only for WebSocket APIs.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The throttling burst limit.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | The logging level. The logging level affects the log entries that are
    -- pushed to CloudWatch Logs. Supported only for WebSocket APIs.
    --
    -- If the logging level is @ERROR@, then the logs only include error-level
    -- entries.
    --
    -- If the logging level is @INFO@, then the logs include both @ERROR@
    -- events and extra informational events.
    --
    -- Valid values: @OFF@ | @ERROR@ | @INFO@
    loggingLevel :: Prelude.Maybe Prelude.Text,
    -- | The throttling rate limit.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double,
    -- | Indicates whether detailed metrics are enabled.
    detailedMetricsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayV2RouteSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTraceEnabled', 'awsApiGatewayV2RouteSettings_dataTraceEnabled' - Indicates whether data trace logging is enabled. Data trace logging
-- affects the log entries that are pushed to CloudWatch Logs. Supported
-- only for WebSocket APIs.
--
-- 'throttlingBurstLimit', 'awsApiGatewayV2RouteSettings_throttlingBurstLimit' - The throttling burst limit.
--
-- 'loggingLevel', 'awsApiGatewayV2RouteSettings_loggingLevel' - The logging level. The logging level affects the log entries that are
-- pushed to CloudWatch Logs. Supported only for WebSocket APIs.
--
-- If the logging level is @ERROR@, then the logs only include error-level
-- entries.
--
-- If the logging level is @INFO@, then the logs include both @ERROR@
-- events and extra informational events.
--
-- Valid values: @OFF@ | @ERROR@ | @INFO@
--
-- 'throttlingRateLimit', 'awsApiGatewayV2RouteSettings_throttlingRateLimit' - The throttling rate limit.
--
-- 'detailedMetricsEnabled', 'awsApiGatewayV2RouteSettings_detailedMetricsEnabled' - Indicates whether detailed metrics are enabled.
newAwsApiGatewayV2RouteSettings ::
  AwsApiGatewayV2RouteSettings
newAwsApiGatewayV2RouteSettings =
  AwsApiGatewayV2RouteSettings'
    { dataTraceEnabled =
        Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      throttlingRateLimit = Prelude.Nothing,
      detailedMetricsEnabled = Prelude.Nothing
    }

-- | Indicates whether data trace logging is enabled. Data trace logging
-- affects the log entries that are pushed to CloudWatch Logs. Supported
-- only for WebSocket APIs.
awsApiGatewayV2RouteSettings_dataTraceEnabled :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2RouteSettings_dataTraceEnabled = Lens.lens (\AwsApiGatewayV2RouteSettings' {dataTraceEnabled} -> dataTraceEnabled) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {dataTraceEnabled = a} :: AwsApiGatewayV2RouteSettings)

-- | The throttling burst limit.
awsApiGatewayV2RouteSettings_throttlingBurstLimit :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayV2RouteSettings_throttlingBurstLimit = Lens.lens (\AwsApiGatewayV2RouteSettings' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {throttlingBurstLimit = a} :: AwsApiGatewayV2RouteSettings)

-- | The logging level. The logging level affects the log entries that are
-- pushed to CloudWatch Logs. Supported only for WebSocket APIs.
--
-- If the logging level is @ERROR@, then the logs only include error-level
-- entries.
--
-- If the logging level is @INFO@, then the logs include both @ERROR@
-- events and extra informational events.
--
-- Valid values: @OFF@ | @ERROR@ | @INFO@
awsApiGatewayV2RouteSettings_loggingLevel :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Text)
awsApiGatewayV2RouteSettings_loggingLevel = Lens.lens (\AwsApiGatewayV2RouteSettings' {loggingLevel} -> loggingLevel) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {loggingLevel = a} :: AwsApiGatewayV2RouteSettings)

-- | The throttling rate limit.
awsApiGatewayV2RouteSettings_throttlingRateLimit :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Double)
awsApiGatewayV2RouteSettings_throttlingRateLimit = Lens.lens (\AwsApiGatewayV2RouteSettings' {throttlingRateLimit} -> throttlingRateLimit) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {throttlingRateLimit = a} :: AwsApiGatewayV2RouteSettings)

-- | Indicates whether detailed metrics are enabled.
awsApiGatewayV2RouteSettings_detailedMetricsEnabled :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2RouteSettings_detailedMetricsEnabled = Lens.lens (\AwsApiGatewayV2RouteSettings' {detailedMetricsEnabled} -> detailedMetricsEnabled) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {detailedMetricsEnabled = a} :: AwsApiGatewayV2RouteSettings)

instance Core.FromJSON AwsApiGatewayV2RouteSettings where
  parseJSON =
    Core.withObject
      "AwsApiGatewayV2RouteSettings"
      ( \x ->
          AwsApiGatewayV2RouteSettings'
            Prelude.<$> (x Core..:? "DataTraceEnabled")
            Prelude.<*> (x Core..:? "ThrottlingBurstLimit")
            Prelude.<*> (x Core..:? "LoggingLevel")
            Prelude.<*> (x Core..:? "ThrottlingRateLimit")
            Prelude.<*> (x Core..:? "DetailedMetricsEnabled")
      )

instance
  Prelude.Hashable
    AwsApiGatewayV2RouteSettings
  where
  hashWithSalt salt' AwsApiGatewayV2RouteSettings' {..} =
    salt' `Prelude.hashWithSalt` detailedMetricsEnabled
      `Prelude.hashWithSalt` throttlingRateLimit
      `Prelude.hashWithSalt` loggingLevel
      `Prelude.hashWithSalt` throttlingBurstLimit
      `Prelude.hashWithSalt` dataTraceEnabled

instance Prelude.NFData AwsApiGatewayV2RouteSettings where
  rnf AwsApiGatewayV2RouteSettings' {..} =
    Prelude.rnf dataTraceEnabled
      `Prelude.seq` Prelude.rnf detailedMetricsEnabled
      `Prelude.seq` Prelude.rnf throttlingRateLimit
      `Prelude.seq` Prelude.rnf loggingLevel
      `Prelude.seq` Prelude.rnf throttlingBurstLimit

instance Core.ToJSON AwsApiGatewayV2RouteSettings where
  toJSON AwsApiGatewayV2RouteSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataTraceEnabled" Core..=)
              Prelude.<$> dataTraceEnabled,
            ("ThrottlingBurstLimit" Core..=)
              Prelude.<$> throttlingBurstLimit,
            ("LoggingLevel" Core..=) Prelude.<$> loggingLevel,
            ("ThrottlingRateLimit" Core..=)
              Prelude.<$> throttlingRateLimit,
            ("DetailedMetricsEnabled" Core..=)
              Prelude.<$> detailedMetricsEnabled
          ]
      )
