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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayV2RouteSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains route settings for a stage.
--
-- /See:/ 'newAwsApiGatewayV2RouteSettings' smart constructor.
data AwsApiGatewayV2RouteSettings = AwsApiGatewayV2RouteSettings'
  { -- | Indicates whether data trace logging is enabled. Data trace logging
    -- affects the log entries that are pushed to CloudWatch Logs. Supported
    -- only for WebSocket APIs.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether detailed metrics are enabled.
    detailedMetricsEnabled :: Prelude.Maybe Prelude.Bool,
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
    -- | The throttling burst limit.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | The throttling rate limit.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double
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
-- 'detailedMetricsEnabled', 'awsApiGatewayV2RouteSettings_detailedMetricsEnabled' - Indicates whether detailed metrics are enabled.
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
-- 'throttlingBurstLimit', 'awsApiGatewayV2RouteSettings_throttlingBurstLimit' - The throttling burst limit.
--
-- 'throttlingRateLimit', 'awsApiGatewayV2RouteSettings_throttlingRateLimit' - The throttling rate limit.
newAwsApiGatewayV2RouteSettings ::
  AwsApiGatewayV2RouteSettings
newAwsApiGatewayV2RouteSettings =
  AwsApiGatewayV2RouteSettings'
    { dataTraceEnabled =
        Prelude.Nothing,
      detailedMetricsEnabled = Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      throttlingRateLimit = Prelude.Nothing
    }

-- | Indicates whether data trace logging is enabled. Data trace logging
-- affects the log entries that are pushed to CloudWatch Logs. Supported
-- only for WebSocket APIs.
awsApiGatewayV2RouteSettings_dataTraceEnabled :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2RouteSettings_dataTraceEnabled = Lens.lens (\AwsApiGatewayV2RouteSettings' {dataTraceEnabled} -> dataTraceEnabled) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {dataTraceEnabled = a} :: AwsApiGatewayV2RouteSettings)

-- | Indicates whether detailed metrics are enabled.
awsApiGatewayV2RouteSettings_detailedMetricsEnabled :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Bool)
awsApiGatewayV2RouteSettings_detailedMetricsEnabled = Lens.lens (\AwsApiGatewayV2RouteSettings' {detailedMetricsEnabled} -> detailedMetricsEnabled) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {detailedMetricsEnabled = a} :: AwsApiGatewayV2RouteSettings)

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

-- | The throttling burst limit.
awsApiGatewayV2RouteSettings_throttlingBurstLimit :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Int)
awsApiGatewayV2RouteSettings_throttlingBurstLimit = Lens.lens (\AwsApiGatewayV2RouteSettings' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {throttlingBurstLimit = a} :: AwsApiGatewayV2RouteSettings)

-- | The throttling rate limit.
awsApiGatewayV2RouteSettings_throttlingRateLimit :: Lens.Lens' AwsApiGatewayV2RouteSettings (Prelude.Maybe Prelude.Double)
awsApiGatewayV2RouteSettings_throttlingRateLimit = Lens.lens (\AwsApiGatewayV2RouteSettings' {throttlingRateLimit} -> throttlingRateLimit) (\s@AwsApiGatewayV2RouteSettings' {} a -> s {throttlingRateLimit = a} :: AwsApiGatewayV2RouteSettings)

instance Data.FromJSON AwsApiGatewayV2RouteSettings where
  parseJSON =
    Data.withObject
      "AwsApiGatewayV2RouteSettings"
      ( \x ->
          AwsApiGatewayV2RouteSettings'
            Prelude.<$> (x Data..:? "DataTraceEnabled")
            Prelude.<*> (x Data..:? "DetailedMetricsEnabled")
            Prelude.<*> (x Data..:? "LoggingLevel")
            Prelude.<*> (x Data..:? "ThrottlingBurstLimit")
            Prelude.<*> (x Data..:? "ThrottlingRateLimit")
      )

instance
  Prelude.Hashable
    AwsApiGatewayV2RouteSettings
  where
  hashWithSalt _salt AwsApiGatewayV2RouteSettings' {..} =
    _salt
      `Prelude.hashWithSalt` dataTraceEnabled
      `Prelude.hashWithSalt` detailedMetricsEnabled
      `Prelude.hashWithSalt` loggingLevel
      `Prelude.hashWithSalt` throttlingBurstLimit
      `Prelude.hashWithSalt` throttlingRateLimit

instance Prelude.NFData AwsApiGatewayV2RouteSettings where
  rnf AwsApiGatewayV2RouteSettings' {..} =
    Prelude.rnf dataTraceEnabled `Prelude.seq`
      Prelude.rnf detailedMetricsEnabled `Prelude.seq`
        Prelude.rnf loggingLevel `Prelude.seq`
          Prelude.rnf throttlingBurstLimit `Prelude.seq`
            Prelude.rnf throttlingRateLimit

instance Data.ToJSON AwsApiGatewayV2RouteSettings where
  toJSON AwsApiGatewayV2RouteSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataTraceEnabled" Data..=)
              Prelude.<$> dataTraceEnabled,
            ("DetailedMetricsEnabled" Data..=)
              Prelude.<$> detailedMetricsEnabled,
            ("LoggingLevel" Data..=) Prelude.<$> loggingLevel,
            ("ThrottlingBurstLimit" Data..=)
              Prelude.<$> throttlingBurstLimit,
            ("ThrottlingRateLimit" Data..=)
              Prelude.<$> throttlingRateLimit
          ]
      )
