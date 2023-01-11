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
-- Module      : Amazonka.ApiGatewayV2.Types.RouteSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.RouteSettings where

import Amazonka.ApiGatewayV2.Types.LoggingLevel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of route settings.
--
-- /See:/ 'newRouteSettings' smart constructor.
data RouteSettings = RouteSettings'
  { -- | Specifies whether (true) or not (false) data trace logging is enabled
    -- for this route. This property affects the log entries pushed to Amazon
    -- CloudWatch Logs. Supported only for WebSocket APIs.
    dataTraceEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether detailed metrics are enabled.
    detailedMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the logging level for this route: INFO, ERROR, or OFF. This
    -- property affects the log entries pushed to Amazon CloudWatch Logs.
    -- Supported only for WebSocket APIs.
    loggingLevel :: Prelude.Maybe LoggingLevel,
    -- | Specifies the throttling burst limit.
    throttlingBurstLimit :: Prelude.Maybe Prelude.Int,
    -- | Specifies the throttling rate limit.
    throttlingRateLimit :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTraceEnabled', 'routeSettings_dataTraceEnabled' - Specifies whether (true) or not (false) data trace logging is enabled
-- for this route. This property affects the log entries pushed to Amazon
-- CloudWatch Logs. Supported only for WebSocket APIs.
--
-- 'detailedMetricsEnabled', 'routeSettings_detailedMetricsEnabled' - Specifies whether detailed metrics are enabled.
--
-- 'loggingLevel', 'routeSettings_loggingLevel' - Specifies the logging level for this route: INFO, ERROR, or OFF. This
-- property affects the log entries pushed to Amazon CloudWatch Logs.
-- Supported only for WebSocket APIs.
--
-- 'throttlingBurstLimit', 'routeSettings_throttlingBurstLimit' - Specifies the throttling burst limit.
--
-- 'throttlingRateLimit', 'routeSettings_throttlingRateLimit' - Specifies the throttling rate limit.
newRouteSettings ::
  RouteSettings
newRouteSettings =
  RouteSettings'
    { dataTraceEnabled = Prelude.Nothing,
      detailedMetricsEnabled = Prelude.Nothing,
      loggingLevel = Prelude.Nothing,
      throttlingBurstLimit = Prelude.Nothing,
      throttlingRateLimit = Prelude.Nothing
    }

-- | Specifies whether (true) or not (false) data trace logging is enabled
-- for this route. This property affects the log entries pushed to Amazon
-- CloudWatch Logs. Supported only for WebSocket APIs.
routeSettings_dataTraceEnabled :: Lens.Lens' RouteSettings (Prelude.Maybe Prelude.Bool)
routeSettings_dataTraceEnabled = Lens.lens (\RouteSettings' {dataTraceEnabled} -> dataTraceEnabled) (\s@RouteSettings' {} a -> s {dataTraceEnabled = a} :: RouteSettings)

-- | Specifies whether detailed metrics are enabled.
routeSettings_detailedMetricsEnabled :: Lens.Lens' RouteSettings (Prelude.Maybe Prelude.Bool)
routeSettings_detailedMetricsEnabled = Lens.lens (\RouteSettings' {detailedMetricsEnabled} -> detailedMetricsEnabled) (\s@RouteSettings' {} a -> s {detailedMetricsEnabled = a} :: RouteSettings)

-- | Specifies the logging level for this route: INFO, ERROR, or OFF. This
-- property affects the log entries pushed to Amazon CloudWatch Logs.
-- Supported only for WebSocket APIs.
routeSettings_loggingLevel :: Lens.Lens' RouteSettings (Prelude.Maybe LoggingLevel)
routeSettings_loggingLevel = Lens.lens (\RouteSettings' {loggingLevel} -> loggingLevel) (\s@RouteSettings' {} a -> s {loggingLevel = a} :: RouteSettings)

-- | Specifies the throttling burst limit.
routeSettings_throttlingBurstLimit :: Lens.Lens' RouteSettings (Prelude.Maybe Prelude.Int)
routeSettings_throttlingBurstLimit = Lens.lens (\RouteSettings' {throttlingBurstLimit} -> throttlingBurstLimit) (\s@RouteSettings' {} a -> s {throttlingBurstLimit = a} :: RouteSettings)

-- | Specifies the throttling rate limit.
routeSettings_throttlingRateLimit :: Lens.Lens' RouteSettings (Prelude.Maybe Prelude.Double)
routeSettings_throttlingRateLimit = Lens.lens (\RouteSettings' {throttlingRateLimit} -> throttlingRateLimit) (\s@RouteSettings' {} a -> s {throttlingRateLimit = a} :: RouteSettings)

instance Data.FromJSON RouteSettings where
  parseJSON =
    Data.withObject
      "RouteSettings"
      ( \x ->
          RouteSettings'
            Prelude.<$> (x Data..:? "dataTraceEnabled")
            Prelude.<*> (x Data..:? "detailedMetricsEnabled")
            Prelude.<*> (x Data..:? "loggingLevel")
            Prelude.<*> (x Data..:? "throttlingBurstLimit")
            Prelude.<*> (x Data..:? "throttlingRateLimit")
      )

instance Prelude.Hashable RouteSettings where
  hashWithSalt _salt RouteSettings' {..} =
    _salt `Prelude.hashWithSalt` dataTraceEnabled
      `Prelude.hashWithSalt` detailedMetricsEnabled
      `Prelude.hashWithSalt` loggingLevel
      `Prelude.hashWithSalt` throttlingBurstLimit
      `Prelude.hashWithSalt` throttlingRateLimit

instance Prelude.NFData RouteSettings where
  rnf RouteSettings' {..} =
    Prelude.rnf dataTraceEnabled
      `Prelude.seq` Prelude.rnf detailedMetricsEnabled
      `Prelude.seq` Prelude.rnf loggingLevel
      `Prelude.seq` Prelude.rnf throttlingBurstLimit
      `Prelude.seq` Prelude.rnf throttlingRateLimit

instance Data.ToJSON RouteSettings where
  toJSON RouteSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataTraceEnabled" Data..=)
              Prelude.<$> dataTraceEnabled,
            ("detailedMetricsEnabled" Data..=)
              Prelude.<$> detailedMetricsEnabled,
            ("loggingLevel" Data..=) Prelude.<$> loggingLevel,
            ("throttlingBurstLimit" Data..=)
              Prelude.<$> throttlingBurstLimit,
            ("throttlingRateLimit" Data..=)
              Prelude.<$> throttlingRateLimit
          ]
      )
