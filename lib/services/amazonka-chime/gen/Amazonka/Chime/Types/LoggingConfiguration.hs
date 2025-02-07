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
-- Module      : Amazonka.Chime.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The logging configuration associated with an Amazon Chime Voice
-- Connector. Specifies whether SIP message logs are enabled for sending to
-- Amazon CloudWatch Logs.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | Boolean that enables the logging of Voice Connector metrics to
    -- Cloudwatch.
    enableMediaMetricLogs :: Prelude.Maybe Prelude.Bool,
    -- | When true, enables SIP message logs for sending to Amazon CloudWatch
    -- Logs.
    enableSIPLogs :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableMediaMetricLogs', 'loggingConfiguration_enableMediaMetricLogs' - Boolean that enables the logging of Voice Connector metrics to
-- Cloudwatch.
--
-- 'enableSIPLogs', 'loggingConfiguration_enableSIPLogs' - When true, enables SIP message logs for sending to Amazon CloudWatch
-- Logs.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { enableMediaMetricLogs =
        Prelude.Nothing,
      enableSIPLogs = Prelude.Nothing
    }

-- | Boolean that enables the logging of Voice Connector metrics to
-- Cloudwatch.
loggingConfiguration_enableMediaMetricLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_enableMediaMetricLogs = Lens.lens (\LoggingConfiguration' {enableMediaMetricLogs} -> enableMediaMetricLogs) (\s@LoggingConfiguration' {} a -> s {enableMediaMetricLogs = a} :: LoggingConfiguration)

-- | When true, enables SIP message logs for sending to Amazon CloudWatch
-- Logs.
loggingConfiguration_enableSIPLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_enableSIPLogs = Lens.lens (\LoggingConfiguration' {enableSIPLogs} -> enableSIPLogs) (\s@LoggingConfiguration' {} a -> s {enableSIPLogs = a} :: LoggingConfiguration)

instance Data.FromJSON LoggingConfiguration where
  parseJSON =
    Data.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Data..:? "EnableMediaMetricLogs")
            Prelude.<*> (x Data..:? "EnableSIPLogs")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` enableMediaMetricLogs
      `Prelude.hashWithSalt` enableSIPLogs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf enableMediaMetricLogs `Prelude.seq`
      Prelude.rnf enableSIPLogs

instance Data.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableMediaMetricLogs" Data..=)
              Prelude.<$> enableMediaMetricLogs,
            ("EnableSIPLogs" Data..=) Prelude.<$> enableSIPLogs
          ]
      )
