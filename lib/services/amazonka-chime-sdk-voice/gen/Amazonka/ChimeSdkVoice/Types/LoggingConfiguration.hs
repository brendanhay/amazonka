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
-- Module      : Amazonka.ChimeSdkVoice.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The logging configuration associated with an Amazon Chime SDK Voice
-- Connector. Specifies whether SIP message logs can be sent to Amazon
-- CloudWatch Logs.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | Enables or disables media metrics logging.
    enableMediaMetricLogs :: Prelude.Maybe Prelude.Bool,
    -- | Boolean that enables sending SIP message logs to Amazon CloudWatch.
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
-- 'enableMediaMetricLogs', 'loggingConfiguration_enableMediaMetricLogs' - Enables or disables media metrics logging.
--
-- 'enableSIPLogs', 'loggingConfiguration_enableSIPLogs' - Boolean that enables sending SIP message logs to Amazon CloudWatch.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { enableMediaMetricLogs =
        Prelude.Nothing,
      enableSIPLogs = Prelude.Nothing
    }

-- | Enables or disables media metrics logging.
loggingConfiguration_enableMediaMetricLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_enableMediaMetricLogs = Lens.lens (\LoggingConfiguration' {enableMediaMetricLogs} -> enableMediaMetricLogs) (\s@LoggingConfiguration' {} a -> s {enableMediaMetricLogs = a} :: LoggingConfiguration)

-- | Boolean that enables sending SIP message logs to Amazon CloudWatch.
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
    Prelude.rnf enableMediaMetricLogs
      `Prelude.seq` Prelude.rnf enableSIPLogs

instance Data.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableMediaMetricLogs" Data..=)
              Prelude.<$> enableMediaMetricLogs,
            ("EnableSIPLogs" Data..=) Prelude.<$> enableSIPLogs
          ]
      )
