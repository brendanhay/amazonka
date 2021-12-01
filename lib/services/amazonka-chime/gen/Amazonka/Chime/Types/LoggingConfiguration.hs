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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The logging configuration associated with an Amazon Chime Voice
-- Connector. Specifies whether SIP message logs are enabled for sending to
-- Amazon CloudWatch Logs.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | When true, enables SIP message logs for sending to Amazon CloudWatch
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
-- 'enableSIPLogs', 'loggingConfiguration_enableSIPLogs' - When true, enables SIP message logs for sending to Amazon CloudWatch
-- Logs.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { enableSIPLogs =
        Prelude.Nothing
    }

-- | When true, enables SIP message logs for sending to Amazon CloudWatch
-- Logs.
loggingConfiguration_enableSIPLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_enableSIPLogs = Lens.lens (\LoggingConfiguration' {enableSIPLogs} -> enableSIPLogs) (\s@LoggingConfiguration' {} a -> s {enableSIPLogs = a} :: LoggingConfiguration)

instance Core.FromJSON LoggingConfiguration where
  parseJSON =
    Core.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Core..:? "EnableSIPLogs")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt salt' LoggingConfiguration' {..} =
    salt' `Prelude.hashWithSalt` enableSIPLogs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf enableSIPLogs

instance Core.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableSIPLogs" Core..=)
              Prelude.<$> enableSIPLogs
          ]
      )
