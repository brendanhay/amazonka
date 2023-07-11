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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { enableSIPLogs :: Prelude.Maybe Prelude.Bool
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
-- 'enableSIPLogs', 'loggingConfiguration_enableSIPLogs' - Undocumented member.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { enableSIPLogs =
        Prelude.Nothing
    }

-- | Undocumented member.
loggingConfiguration_enableSIPLogs :: Lens.Lens' LoggingConfiguration (Prelude.Maybe Prelude.Bool)
loggingConfiguration_enableSIPLogs = Lens.lens (\LoggingConfiguration' {enableSIPLogs} -> enableSIPLogs) (\s@LoggingConfiguration' {} a -> s {enableSIPLogs = a} :: LoggingConfiguration)

instance Data.FromJSON LoggingConfiguration where
  parseJSON =
    Data.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> (x Data..:? "EnableSIPLogs")
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` enableSIPLogs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf enableSIPLogs

instance Data.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableSIPLogs" Data..=)
              Prelude.<$> enableSIPLogs
          ]
      )
