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
-- Module      : Amazonka.ChimeSdkVoice.Types.SipMediaApplicationLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SipMediaApplicationLoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The logging configuration of a SIP media application.
--
-- /See:/ 'newSipMediaApplicationLoggingConfiguration' smart constructor.
data SipMediaApplicationLoggingConfiguration = SipMediaApplicationLoggingConfiguration'
  { -- | Enables message logging for the specified SIP media application.
    enableSipMediaApplicationMessageLogs :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipMediaApplicationLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableSipMediaApplicationMessageLogs', 'sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs' - Enables message logging for the specified SIP media application.
newSipMediaApplicationLoggingConfiguration ::
  SipMediaApplicationLoggingConfiguration
newSipMediaApplicationLoggingConfiguration =
  SipMediaApplicationLoggingConfiguration'
    { enableSipMediaApplicationMessageLogs =
        Prelude.Nothing
    }

-- | Enables message logging for the specified SIP media application.
sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs :: Lens.Lens' SipMediaApplicationLoggingConfiguration (Prelude.Maybe Prelude.Bool)
sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs = Lens.lens (\SipMediaApplicationLoggingConfiguration' {enableSipMediaApplicationMessageLogs} -> enableSipMediaApplicationMessageLogs) (\s@SipMediaApplicationLoggingConfiguration' {} a -> s {enableSipMediaApplicationMessageLogs = a} :: SipMediaApplicationLoggingConfiguration)

instance
  Data.FromJSON
    SipMediaApplicationLoggingConfiguration
  where
  parseJSON =
    Data.withObject
      "SipMediaApplicationLoggingConfiguration"
      ( \x ->
          SipMediaApplicationLoggingConfiguration'
            Prelude.<$> (x Data..:? "EnableSipMediaApplicationMessageLogs")
      )

instance
  Prelude.Hashable
    SipMediaApplicationLoggingConfiguration
  where
  hashWithSalt
    _salt
    SipMediaApplicationLoggingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` enableSipMediaApplicationMessageLogs

instance
  Prelude.NFData
    SipMediaApplicationLoggingConfiguration
  where
  rnf SipMediaApplicationLoggingConfiguration' {..} =
    Prelude.rnf enableSipMediaApplicationMessageLogs

instance
  Data.ToJSON
    SipMediaApplicationLoggingConfiguration
  where
  toJSON SipMediaApplicationLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableSipMediaApplicationMessageLogs" Data..=)
              Prelude.<$> enableSipMediaApplicationMessageLogs
          ]
      )
