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
-- Module      : Amazonka.WorkSpaces.Types.ClientProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ClientProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.LogUploadEnum
import Amazonka.WorkSpaces.Types.ReconnectEnum

-- | Describes an Amazon WorkSpaces client.
--
-- /See:/ 'newClientProperties' smart constructor.
data ClientProperties = ClientProperties'
  { -- | Specifies whether users can cache their credentials on the Amazon
    -- WorkSpaces client. When enabled, users can choose to reconnect to their
    -- WorkSpaces without re-entering their credentials.
    reconnectEnabled :: Prelude.Maybe ReconnectEnum,
    -- | Specifies whether users can upload diagnostic log files of Amazon
    -- WorkSpaces client directly to WorkSpaces to troubleshoot issues when
    -- using the WorkSpaces client. When enabled, the log files will be sent to
    -- WorkSpaces automatically and will be applied to all users in the
    -- specified directory.
    logUploadEnabled :: Prelude.Maybe LogUploadEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reconnectEnabled', 'clientProperties_reconnectEnabled' - Specifies whether users can cache their credentials on the Amazon
-- WorkSpaces client. When enabled, users can choose to reconnect to their
-- WorkSpaces without re-entering their credentials.
--
-- 'logUploadEnabled', 'clientProperties_logUploadEnabled' - Specifies whether users can upload diagnostic log files of Amazon
-- WorkSpaces client directly to WorkSpaces to troubleshoot issues when
-- using the WorkSpaces client. When enabled, the log files will be sent to
-- WorkSpaces automatically and will be applied to all users in the
-- specified directory.
newClientProperties ::
  ClientProperties
newClientProperties =
  ClientProperties'
    { reconnectEnabled =
        Prelude.Nothing,
      logUploadEnabled = Prelude.Nothing
    }

-- | Specifies whether users can cache their credentials on the Amazon
-- WorkSpaces client. When enabled, users can choose to reconnect to their
-- WorkSpaces without re-entering their credentials.
clientProperties_reconnectEnabled :: Lens.Lens' ClientProperties (Prelude.Maybe ReconnectEnum)
clientProperties_reconnectEnabled = Lens.lens (\ClientProperties' {reconnectEnabled} -> reconnectEnabled) (\s@ClientProperties' {} a -> s {reconnectEnabled = a} :: ClientProperties)

-- | Specifies whether users can upload diagnostic log files of Amazon
-- WorkSpaces client directly to WorkSpaces to troubleshoot issues when
-- using the WorkSpaces client. When enabled, the log files will be sent to
-- WorkSpaces automatically and will be applied to all users in the
-- specified directory.
clientProperties_logUploadEnabled :: Lens.Lens' ClientProperties (Prelude.Maybe LogUploadEnum)
clientProperties_logUploadEnabled = Lens.lens (\ClientProperties' {logUploadEnabled} -> logUploadEnabled) (\s@ClientProperties' {} a -> s {logUploadEnabled = a} :: ClientProperties)

instance Data.FromJSON ClientProperties where
  parseJSON =
    Data.withObject
      "ClientProperties"
      ( \x ->
          ClientProperties'
            Prelude.<$> (x Data..:? "ReconnectEnabled")
            Prelude.<*> (x Data..:? "LogUploadEnabled")
      )

instance Prelude.Hashable ClientProperties where
  hashWithSalt _salt ClientProperties' {..} =
    _salt `Prelude.hashWithSalt` reconnectEnabled
      `Prelude.hashWithSalt` logUploadEnabled

instance Prelude.NFData ClientProperties where
  rnf ClientProperties' {..} =
    Prelude.rnf reconnectEnabled
      `Prelude.seq` Prelude.rnf logUploadEnabled

instance Data.ToJSON ClientProperties where
  toJSON ClientProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReconnectEnabled" Data..=)
              Prelude.<$> reconnectEnabled,
            ("LogUploadEnabled" Data..=)
              Prelude.<$> logUploadEnabled
          ]
      )
