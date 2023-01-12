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
-- Module      : Amazonka.DirectoryService.Types.ClientAuthenticationSettingInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.ClientAuthenticationSettingInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.ClientAuthenticationStatus
import Amazonka.DirectoryService.Types.ClientAuthenticationType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a client authentication method for a
-- directory.
--
-- /See:/ 'newClientAuthenticationSettingInfo' smart constructor.
data ClientAuthenticationSettingInfo = ClientAuthenticationSettingInfo'
  { -- | The date and time when the status of the client authentication type was
    -- last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Whether the client authentication type is enabled or disabled for the
    -- specified directory.
    status :: Prelude.Maybe ClientAuthenticationStatus,
    -- | The type of client authentication for the specified directory. If no
    -- type is specified, a list of all client authentication types that are
    -- supported for the directory is retrieved.
    type' :: Prelude.Maybe ClientAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientAuthenticationSettingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDateTime', 'clientAuthenticationSettingInfo_lastUpdatedDateTime' - The date and time when the status of the client authentication type was
-- last updated.
--
-- 'status', 'clientAuthenticationSettingInfo_status' - Whether the client authentication type is enabled or disabled for the
-- specified directory.
--
-- 'type'', 'clientAuthenticationSettingInfo_type' - The type of client authentication for the specified directory. If no
-- type is specified, a list of all client authentication types that are
-- supported for the directory is retrieved.
newClientAuthenticationSettingInfo ::
  ClientAuthenticationSettingInfo
newClientAuthenticationSettingInfo =
  ClientAuthenticationSettingInfo'
    { lastUpdatedDateTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date and time when the status of the client authentication type was
-- last updated.
clientAuthenticationSettingInfo_lastUpdatedDateTime :: Lens.Lens' ClientAuthenticationSettingInfo (Prelude.Maybe Prelude.UTCTime)
clientAuthenticationSettingInfo_lastUpdatedDateTime = Lens.lens (\ClientAuthenticationSettingInfo' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@ClientAuthenticationSettingInfo' {} a -> s {lastUpdatedDateTime = a} :: ClientAuthenticationSettingInfo) Prelude.. Lens.mapping Data._Time

-- | Whether the client authentication type is enabled or disabled for the
-- specified directory.
clientAuthenticationSettingInfo_status :: Lens.Lens' ClientAuthenticationSettingInfo (Prelude.Maybe ClientAuthenticationStatus)
clientAuthenticationSettingInfo_status = Lens.lens (\ClientAuthenticationSettingInfo' {status} -> status) (\s@ClientAuthenticationSettingInfo' {} a -> s {status = a} :: ClientAuthenticationSettingInfo)

-- | The type of client authentication for the specified directory. If no
-- type is specified, a list of all client authentication types that are
-- supported for the directory is retrieved.
clientAuthenticationSettingInfo_type :: Lens.Lens' ClientAuthenticationSettingInfo (Prelude.Maybe ClientAuthenticationType)
clientAuthenticationSettingInfo_type = Lens.lens (\ClientAuthenticationSettingInfo' {type'} -> type') (\s@ClientAuthenticationSettingInfo' {} a -> s {type' = a} :: ClientAuthenticationSettingInfo)

instance
  Data.FromJSON
    ClientAuthenticationSettingInfo
  where
  parseJSON =
    Data.withObject
      "ClientAuthenticationSettingInfo"
      ( \x ->
          ClientAuthenticationSettingInfo'
            Prelude.<$> (x Data..:? "LastUpdatedDateTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    ClientAuthenticationSettingInfo
  where
  hashWithSalt
    _salt
    ClientAuthenticationSettingInfo' {..} =
      _salt `Prelude.hashWithSalt` lastUpdatedDateTime
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    ClientAuthenticationSettingInfo
  where
  rnf ClientAuthenticationSettingInfo' {..} =
    Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
