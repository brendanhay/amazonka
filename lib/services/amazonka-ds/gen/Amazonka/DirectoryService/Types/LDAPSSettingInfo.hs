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
-- Module      : Amazonka.DirectoryService.Types.LDAPSSettingInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.LDAPSSettingInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.LDAPSStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains general information about the LDAPS settings.
--
-- /See:/ 'newLDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { -- | The state of the LDAPS settings.
    lDAPSStatus :: Prelude.Maybe LDAPSStatus,
    -- | Describes a state change for LDAPS.
    lDAPSStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the LDAPS settings were last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LDAPSSettingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lDAPSStatus', 'lDAPSSettingInfo_lDAPSStatus' - The state of the LDAPS settings.
--
-- 'lDAPSStatusReason', 'lDAPSSettingInfo_lDAPSStatusReason' - Describes a state change for LDAPS.
--
-- 'lastUpdatedDateTime', 'lDAPSSettingInfo_lastUpdatedDateTime' - The date and time when the LDAPS settings were last updated.
newLDAPSSettingInfo ::
  LDAPSSettingInfo
newLDAPSSettingInfo =
  LDAPSSettingInfo'
    { lDAPSStatus = Prelude.Nothing,
      lDAPSStatusReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The state of the LDAPS settings.
lDAPSSettingInfo_lDAPSStatus :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe LDAPSStatus)
lDAPSSettingInfo_lDAPSStatus = Lens.lens (\LDAPSSettingInfo' {lDAPSStatus} -> lDAPSStatus) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatus = a} :: LDAPSSettingInfo)

-- | Describes a state change for LDAPS.
lDAPSSettingInfo_lDAPSStatusReason :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe Prelude.Text)
lDAPSSettingInfo_lDAPSStatusReason = Lens.lens (\LDAPSSettingInfo' {lDAPSStatusReason} -> lDAPSStatusReason) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatusReason = a} :: LDAPSSettingInfo)

-- | The date and time when the LDAPS settings were last updated.
lDAPSSettingInfo_lastUpdatedDateTime :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe Prelude.UTCTime)
lDAPSSettingInfo_lastUpdatedDateTime = Lens.lens (\LDAPSSettingInfo' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@LDAPSSettingInfo' {} a -> s {lastUpdatedDateTime = a} :: LDAPSSettingInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LDAPSSettingInfo where
  parseJSON =
    Data.withObject
      "LDAPSSettingInfo"
      ( \x ->
          LDAPSSettingInfo'
            Prelude.<$> (x Data..:? "LDAPSStatus")
            Prelude.<*> (x Data..:? "LDAPSStatusReason")
            Prelude.<*> (x Data..:? "LastUpdatedDateTime")
      )

instance Prelude.Hashable LDAPSSettingInfo where
  hashWithSalt _salt LDAPSSettingInfo' {..} =
    _salt
      `Prelude.hashWithSalt` lDAPSStatus
      `Prelude.hashWithSalt` lDAPSStatusReason
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData LDAPSSettingInfo where
  rnf LDAPSSettingInfo' {..} =
    Prelude.rnf lDAPSStatus
      `Prelude.seq` Prelude.rnf lDAPSStatusReason
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
