{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.Types.LDAPSSettingInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSSettingInfo where

import Network.AWS.DirectoryService.Types.LDAPSStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains general information about the LDAPS settings.
--
-- /See:/ 'newLDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { -- | The date and time when the LDAPS settings were last updated.
    lastUpdatedDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Describes a state change for LDAPS.
    lDAPSStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The state of the LDAPS settings.
    lDAPSStatus :: Prelude.Maybe LDAPSStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LDAPSSettingInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedDateTime', 'lDAPSSettingInfo_lastUpdatedDateTime' - The date and time when the LDAPS settings were last updated.
--
-- 'lDAPSStatusReason', 'lDAPSSettingInfo_lDAPSStatusReason' - Describes a state change for LDAPS.
--
-- 'lDAPSStatus', 'lDAPSSettingInfo_lDAPSStatus' - The state of the LDAPS settings.
newLDAPSSettingInfo ::
  LDAPSSettingInfo
newLDAPSSettingInfo =
  LDAPSSettingInfo'
    { lastUpdatedDateTime =
        Prelude.Nothing,
      lDAPSStatusReason = Prelude.Nothing,
      lDAPSStatus = Prelude.Nothing
    }

-- | The date and time when the LDAPS settings were last updated.
lDAPSSettingInfo_lastUpdatedDateTime :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe Prelude.UTCTime)
lDAPSSettingInfo_lastUpdatedDateTime = Lens.lens (\LDAPSSettingInfo' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@LDAPSSettingInfo' {} a -> s {lastUpdatedDateTime = a} :: LDAPSSettingInfo) Prelude.. Lens.mapping Prelude._Time

-- | Describes a state change for LDAPS.
lDAPSSettingInfo_lDAPSStatusReason :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe Prelude.Text)
lDAPSSettingInfo_lDAPSStatusReason = Lens.lens (\LDAPSSettingInfo' {lDAPSStatusReason} -> lDAPSStatusReason) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatusReason = a} :: LDAPSSettingInfo)

-- | The state of the LDAPS settings.
lDAPSSettingInfo_lDAPSStatus :: Lens.Lens' LDAPSSettingInfo (Prelude.Maybe LDAPSStatus)
lDAPSSettingInfo_lDAPSStatus = Lens.lens (\LDAPSSettingInfo' {lDAPSStatus} -> lDAPSStatus) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatus = a} :: LDAPSSettingInfo)

instance Prelude.FromJSON LDAPSSettingInfo where
  parseJSON =
    Prelude.withObject
      "LDAPSSettingInfo"
      ( \x ->
          LDAPSSettingInfo'
            Prelude.<$> (x Prelude..:? "LastUpdatedDateTime")
            Prelude.<*> (x Prelude..:? "LDAPSStatusReason")
            Prelude.<*> (x Prelude..:? "LDAPSStatus")
      )

instance Prelude.Hashable LDAPSSettingInfo

instance Prelude.NFData LDAPSSettingInfo
