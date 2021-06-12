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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.LDAPSStatus
import qualified Network.AWS.Lens as Lens

-- | Contains general information about the LDAPS settings.
--
-- /See:/ 'newLDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { -- | The date and time when the LDAPS settings were last updated.
    lastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | Describes a state change for LDAPS.
    lDAPSStatusReason :: Core.Maybe Core.Text,
    -- | The state of the LDAPS settings.
    lDAPSStatus :: Core.Maybe LDAPSStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      lDAPSStatusReason = Core.Nothing,
      lDAPSStatus = Core.Nothing
    }

-- | The date and time when the LDAPS settings were last updated.
lDAPSSettingInfo_lastUpdatedDateTime :: Lens.Lens' LDAPSSettingInfo (Core.Maybe Core.UTCTime)
lDAPSSettingInfo_lastUpdatedDateTime = Lens.lens (\LDAPSSettingInfo' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@LDAPSSettingInfo' {} a -> s {lastUpdatedDateTime = a} :: LDAPSSettingInfo) Core.. Lens.mapping Core._Time

-- | Describes a state change for LDAPS.
lDAPSSettingInfo_lDAPSStatusReason :: Lens.Lens' LDAPSSettingInfo (Core.Maybe Core.Text)
lDAPSSettingInfo_lDAPSStatusReason = Lens.lens (\LDAPSSettingInfo' {lDAPSStatusReason} -> lDAPSStatusReason) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatusReason = a} :: LDAPSSettingInfo)

-- | The state of the LDAPS settings.
lDAPSSettingInfo_lDAPSStatus :: Lens.Lens' LDAPSSettingInfo (Core.Maybe LDAPSStatus)
lDAPSSettingInfo_lDAPSStatus = Lens.lens (\LDAPSSettingInfo' {lDAPSStatus} -> lDAPSStatus) (\s@LDAPSSettingInfo' {} a -> s {lDAPSStatus = a} :: LDAPSSettingInfo)

instance Core.FromJSON LDAPSSettingInfo where
  parseJSON =
    Core.withObject
      "LDAPSSettingInfo"
      ( \x ->
          LDAPSSettingInfo'
            Core.<$> (x Core..:? "LastUpdatedDateTime")
            Core.<*> (x Core..:? "LDAPSStatusReason")
            Core.<*> (x Core..:? "LDAPSStatus")
      )

instance Core.Hashable LDAPSSettingInfo

instance Core.NFData LDAPSSettingInfo
