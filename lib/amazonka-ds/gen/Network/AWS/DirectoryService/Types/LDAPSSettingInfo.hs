{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSSettingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSSettingInfo
  ( LDAPSSettingInfo (..),

    -- * Smart constructor
    mkLDAPSSettingInfo,

    -- * Lenses
    ldapssiLDAPSStatus,
    ldapssiLDAPSStatusReason,
    ldapssiLastUpdatedDateTime,
  )
where

import qualified Network.AWS.DirectoryService.Types.LDAPSStatus as Types
import qualified Network.AWS.DirectoryService.Types.LDAPSStatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains general information about the LDAPS settings.
--
-- /See:/ 'mkLDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { -- | The state of the LDAPS settings.
    lDAPSStatus :: Core.Maybe Types.LDAPSStatus,
    -- | Describes a state change for LDAPS.
    lDAPSStatusReason :: Core.Maybe Types.LDAPSStatusReason,
    -- | The date and time when the LDAPS settings were last updated.
    lastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LDAPSSettingInfo' value with any optional fields omitted.
mkLDAPSSettingInfo ::
  LDAPSSettingInfo
mkLDAPSSettingInfo =
  LDAPSSettingInfo'
    { lDAPSStatus = Core.Nothing,
      lDAPSStatusReason = Core.Nothing,
      lastUpdatedDateTime = Core.Nothing
    }

-- | The state of the LDAPS settings.
--
-- /Note:/ Consider using 'lDAPSStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLDAPSStatus :: Lens.Lens' LDAPSSettingInfo (Core.Maybe Types.LDAPSStatus)
ldapssiLDAPSStatus = Lens.field @"lDAPSStatus"
{-# DEPRECATED ldapssiLDAPSStatus "Use generic-lens or generic-optics with 'lDAPSStatus' instead." #-}

-- | Describes a state change for LDAPS.
--
-- /Note:/ Consider using 'lDAPSStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLDAPSStatusReason :: Lens.Lens' LDAPSSettingInfo (Core.Maybe Types.LDAPSStatusReason)
ldapssiLDAPSStatusReason = Lens.field @"lDAPSStatusReason"
{-# DEPRECATED ldapssiLDAPSStatusReason "Use generic-lens or generic-optics with 'lDAPSStatusReason' instead." #-}

-- | The date and time when the LDAPS settings were last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLastUpdatedDateTime :: Lens.Lens' LDAPSSettingInfo (Core.Maybe Core.NominalDiffTime)
ldapssiLastUpdatedDateTime = Lens.field @"lastUpdatedDateTime"
{-# DEPRECATED ldapssiLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

instance Core.FromJSON LDAPSSettingInfo where
  parseJSON =
    Core.withObject "LDAPSSettingInfo" Core.$
      \x ->
        LDAPSSettingInfo'
          Core.<$> (x Core..:? "LDAPSStatus")
          Core.<*> (x Core..:? "LDAPSStatusReason")
          Core.<*> (x Core..:? "LastUpdatedDateTime")
