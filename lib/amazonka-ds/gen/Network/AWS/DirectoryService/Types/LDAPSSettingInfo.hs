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
    ldapssiLastUpdatedDateTime,
    ldapssiLDAPSStatusReason,
    ldapssiLDAPSStatus,
  )
where

import Network.AWS.DirectoryService.Types.LDAPSStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains general information about the LDAPS settings.
--
-- /See:/ 'mkLDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { lastUpdatedDateTime ::
      Lude.Maybe Lude.Timestamp,
    lDAPSStatusReason :: Lude.Maybe Lude.Text,
    lDAPSStatus :: Lude.Maybe LDAPSStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LDAPSSettingInfo' with the minimum fields required to make a request.
--
-- * 'lDAPSStatus' - The state of the LDAPS settings.
-- * 'lDAPSStatusReason' - Describes a state change for LDAPS.
-- * 'lastUpdatedDateTime' - The date and time when the LDAPS settings were last updated.
mkLDAPSSettingInfo ::
  LDAPSSettingInfo
mkLDAPSSettingInfo =
  LDAPSSettingInfo'
    { lastUpdatedDateTime = Lude.Nothing,
      lDAPSStatusReason = Lude.Nothing,
      lDAPSStatus = Lude.Nothing
    }

-- | The date and time when the LDAPS settings were last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLastUpdatedDateTime :: Lens.Lens' LDAPSSettingInfo (Lude.Maybe Lude.Timestamp)
ldapssiLastUpdatedDateTime = Lens.lens (lastUpdatedDateTime :: LDAPSSettingInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDateTime = a} :: LDAPSSettingInfo)
{-# DEPRECATED ldapssiLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | Describes a state change for LDAPS.
--
-- /Note:/ Consider using 'lDAPSStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLDAPSStatusReason :: Lens.Lens' LDAPSSettingInfo (Lude.Maybe Lude.Text)
ldapssiLDAPSStatusReason = Lens.lens (lDAPSStatusReason :: LDAPSSettingInfo -> Lude.Maybe Lude.Text) (\s a -> s {lDAPSStatusReason = a} :: LDAPSSettingInfo)
{-# DEPRECATED ldapssiLDAPSStatusReason "Use generic-lens or generic-optics with 'lDAPSStatusReason' instead." #-}

-- | The state of the LDAPS settings.
--
-- /Note:/ Consider using 'lDAPSStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldapssiLDAPSStatus :: Lens.Lens' LDAPSSettingInfo (Lude.Maybe LDAPSStatus)
ldapssiLDAPSStatus = Lens.lens (lDAPSStatus :: LDAPSSettingInfo -> Lude.Maybe LDAPSStatus) (\s a -> s {lDAPSStatus = a} :: LDAPSSettingInfo)
{-# DEPRECATED ldapssiLDAPSStatus "Use generic-lens or generic-optics with 'lDAPSStatus' instead." #-}

instance Lude.FromJSON LDAPSSettingInfo where
  parseJSON =
    Lude.withObject
      "LDAPSSettingInfo"
      ( \x ->
          LDAPSSettingInfo'
            Lude.<$> (x Lude..:? "LastUpdatedDateTime")
            Lude.<*> (x Lude..:? "LDAPSStatusReason")
            Lude.<*> (x Lude..:? "LDAPSStatus")
      )
