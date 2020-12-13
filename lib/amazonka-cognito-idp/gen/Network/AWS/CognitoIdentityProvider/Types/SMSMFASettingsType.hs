{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType
  ( SMSMFASettingsType (..),

    -- * Smart constructor
    mkSMSMFASettingsType,

    -- * Lenses
    smsmstEnabled,
    smsmstPreferredMFA,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The type used for enabling SMS MFA at the user level. Phone numbers don't need to be verified to be used for SMS MFA. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted. If you would like MFA to be applied selectively based on the assessed risk level of sign in attempts, disable MFA for users and turn on Adaptive Authentication for the user pool.
--
-- /See:/ 'mkSMSMFASettingsType' smart constructor.
data SMSMFASettingsType = SMSMFASettingsType'
  { -- | Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Specifies whether SMS is the preferred MFA method.
    preferredMFA :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMSMFASettingsType' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
-- * 'preferredMFA' - Specifies whether SMS is the preferred MFA method.
mkSMSMFASettingsType ::
  SMSMFASettingsType
mkSMSMFASettingsType =
  SMSMFASettingsType'
    { enabled = Lude.Nothing,
      preferredMFA = Lude.Nothing
    }

-- | Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmstEnabled :: Lens.Lens' SMSMFASettingsType (Lude.Maybe Lude.Bool)
smsmstEnabled = Lens.lens (enabled :: SMSMFASettingsType -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SMSMFASettingsType)
{-# DEPRECATED smsmstEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether SMS is the preferred MFA method.
--
-- /Note:/ Consider using 'preferredMFA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmstPreferredMFA :: Lens.Lens' SMSMFASettingsType (Lude.Maybe Lude.Bool)
smsmstPreferredMFA = Lens.lens (preferredMFA :: SMSMFASettingsType -> Lude.Maybe Lude.Bool) (\s a -> s {preferredMFA = a} :: SMSMFASettingsType)
{-# DEPRECATED smsmstPreferredMFA "Use generic-lens or generic-optics with 'preferredMFA' instead." #-}

instance Lude.ToJSON SMSMFASettingsType where
  toJSON SMSMFASettingsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("PreferredMfa" Lude..=) Lude.<$> preferredMFA
          ]
      )
