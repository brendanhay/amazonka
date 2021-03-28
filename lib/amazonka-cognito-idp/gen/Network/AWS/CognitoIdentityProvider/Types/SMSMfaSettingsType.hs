{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
  ( SMSMfaSettingsType (..)
  -- * Smart constructor
  , mkSMSMfaSettingsType
  -- * Lenses
  , smsmstEnabled
  , smsmstPreferredMfa
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The type used for enabling SMS MFA at the user level. Phone numbers don't need to be verified to be used for SMS MFA. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted. If you would like MFA to be applied selectively based on the assessed risk level of sign in attempts, disable MFA for users and turn on Adaptive Authentication for the user pool.
--
-- /See:/ 'mkSMSMfaSettingsType' smart constructor.
data SMSMfaSettingsType = SMSMfaSettingsType'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
  , preferredMfa :: Core.Maybe Core.Bool
    -- ^ Specifies whether SMS is the preferred MFA method.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMSMfaSettingsType' value with any optional fields omitted.
mkSMSMfaSettingsType
    :: SMSMfaSettingsType
mkSMSMfaSettingsType
  = SMSMfaSettingsType'{enabled = Core.Nothing,
                        preferredMfa = Core.Nothing}

-- | Specifies whether SMS text message MFA is enabled. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmstEnabled :: Lens.Lens' SMSMfaSettingsType (Core.Maybe Core.Bool)
smsmstEnabled = Lens.field @"enabled"
{-# INLINEABLE smsmstEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Specifies whether SMS is the preferred MFA method.
--
-- /Note:/ Consider using 'preferredMfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smsmstPreferredMfa :: Lens.Lens' SMSMfaSettingsType (Core.Maybe Core.Bool)
smsmstPreferredMfa = Lens.field @"preferredMfa"
{-# INLINEABLE smsmstPreferredMfa #-}
{-# DEPRECATED preferredMfa "Use generic-lens or generic-optics with 'preferredMfa' instead"  #-}

instance Core.FromJSON SMSMfaSettingsType where
        toJSON SMSMfaSettingsType{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("PreferredMfa" Core..=) Core.<$> preferredMfa])
