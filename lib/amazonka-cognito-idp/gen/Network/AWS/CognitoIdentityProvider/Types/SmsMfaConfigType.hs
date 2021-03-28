{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
  ( SmsMfaConfigType (..)
  -- * Smart constructor
  , mkSmsMfaConfigType
  -- * Lenses
  , smctSmsAuthenticationMessage
  , smctSmsConfiguration
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsVerificationMessageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SMS text message multi-factor authentication (MFA) configuration type.
--
-- /See:/ 'mkSmsMfaConfigType' smart constructor.
data SmsMfaConfigType = SmsMfaConfigType'
  { smsAuthenticationMessage :: Core.Maybe Types.SmsVerificationMessageType
    -- ^ The SMS authentication message that will be sent to users with the code they need to sign in. The message must contain the ‘{####}’ placeholder, which will be replaced with the code. If the message is not included, and default message will be used.
  , smsConfiguration :: Core.Maybe Types.SmsConfigurationType
    -- ^ The SMS configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SmsMfaConfigType' value with any optional fields omitted.
mkSmsMfaConfigType
    :: SmsMfaConfigType
mkSmsMfaConfigType
  = SmsMfaConfigType'{smsAuthenticationMessage = Core.Nothing,
                      smsConfiguration = Core.Nothing}

-- | The SMS authentication message that will be sent to users with the code they need to sign in. The message must contain the ‘{####}’ placeholder, which will be replaced with the code. If the message is not included, and default message will be used.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smctSmsAuthenticationMessage :: Lens.Lens' SmsMfaConfigType (Core.Maybe Types.SmsVerificationMessageType)
smctSmsAuthenticationMessage = Lens.field @"smsAuthenticationMessage"
{-# INLINEABLE smctSmsAuthenticationMessage #-}
{-# DEPRECATED smsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead"  #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smctSmsConfiguration :: Lens.Lens' SmsMfaConfigType (Core.Maybe Types.SmsConfigurationType)
smctSmsConfiguration = Lens.field @"smsConfiguration"
{-# INLINEABLE smctSmsConfiguration #-}
{-# DEPRECATED smsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead"  #-}

instance Core.FromJSON SmsMfaConfigType where
        toJSON SmsMfaConfigType{..}
          = Core.object
              (Core.catMaybes
                 [("SmsAuthenticationMessage" Core..=) Core.<$>
                    smsAuthenticationMessage,
                  ("SmsConfiguration" Core..=) Core.<$> smsConfiguration])

instance Core.FromJSON SmsMfaConfigType where
        parseJSON
          = Core.withObject "SmsMfaConfigType" Core.$
              \ x ->
                SmsMfaConfigType' Core.<$>
                  (x Core..:? "SmsAuthenticationMessage") Core.<*>
                    x Core..:? "SmsConfiguration"
