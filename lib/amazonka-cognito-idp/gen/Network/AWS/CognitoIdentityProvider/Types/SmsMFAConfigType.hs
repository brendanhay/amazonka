{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsMFAConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SmsMFAConfigType
  ( SmsMFAConfigType (..),

    -- * Smart constructor
    mkSmsMFAConfigType,

    -- * Lenses
    smctSmsAuthenticationMessage,
    smctSmsConfiguration,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SMS text message multi-factor authentication (MFA) configuration type.
--
-- /See:/ 'mkSmsMFAConfigType' smart constructor.
data SmsMFAConfigType = SmsMFAConfigType'
  { smsAuthenticationMessage ::
      Lude.Maybe Lude.Text,
    smsConfiguration :: Lude.Maybe SmsConfigurationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SmsMFAConfigType' with the minimum fields required to make a request.
--
-- * 'smsAuthenticationMessage' - The SMS authentication message that will be sent to users with the code they need to sign in. The message must contain the ‘{####}’ placeholder, which will be replaced with the code. If the message is not included, and default message will be used.
-- * 'smsConfiguration' - The SMS configuration.
mkSmsMFAConfigType ::
  SmsMFAConfigType
mkSmsMFAConfigType =
  SmsMFAConfigType'
    { smsAuthenticationMessage = Lude.Nothing,
      smsConfiguration = Lude.Nothing
    }

-- | The SMS authentication message that will be sent to users with the code they need to sign in. The message must contain the ‘{####}’ placeholder, which will be replaced with the code. If the message is not included, and default message will be used.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smctSmsAuthenticationMessage :: Lens.Lens' SmsMFAConfigType (Lude.Maybe Lude.Text)
smctSmsAuthenticationMessage = Lens.lens (smsAuthenticationMessage :: SmsMFAConfigType -> Lude.Maybe Lude.Text) (\s a -> s {smsAuthenticationMessage = a} :: SmsMFAConfigType)
{-# DEPRECATED smctSmsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead." #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smctSmsConfiguration :: Lens.Lens' SmsMFAConfigType (Lude.Maybe SmsConfigurationType)
smctSmsConfiguration = Lens.lens (smsConfiguration :: SmsMFAConfigType -> Lude.Maybe SmsConfigurationType) (\s a -> s {smsConfiguration = a} :: SmsMFAConfigType)
{-# DEPRECATED smctSmsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead." #-}

instance Lude.FromJSON SmsMFAConfigType where
  parseJSON =
    Lude.withObject
      "SmsMFAConfigType"
      ( \x ->
          SmsMFAConfigType'
            Lude.<$> (x Lude..:? "SmsAuthenticationMessage")
            Lude.<*> (x Lude..:? "SmsConfiguration")
      )

instance Lude.ToJSON SmsMFAConfigType where
  toJSON SmsMFAConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SmsAuthenticationMessage" Lude..=)
              Lude.<$> smsAuthenticationMessage,
            ("SmsConfiguration" Lude..=) Lude.<$> smsConfiguration
          ]
      )
