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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType where

import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SMS text message multi-factor authentication (MFA) configuration
-- type.
--
-- /See:/ 'newSmsMfaConfigType' smart constructor.
data SmsMfaConfigType = SmsMfaConfigType'
  { -- | The SMS configuration.
    smsConfiguration :: Prelude.Maybe SmsConfigurationType,
    -- | The SMS authentication message that will be sent to users with the code
    -- they need to sign in. The message must contain the ‘{####}’ placeholder,
    -- which will be replaced with the code. If the message is not included,
    -- and default message will be used.
    smsAuthenticationMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SmsMfaConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'smsConfiguration', 'smsMfaConfigType_smsConfiguration' - The SMS configuration.
--
-- 'smsAuthenticationMessage', 'smsMfaConfigType_smsAuthenticationMessage' - The SMS authentication message that will be sent to users with the code
-- they need to sign in. The message must contain the ‘{####}’ placeholder,
-- which will be replaced with the code. If the message is not included,
-- and default message will be used.
newSmsMfaConfigType ::
  SmsMfaConfigType
newSmsMfaConfigType =
  SmsMfaConfigType'
    { smsConfiguration =
        Prelude.Nothing,
      smsAuthenticationMessage = Prelude.Nothing
    }

-- | The SMS configuration.
smsMfaConfigType_smsConfiguration :: Lens.Lens' SmsMfaConfigType (Prelude.Maybe SmsConfigurationType)
smsMfaConfigType_smsConfiguration = Lens.lens (\SmsMfaConfigType' {smsConfiguration} -> smsConfiguration) (\s@SmsMfaConfigType' {} a -> s {smsConfiguration = a} :: SmsMfaConfigType)

-- | The SMS authentication message that will be sent to users with the code
-- they need to sign in. The message must contain the ‘{####}’ placeholder,
-- which will be replaced with the code. If the message is not included,
-- and default message will be used.
smsMfaConfigType_smsAuthenticationMessage :: Lens.Lens' SmsMfaConfigType (Prelude.Maybe Prelude.Text)
smsMfaConfigType_smsAuthenticationMessage = Lens.lens (\SmsMfaConfigType' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@SmsMfaConfigType' {} a -> s {smsAuthenticationMessage = a} :: SmsMfaConfigType)

instance Prelude.FromJSON SmsMfaConfigType where
  parseJSON =
    Prelude.withObject
      "SmsMfaConfigType"
      ( \x ->
          SmsMfaConfigType'
            Prelude.<$> (x Prelude..:? "SmsConfiguration")
            Prelude.<*> (x Prelude..:? "SmsAuthenticationMessage")
      )

instance Prelude.Hashable SmsMfaConfigType

instance Prelude.NFData SmsMfaConfigType

instance Prelude.ToJSON SmsMfaConfigType where
  toJSON SmsMfaConfigType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SmsConfiguration" Prelude..=)
              Prelude.<$> smsConfiguration,
            ("SmsAuthenticationMessage" Prelude..=)
              Prelude.<$> smsAuthenticationMessage
          ]
      )
