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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthForgotPasswordConfig where

import Amazonka.AmplifyBackend.Types.DeliveryMethod
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.SmsSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __(DEPRECATED)__ Describes the forgot password policy for authenticating
-- into the Amplify app.
--
-- /See:/ 'newCreateBackendAuthForgotPasswordConfig' smart constructor.
data CreateBackendAuthForgotPasswordConfig = CreateBackendAuthForgotPasswordConfig'
  { -- | __(DEPRECATED)__ The configuration for the email sent when an app user
    -- forgets their password.
    emailSettings :: Prelude.Maybe EmailSettings,
    -- | __(DEPRECATED)__ The configuration for the SMS message sent when an app
    -- user forgets their password.
    smsSettings :: Prelude.Maybe SmsSettings,
    -- | __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
    -- deliver messages to app users who want to recover their password.
    deliveryMethod :: DeliveryMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthForgotPasswordConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSettings', 'createBackendAuthForgotPasswordConfig_emailSettings' - __(DEPRECATED)__ The configuration for the email sent when an app user
-- forgets their password.
--
-- 'smsSettings', 'createBackendAuthForgotPasswordConfig_smsSettings' - __(DEPRECATED)__ The configuration for the SMS message sent when an app
-- user forgets their password.
--
-- 'deliveryMethod', 'createBackendAuthForgotPasswordConfig_deliveryMethod' - __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
-- deliver messages to app users who want to recover their password.
newCreateBackendAuthForgotPasswordConfig ::
  -- | 'deliveryMethod'
  DeliveryMethod ->
  CreateBackendAuthForgotPasswordConfig
newCreateBackendAuthForgotPasswordConfig
  pDeliveryMethod_ =
    CreateBackendAuthForgotPasswordConfig'
      { emailSettings =
          Prelude.Nothing,
        smsSettings = Prelude.Nothing,
        deliveryMethod = pDeliveryMethod_
      }

-- | __(DEPRECATED)__ The configuration for the email sent when an app user
-- forgets their password.
createBackendAuthForgotPasswordConfig_emailSettings :: Lens.Lens' CreateBackendAuthForgotPasswordConfig (Prelude.Maybe EmailSettings)
createBackendAuthForgotPasswordConfig_emailSettings = Lens.lens (\CreateBackendAuthForgotPasswordConfig' {emailSettings} -> emailSettings) (\s@CreateBackendAuthForgotPasswordConfig' {} a -> s {emailSettings = a} :: CreateBackendAuthForgotPasswordConfig)

-- | __(DEPRECATED)__ The configuration for the SMS message sent when an app
-- user forgets their password.
createBackendAuthForgotPasswordConfig_smsSettings :: Lens.Lens' CreateBackendAuthForgotPasswordConfig (Prelude.Maybe SmsSettings)
createBackendAuthForgotPasswordConfig_smsSettings = Lens.lens (\CreateBackendAuthForgotPasswordConfig' {smsSettings} -> smsSettings) (\s@CreateBackendAuthForgotPasswordConfig' {} a -> s {smsSettings = a} :: CreateBackendAuthForgotPasswordConfig)

-- | __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
-- deliver messages to app users who want to recover their password.
createBackendAuthForgotPasswordConfig_deliveryMethod :: Lens.Lens' CreateBackendAuthForgotPasswordConfig DeliveryMethod
createBackendAuthForgotPasswordConfig_deliveryMethod = Lens.lens (\CreateBackendAuthForgotPasswordConfig' {deliveryMethod} -> deliveryMethod) (\s@CreateBackendAuthForgotPasswordConfig' {} a -> s {deliveryMethod = a} :: CreateBackendAuthForgotPasswordConfig)

instance
  Data.FromJSON
    CreateBackendAuthForgotPasswordConfig
  where
  parseJSON =
    Data.withObject
      "CreateBackendAuthForgotPasswordConfig"
      ( \x ->
          CreateBackendAuthForgotPasswordConfig'
            Prelude.<$> (x Data..:? "emailSettings")
            Prelude.<*> (x Data..:? "smsSettings")
            Prelude.<*> (x Data..: "deliveryMethod")
      )

instance
  Prelude.Hashable
    CreateBackendAuthForgotPasswordConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthForgotPasswordConfig' {..} =
      _salt
        `Prelude.hashWithSalt` emailSettings
        `Prelude.hashWithSalt` smsSettings
        `Prelude.hashWithSalt` deliveryMethod

instance
  Prelude.NFData
    CreateBackendAuthForgotPasswordConfig
  where
  rnf CreateBackendAuthForgotPasswordConfig' {..} =
    Prelude.rnf emailSettings
      `Prelude.seq` Prelude.rnf smsSettings
      `Prelude.seq` Prelude.rnf deliveryMethod

instance
  Data.ToJSON
    CreateBackendAuthForgotPasswordConfig
  where
  toJSON CreateBackendAuthForgotPasswordConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("emailSettings" Data..=) Prelude.<$> emailSettings,
            ("smsSettings" Data..=) Prelude.<$> smsSettings,
            Prelude.Just
              ("deliveryMethod" Data..= deliveryMethod)
          ]
      )
