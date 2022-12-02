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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthForgotPasswordConfig where

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
-- /See:/ 'newUpdateBackendAuthForgotPasswordConfig' smart constructor.
data UpdateBackendAuthForgotPasswordConfig = UpdateBackendAuthForgotPasswordConfig'
  { -- | __(DEPRECATED)__ The configuration for the email sent when an app user
    -- forgets their password.
    emailSettings :: Prelude.Maybe EmailSettings,
    -- | __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
    -- deliver messages to app users that want to recover their password.
    deliveryMethod :: Prelude.Maybe DeliveryMethod,
    -- | __(DEPRECATED)__ The configuration for the SMS message sent when an
    -- Amplify app user forgets their password.
    smsSettings :: Prelude.Maybe SmsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthForgotPasswordConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSettings', 'updateBackendAuthForgotPasswordConfig_emailSettings' - __(DEPRECATED)__ The configuration for the email sent when an app user
-- forgets their password.
--
-- 'deliveryMethod', 'updateBackendAuthForgotPasswordConfig_deliveryMethod' - __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
-- deliver messages to app users that want to recover their password.
--
-- 'smsSettings', 'updateBackendAuthForgotPasswordConfig_smsSettings' - __(DEPRECATED)__ The configuration for the SMS message sent when an
-- Amplify app user forgets their password.
newUpdateBackendAuthForgotPasswordConfig ::
  UpdateBackendAuthForgotPasswordConfig
newUpdateBackendAuthForgotPasswordConfig =
  UpdateBackendAuthForgotPasswordConfig'
    { emailSettings =
        Prelude.Nothing,
      deliveryMethod = Prelude.Nothing,
      smsSettings = Prelude.Nothing
    }

-- | __(DEPRECATED)__ The configuration for the email sent when an app user
-- forgets their password.
updateBackendAuthForgotPasswordConfig_emailSettings :: Lens.Lens' UpdateBackendAuthForgotPasswordConfig (Prelude.Maybe EmailSettings)
updateBackendAuthForgotPasswordConfig_emailSettings = Lens.lens (\UpdateBackendAuthForgotPasswordConfig' {emailSettings} -> emailSettings) (\s@UpdateBackendAuthForgotPasswordConfig' {} a -> s {emailSettings = a} :: UpdateBackendAuthForgotPasswordConfig)

-- | __(DEPRECATED)__ Describes which mode to use (either SMS or email) to
-- deliver messages to app users that want to recover their password.
updateBackendAuthForgotPasswordConfig_deliveryMethod :: Lens.Lens' UpdateBackendAuthForgotPasswordConfig (Prelude.Maybe DeliveryMethod)
updateBackendAuthForgotPasswordConfig_deliveryMethod = Lens.lens (\UpdateBackendAuthForgotPasswordConfig' {deliveryMethod} -> deliveryMethod) (\s@UpdateBackendAuthForgotPasswordConfig' {} a -> s {deliveryMethod = a} :: UpdateBackendAuthForgotPasswordConfig)

-- | __(DEPRECATED)__ The configuration for the SMS message sent when an
-- Amplify app user forgets their password.
updateBackendAuthForgotPasswordConfig_smsSettings :: Lens.Lens' UpdateBackendAuthForgotPasswordConfig (Prelude.Maybe SmsSettings)
updateBackendAuthForgotPasswordConfig_smsSettings = Lens.lens (\UpdateBackendAuthForgotPasswordConfig' {smsSettings} -> smsSettings) (\s@UpdateBackendAuthForgotPasswordConfig' {} a -> s {smsSettings = a} :: UpdateBackendAuthForgotPasswordConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthForgotPasswordConfig
  where
  hashWithSalt
    _salt
    UpdateBackendAuthForgotPasswordConfig' {..} =
      _salt `Prelude.hashWithSalt` emailSettings
        `Prelude.hashWithSalt` deliveryMethod
        `Prelude.hashWithSalt` smsSettings

instance
  Prelude.NFData
    UpdateBackendAuthForgotPasswordConfig
  where
  rnf UpdateBackendAuthForgotPasswordConfig' {..} =
    Prelude.rnf emailSettings
      `Prelude.seq` Prelude.rnf deliveryMethod
      `Prelude.seq` Prelude.rnf smsSettings

instance
  Data.ToJSON
    UpdateBackendAuthForgotPasswordConfig
  where
  toJSON UpdateBackendAuthForgotPasswordConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("emailSettings" Data..=) Prelude.<$> emailSettings,
            ("deliveryMethod" Data..=)
              Prelude.<$> deliveryMethod,
            ("smsSettings" Data..=) Prelude.<$> smsSettings
          ]
      )
