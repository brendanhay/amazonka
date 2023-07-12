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
-- Module      : Amazonka.AmplifyBackend.Types.UpdateBackendAuthVerificationMessageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.UpdateBackendAuthVerificationMessageConfig where

import Amazonka.AmplifyBackend.Types.DeliveryMethod
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.SmsSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates the configuration of the email or SMS message for the auth
-- resource configured for your Amplify project.
--
-- /See:/ 'newUpdateBackendAuthVerificationMessageConfig' smart constructor.
data UpdateBackendAuthVerificationMessageConfig = UpdateBackendAuthVerificationMessageConfig'
  { -- | The settings for the email message.
    emailSettings :: Prelude.Maybe EmailSettings,
    -- | The settings for the SMS message.
    smsSettings :: Prelude.Maybe SmsSettings,
    -- | The type of verification message to send.
    deliveryMethod :: DeliveryMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackendAuthVerificationMessageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSettings', 'updateBackendAuthVerificationMessageConfig_emailSettings' - The settings for the email message.
--
-- 'smsSettings', 'updateBackendAuthVerificationMessageConfig_smsSettings' - The settings for the SMS message.
--
-- 'deliveryMethod', 'updateBackendAuthVerificationMessageConfig_deliveryMethod' - The type of verification message to send.
newUpdateBackendAuthVerificationMessageConfig ::
  -- | 'deliveryMethod'
  DeliveryMethod ->
  UpdateBackendAuthVerificationMessageConfig
newUpdateBackendAuthVerificationMessageConfig
  pDeliveryMethod_ =
    UpdateBackendAuthVerificationMessageConfig'
      { emailSettings =
          Prelude.Nothing,
        smsSettings = Prelude.Nothing,
        deliveryMethod =
          pDeliveryMethod_
      }

-- | The settings for the email message.
updateBackendAuthVerificationMessageConfig_emailSettings :: Lens.Lens' UpdateBackendAuthVerificationMessageConfig (Prelude.Maybe EmailSettings)
updateBackendAuthVerificationMessageConfig_emailSettings = Lens.lens (\UpdateBackendAuthVerificationMessageConfig' {emailSettings} -> emailSettings) (\s@UpdateBackendAuthVerificationMessageConfig' {} a -> s {emailSettings = a} :: UpdateBackendAuthVerificationMessageConfig)

-- | The settings for the SMS message.
updateBackendAuthVerificationMessageConfig_smsSettings :: Lens.Lens' UpdateBackendAuthVerificationMessageConfig (Prelude.Maybe SmsSettings)
updateBackendAuthVerificationMessageConfig_smsSettings = Lens.lens (\UpdateBackendAuthVerificationMessageConfig' {smsSettings} -> smsSettings) (\s@UpdateBackendAuthVerificationMessageConfig' {} a -> s {smsSettings = a} :: UpdateBackendAuthVerificationMessageConfig)

-- | The type of verification message to send.
updateBackendAuthVerificationMessageConfig_deliveryMethod :: Lens.Lens' UpdateBackendAuthVerificationMessageConfig DeliveryMethod
updateBackendAuthVerificationMessageConfig_deliveryMethod = Lens.lens (\UpdateBackendAuthVerificationMessageConfig' {deliveryMethod} -> deliveryMethod) (\s@UpdateBackendAuthVerificationMessageConfig' {} a -> s {deliveryMethod = a} :: UpdateBackendAuthVerificationMessageConfig)

instance
  Prelude.Hashable
    UpdateBackendAuthVerificationMessageConfig
  where
  hashWithSalt
    _salt
    UpdateBackendAuthVerificationMessageConfig' {..} =
      _salt
        `Prelude.hashWithSalt` emailSettings
        `Prelude.hashWithSalt` smsSettings
        `Prelude.hashWithSalt` deliveryMethod

instance
  Prelude.NFData
    UpdateBackendAuthVerificationMessageConfig
  where
  rnf UpdateBackendAuthVerificationMessageConfig' {..} =
    Prelude.rnf emailSettings
      `Prelude.seq` Prelude.rnf smsSettings
      `Prelude.seq` Prelude.rnf deliveryMethod

instance
  Data.ToJSON
    UpdateBackendAuthVerificationMessageConfig
  where
  toJSON
    UpdateBackendAuthVerificationMessageConfig' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("emailSettings" Data..=) Prelude.<$> emailSettings,
              ("smsSettings" Data..=) Prelude.<$> smsSettings,
              Prelude.Just
                ("deliveryMethod" Data..= deliveryMethod)
            ]
        )
