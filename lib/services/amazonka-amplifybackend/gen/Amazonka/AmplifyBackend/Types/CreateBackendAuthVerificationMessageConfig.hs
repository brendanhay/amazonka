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
-- Module      : Amazonka.AmplifyBackend.Types.CreateBackendAuthVerificationMessageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.CreateBackendAuthVerificationMessageConfig where

import Amazonka.AmplifyBackend.Types.DeliveryMethod
import Amazonka.AmplifyBackend.Types.EmailSettings
import Amazonka.AmplifyBackend.Types.SmsSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Creates an email or SMS verification message for the auth resource
-- configured for your Amplify project.
--
-- /See:/ 'newCreateBackendAuthVerificationMessageConfig' smart constructor.
data CreateBackendAuthVerificationMessageConfig = CreateBackendAuthVerificationMessageConfig'
  { -- | The settings for the email message.
    emailSettings :: Prelude.Maybe EmailSettings,
    -- | The settings for the SMS message.
    smsSettings :: Prelude.Maybe SmsSettings,
    -- | The type of verification message to send.
    deliveryMethod :: DeliveryMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackendAuthVerificationMessageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSettings', 'createBackendAuthVerificationMessageConfig_emailSettings' - The settings for the email message.
--
-- 'smsSettings', 'createBackendAuthVerificationMessageConfig_smsSettings' - The settings for the SMS message.
--
-- 'deliveryMethod', 'createBackendAuthVerificationMessageConfig_deliveryMethod' - The type of verification message to send.
newCreateBackendAuthVerificationMessageConfig ::
  -- | 'deliveryMethod'
  DeliveryMethod ->
  CreateBackendAuthVerificationMessageConfig
newCreateBackendAuthVerificationMessageConfig
  pDeliveryMethod_ =
    CreateBackendAuthVerificationMessageConfig'
      { emailSettings =
          Prelude.Nothing,
        smsSettings = Prelude.Nothing,
        deliveryMethod =
          pDeliveryMethod_
      }

-- | The settings for the email message.
createBackendAuthVerificationMessageConfig_emailSettings :: Lens.Lens' CreateBackendAuthVerificationMessageConfig (Prelude.Maybe EmailSettings)
createBackendAuthVerificationMessageConfig_emailSettings = Lens.lens (\CreateBackendAuthVerificationMessageConfig' {emailSettings} -> emailSettings) (\s@CreateBackendAuthVerificationMessageConfig' {} a -> s {emailSettings = a} :: CreateBackendAuthVerificationMessageConfig)

-- | The settings for the SMS message.
createBackendAuthVerificationMessageConfig_smsSettings :: Lens.Lens' CreateBackendAuthVerificationMessageConfig (Prelude.Maybe SmsSettings)
createBackendAuthVerificationMessageConfig_smsSettings = Lens.lens (\CreateBackendAuthVerificationMessageConfig' {smsSettings} -> smsSettings) (\s@CreateBackendAuthVerificationMessageConfig' {} a -> s {smsSettings = a} :: CreateBackendAuthVerificationMessageConfig)

-- | The type of verification message to send.
createBackendAuthVerificationMessageConfig_deliveryMethod :: Lens.Lens' CreateBackendAuthVerificationMessageConfig DeliveryMethod
createBackendAuthVerificationMessageConfig_deliveryMethod = Lens.lens (\CreateBackendAuthVerificationMessageConfig' {deliveryMethod} -> deliveryMethod) (\s@CreateBackendAuthVerificationMessageConfig' {} a -> s {deliveryMethod = a} :: CreateBackendAuthVerificationMessageConfig)

instance
  Data.FromJSON
    CreateBackendAuthVerificationMessageConfig
  where
  parseJSON =
    Data.withObject
      "CreateBackendAuthVerificationMessageConfig"
      ( \x ->
          CreateBackendAuthVerificationMessageConfig'
            Prelude.<$> (x Data..:? "emailSettings")
            Prelude.<*> (x Data..:? "smsSettings")
            Prelude.<*> (x Data..: "deliveryMethod")
      )

instance
  Prelude.Hashable
    CreateBackendAuthVerificationMessageConfig
  where
  hashWithSalt
    _salt
    CreateBackendAuthVerificationMessageConfig' {..} =
      _salt
        `Prelude.hashWithSalt` emailSettings
        `Prelude.hashWithSalt` smsSettings
        `Prelude.hashWithSalt` deliveryMethod

instance
  Prelude.NFData
    CreateBackendAuthVerificationMessageConfig
  where
  rnf CreateBackendAuthVerificationMessageConfig' {..} =
    Prelude.rnf emailSettings
      `Prelude.seq` Prelude.rnf smsSettings
      `Prelude.seq` Prelude.rnf deliveryMethod

instance
  Data.ToJSON
    CreateBackendAuthVerificationMessageConfig
  where
  toJSON
    CreateBackendAuthVerificationMessageConfig' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("emailSettings" Data..=) Prelude.<$> emailSettings,
              ("smsSettings" Data..=) Prelude.<$> smsSettings,
              Prelude.Just
                ("deliveryMethod" Data..= deliveryMethod)
            ]
        )
