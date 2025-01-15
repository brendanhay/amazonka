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
-- Module      : Amazonka.LexV2Models.Types.EncryptionSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.EncryptionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The object representing the passwords that were used to encrypt the data
-- related to the bot recommendation, as well as the KMS key ARN used to
-- encrypt the associated metadata.
--
-- /See:/ 'newEncryptionSetting' smart constructor.
data EncryptionSetting = EncryptionSetting'
  { -- | The password used to encrypt the associated transcript file.
    associatedTranscriptsPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The password used to encrypt the recommended bot recommendation file.
    botLocaleExportPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The KMS key ARN used to encrypt the metadata associated with the bot
    -- recommendation.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedTranscriptsPassword', 'encryptionSetting_associatedTranscriptsPassword' - The password used to encrypt the associated transcript file.
--
-- 'botLocaleExportPassword', 'encryptionSetting_botLocaleExportPassword' - The password used to encrypt the recommended bot recommendation file.
--
-- 'kmsKeyArn', 'encryptionSetting_kmsKeyArn' - The KMS key ARN used to encrypt the metadata associated with the bot
-- recommendation.
newEncryptionSetting ::
  EncryptionSetting
newEncryptionSetting =
  EncryptionSetting'
    { associatedTranscriptsPassword =
        Prelude.Nothing,
      botLocaleExportPassword = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | The password used to encrypt the associated transcript file.
encryptionSetting_associatedTranscriptsPassword :: Lens.Lens' EncryptionSetting (Prelude.Maybe Prelude.Text)
encryptionSetting_associatedTranscriptsPassword = Lens.lens (\EncryptionSetting' {associatedTranscriptsPassword} -> associatedTranscriptsPassword) (\s@EncryptionSetting' {} a -> s {associatedTranscriptsPassword = a} :: EncryptionSetting) Prelude.. Lens.mapping Data._Sensitive

-- | The password used to encrypt the recommended bot recommendation file.
encryptionSetting_botLocaleExportPassword :: Lens.Lens' EncryptionSetting (Prelude.Maybe Prelude.Text)
encryptionSetting_botLocaleExportPassword = Lens.lens (\EncryptionSetting' {botLocaleExportPassword} -> botLocaleExportPassword) (\s@EncryptionSetting' {} a -> s {botLocaleExportPassword = a} :: EncryptionSetting) Prelude.. Lens.mapping Data._Sensitive

-- | The KMS key ARN used to encrypt the metadata associated with the bot
-- recommendation.
encryptionSetting_kmsKeyArn :: Lens.Lens' EncryptionSetting (Prelude.Maybe Prelude.Text)
encryptionSetting_kmsKeyArn = Lens.lens (\EncryptionSetting' {kmsKeyArn} -> kmsKeyArn) (\s@EncryptionSetting' {} a -> s {kmsKeyArn = a} :: EncryptionSetting)

instance Data.FromJSON EncryptionSetting where
  parseJSON =
    Data.withObject
      "EncryptionSetting"
      ( \x ->
          EncryptionSetting'
            Prelude.<$> (x Data..:? "associatedTranscriptsPassword")
            Prelude.<*> (x Data..:? "botLocaleExportPassword")
            Prelude.<*> (x Data..:? "kmsKeyArn")
      )

instance Prelude.Hashable EncryptionSetting where
  hashWithSalt _salt EncryptionSetting' {..} =
    _salt
      `Prelude.hashWithSalt` associatedTranscriptsPassword
      `Prelude.hashWithSalt` botLocaleExportPassword
      `Prelude.hashWithSalt` kmsKeyArn

instance Prelude.NFData EncryptionSetting where
  rnf EncryptionSetting' {..} =
    Prelude.rnf associatedTranscriptsPassword `Prelude.seq`
      Prelude.rnf botLocaleExportPassword `Prelude.seq`
        Prelude.rnf kmsKeyArn

instance Data.ToJSON EncryptionSetting where
  toJSON EncryptionSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associatedTranscriptsPassword" Data..=)
              Prelude.<$> associatedTranscriptsPassword,
            ("botLocaleExportPassword" Data..=)
              Prelude.<$> botLocaleExportPassword,
            ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn
          ]
      )
