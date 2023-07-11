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
-- Module      : Amazonka.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.EncryptionAtRestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Encryption At Rest Options.
--
-- /See:/ 'newEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | Specifies the option to enable Encryption At Rest.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS Key ID for Encryption At Rest options.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'encryptionAtRestOptions_enabled' - Specifies the option to enable Encryption At Rest.
--
-- 'kmsKeyId', 'encryptionAtRestOptions_kmsKeyId' - Specifies the KMS Key ID for Encryption At Rest options.
newEncryptionAtRestOptions ::
  EncryptionAtRestOptions
newEncryptionAtRestOptions =
  EncryptionAtRestOptions'
    { enabled = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing
    }

-- | Specifies the option to enable Encryption At Rest.
encryptionAtRestOptions_enabled :: Lens.Lens' EncryptionAtRestOptions (Prelude.Maybe Prelude.Bool)
encryptionAtRestOptions_enabled = Lens.lens (\EncryptionAtRestOptions' {enabled} -> enabled) (\s@EncryptionAtRestOptions' {} a -> s {enabled = a} :: EncryptionAtRestOptions)

-- | Specifies the KMS Key ID for Encryption At Rest options.
encryptionAtRestOptions_kmsKeyId :: Lens.Lens' EncryptionAtRestOptions (Prelude.Maybe Prelude.Text)
encryptionAtRestOptions_kmsKeyId = Lens.lens (\EncryptionAtRestOptions' {kmsKeyId} -> kmsKeyId) (\s@EncryptionAtRestOptions' {} a -> s {kmsKeyId = a} :: EncryptionAtRestOptions)

instance Data.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Data.withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "KmsKeyId")
      )

instance Prelude.Hashable EncryptionAtRestOptions where
  hashWithSalt _salt EncryptionAtRestOptions' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData EncryptionAtRestOptions where
  rnf EncryptionAtRestOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId
          ]
      )
