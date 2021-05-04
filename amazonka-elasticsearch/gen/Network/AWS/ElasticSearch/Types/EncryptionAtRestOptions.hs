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
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the Encryption At Rest Options.
--
-- /See:/ 'newEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | Specifies the option to enable Encryption At Rest.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the KMS Key ID for Encryption At Rest options.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Prelude.withObject
      "EncryptionAtRestOptions"
      ( \x ->
          EncryptionAtRestOptions'
            Prelude.<$> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
      )

instance Prelude.Hashable EncryptionAtRestOptions

instance Prelude.NFData EncryptionAtRestOptions

instance Prelude.ToJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId
          ]
      )
