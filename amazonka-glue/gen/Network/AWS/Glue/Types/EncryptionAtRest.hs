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
-- Module      : Network.AWS.Glue.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionAtRest where

import Network.AWS.Glue.Types.CatalogEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /See:/ 'newEncryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { -- | The ID of the AWS KMS key to use for encryption at rest.
    sseAwsKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption-at-rest mode for encrypting Data Catalog data.
    catalogEncryptionMode :: CatalogEncryptionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sseAwsKmsKeyId', 'encryptionAtRest_sseAwsKmsKeyId' - The ID of the AWS KMS key to use for encryption at rest.
--
-- 'catalogEncryptionMode', 'encryptionAtRest_catalogEncryptionMode' - The encryption-at-rest mode for encrypting Data Catalog data.
newEncryptionAtRest ::
  -- | 'catalogEncryptionMode'
  CatalogEncryptionMode ->
  EncryptionAtRest
newEncryptionAtRest pCatalogEncryptionMode_ =
  EncryptionAtRest'
    { sseAwsKmsKeyId = Prelude.Nothing,
      catalogEncryptionMode = pCatalogEncryptionMode_
    }

-- | The ID of the AWS KMS key to use for encryption at rest.
encryptionAtRest_sseAwsKmsKeyId :: Lens.Lens' EncryptionAtRest (Prelude.Maybe Prelude.Text)
encryptionAtRest_sseAwsKmsKeyId = Lens.lens (\EncryptionAtRest' {sseAwsKmsKeyId} -> sseAwsKmsKeyId) (\s@EncryptionAtRest' {} a -> s {sseAwsKmsKeyId = a} :: EncryptionAtRest)

-- | The encryption-at-rest mode for encrypting Data Catalog data.
encryptionAtRest_catalogEncryptionMode :: Lens.Lens' EncryptionAtRest CatalogEncryptionMode
encryptionAtRest_catalogEncryptionMode = Lens.lens (\EncryptionAtRest' {catalogEncryptionMode} -> catalogEncryptionMode) (\s@EncryptionAtRest' {} a -> s {catalogEncryptionMode = a} :: EncryptionAtRest)

instance Prelude.FromJSON EncryptionAtRest where
  parseJSON =
    Prelude.withObject
      "EncryptionAtRest"
      ( \x ->
          EncryptionAtRest'
            Prelude.<$> (x Prelude..:? "SseAwsKmsKeyId")
            Prelude.<*> (x Prelude..: "CatalogEncryptionMode")
      )

instance Prelude.Hashable EncryptionAtRest

instance Prelude.NFData EncryptionAtRest

instance Prelude.ToJSON EncryptionAtRest where
  toJSON EncryptionAtRest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SseAwsKmsKeyId" Prelude..=)
              Prelude.<$> sseAwsKmsKeyId,
            Prelude.Just
              ( "CatalogEncryptionMode"
                  Prelude..= catalogEncryptionMode
              )
          ]
      )
