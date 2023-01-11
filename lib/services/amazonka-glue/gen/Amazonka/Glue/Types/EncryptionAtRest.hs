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
-- Module      : Amazonka.Glue.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.EncryptionAtRest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CatalogEncryptionMode
import qualified Amazonka.Prelude as Prelude

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /See:/ 'newEncryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { -- | The ID of the KMS key to use for encryption at rest.
    sseAwsKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption-at-rest mode for encrypting Data Catalog data.
    catalogEncryptionMode :: CatalogEncryptionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sseAwsKmsKeyId', 'encryptionAtRest_sseAwsKmsKeyId' - The ID of the KMS key to use for encryption at rest.
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

-- | The ID of the KMS key to use for encryption at rest.
encryptionAtRest_sseAwsKmsKeyId :: Lens.Lens' EncryptionAtRest (Prelude.Maybe Prelude.Text)
encryptionAtRest_sseAwsKmsKeyId = Lens.lens (\EncryptionAtRest' {sseAwsKmsKeyId} -> sseAwsKmsKeyId) (\s@EncryptionAtRest' {} a -> s {sseAwsKmsKeyId = a} :: EncryptionAtRest)

-- | The encryption-at-rest mode for encrypting Data Catalog data.
encryptionAtRest_catalogEncryptionMode :: Lens.Lens' EncryptionAtRest CatalogEncryptionMode
encryptionAtRest_catalogEncryptionMode = Lens.lens (\EncryptionAtRest' {catalogEncryptionMode} -> catalogEncryptionMode) (\s@EncryptionAtRest' {} a -> s {catalogEncryptionMode = a} :: EncryptionAtRest)

instance Data.FromJSON EncryptionAtRest where
  parseJSON =
    Data.withObject
      "EncryptionAtRest"
      ( \x ->
          EncryptionAtRest'
            Prelude.<$> (x Data..:? "SseAwsKmsKeyId")
            Prelude.<*> (x Data..: "CatalogEncryptionMode")
      )

instance Prelude.Hashable EncryptionAtRest where
  hashWithSalt _salt EncryptionAtRest' {..} =
    _salt `Prelude.hashWithSalt` sseAwsKmsKeyId
      `Prelude.hashWithSalt` catalogEncryptionMode

instance Prelude.NFData EncryptionAtRest where
  rnf EncryptionAtRest' {..} =
    Prelude.rnf sseAwsKmsKeyId
      `Prelude.seq` Prelude.rnf catalogEncryptionMode

instance Data.ToJSON EncryptionAtRest where
  toJSON EncryptionAtRest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SseAwsKmsKeyId" Data..=)
              Prelude.<$> sseAwsKmsKeyId,
            Prelude.Just
              ( "CatalogEncryptionMode"
                  Data..= catalogEncryptionMode
              )
          ]
      )
