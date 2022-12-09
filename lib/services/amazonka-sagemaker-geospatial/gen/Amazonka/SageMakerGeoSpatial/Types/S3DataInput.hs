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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.S3DataInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.S3DataInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.MetadataProvider

-- | Path to Amazon S3 storage location for input data.
--
-- /See:/ 'newS3DataInput' smart constructor.
data S3DataInput = S3DataInput'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    metadataProvider :: MetadataProvider,
    -- | The URL to the Amazon S3 input.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 's3DataInput_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 'metadataProvider', 's3DataInput_metadataProvider' -
--
-- 's3Uri', 's3DataInput_s3Uri' - The URL to the Amazon S3 input.
newS3DataInput ::
  -- | 'metadataProvider'
  MetadataProvider ->
  -- | 's3Uri'
  Prelude.Text ->
  S3DataInput
newS3DataInput pMetadataProvider_ pS3Uri_ =
  S3DataInput'
    { kmsKeyId = Prelude.Nothing,
      metadataProvider = pMetadataProvider_,
      s3Uri = pS3Uri_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
s3DataInput_kmsKeyId :: Lens.Lens' S3DataInput (Prelude.Maybe Prelude.Text)
s3DataInput_kmsKeyId = Lens.lens (\S3DataInput' {kmsKeyId} -> kmsKeyId) (\s@S3DataInput' {} a -> s {kmsKeyId = a} :: S3DataInput)

-- |
s3DataInput_metadataProvider :: Lens.Lens' S3DataInput MetadataProvider
s3DataInput_metadataProvider = Lens.lens (\S3DataInput' {metadataProvider} -> metadataProvider) (\s@S3DataInput' {} a -> s {metadataProvider = a} :: S3DataInput)

-- | The URL to the Amazon S3 input.
s3DataInput_s3Uri :: Lens.Lens' S3DataInput Prelude.Text
s3DataInput_s3Uri = Lens.lens (\S3DataInput' {s3Uri} -> s3Uri) (\s@S3DataInput' {} a -> s {s3Uri = a} :: S3DataInput)

instance Data.FromJSON S3DataInput where
  parseJSON =
    Data.withObject
      "S3DataInput"
      ( \x ->
          S3DataInput'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "MetadataProvider")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable S3DataInput where
  hashWithSalt _salt S3DataInput' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` metadataProvider
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData S3DataInput where
  rnf S3DataInput' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf metadataProvider
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON S3DataInput where
  toJSON S3DataInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("MetadataProvider" Data..= metadataProvider),
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
