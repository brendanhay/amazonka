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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 data for the Vector Enrichment job.
--
-- /See:/ 'newVectorEnrichmentJobS3Data' smart constructor.
data VectorEnrichmentJobS3Data = VectorEnrichmentJobS3Data'
  { -- | The Amazon Key Management Service (KMS) key ID for server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The URL to the Amazon S3 data for the Vector Enrichment job.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobS3Data' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'vectorEnrichmentJobS3Data_kmsKeyId' - The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
--
-- 's3Uri', 'vectorEnrichmentJobS3Data_s3Uri' - The URL to the Amazon S3 data for the Vector Enrichment job.
newVectorEnrichmentJobS3Data ::
  -- | 's3Uri'
  Prelude.Text ->
  VectorEnrichmentJobS3Data
newVectorEnrichmentJobS3Data pS3Uri_ =
  VectorEnrichmentJobS3Data'
    { kmsKeyId =
        Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | The Amazon Key Management Service (KMS) key ID for server-side
-- encryption.
vectorEnrichmentJobS3Data_kmsKeyId :: Lens.Lens' VectorEnrichmentJobS3Data (Prelude.Maybe Prelude.Text)
vectorEnrichmentJobS3Data_kmsKeyId = Lens.lens (\VectorEnrichmentJobS3Data' {kmsKeyId} -> kmsKeyId) (\s@VectorEnrichmentJobS3Data' {} a -> s {kmsKeyId = a} :: VectorEnrichmentJobS3Data)

-- | The URL to the Amazon S3 data for the Vector Enrichment job.
vectorEnrichmentJobS3Data_s3Uri :: Lens.Lens' VectorEnrichmentJobS3Data Prelude.Text
vectorEnrichmentJobS3Data_s3Uri = Lens.lens (\VectorEnrichmentJobS3Data' {s3Uri} -> s3Uri) (\s@VectorEnrichmentJobS3Data' {} a -> s {s3Uri = a} :: VectorEnrichmentJobS3Data)

instance Data.FromJSON VectorEnrichmentJobS3Data where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobS3Data"
      ( \x ->
          VectorEnrichmentJobS3Data'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable VectorEnrichmentJobS3Data where
  hashWithSalt _salt VectorEnrichmentJobS3Data' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData VectorEnrichmentJobS3Data where
  rnf VectorEnrichmentJobS3Data' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON VectorEnrichmentJobS3Data where
  toJSON VectorEnrichmentJobS3Data' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
