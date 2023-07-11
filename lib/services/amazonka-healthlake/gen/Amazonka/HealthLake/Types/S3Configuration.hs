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
-- Module      : Amazonka.HealthLake.Types.S3Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.S3Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the S3 bucket for either an import or export job.
-- This includes assigning permissions for access.
--
-- /See:/ 'newS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | The S3Uri is the user specified S3 location of the FHIR data to be
    -- imported into Amazon HealthLake.
    s3Uri :: Prelude.Text,
    -- | The KMS key ID used to access the S3 bucket.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 's3Configuration_s3Uri' - The S3Uri is the user specified S3 location of the FHIR data to be
-- imported into Amazon HealthLake.
--
-- 'kmsKeyId', 's3Configuration_kmsKeyId' - The KMS key ID used to access the S3 bucket.
newS3Configuration ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  S3Configuration
newS3Configuration pS3Uri_ pKmsKeyId_ =
  S3Configuration'
    { s3Uri = pS3Uri_,
      kmsKeyId = pKmsKeyId_
    }

-- | The S3Uri is the user specified S3 location of the FHIR data to be
-- imported into Amazon HealthLake.
s3Configuration_s3Uri :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_s3Uri = Lens.lens (\S3Configuration' {s3Uri} -> s3Uri) (\s@S3Configuration' {} a -> s {s3Uri = a} :: S3Configuration)

-- | The KMS key ID used to access the S3 bucket.
s3Configuration_kmsKeyId :: Lens.Lens' S3Configuration Prelude.Text
s3Configuration_kmsKeyId = Lens.lens (\S3Configuration' {kmsKeyId} -> kmsKeyId) (\s@S3Configuration' {} a -> s {kmsKeyId = a} :: S3Configuration)

instance Data.FromJSON S3Configuration where
  parseJSON =
    Data.withObject
      "S3Configuration"
      ( \x ->
          S3Configuration'
            Prelude.<$> (x Data..: "S3Uri")
            Prelude.<*> (x Data..: "KmsKeyId")
      )

instance Prelude.Hashable S3Configuration where
  hashWithSalt _salt S3Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData S3Configuration where
  rnf S3Configuration' {..} =
    Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just ("KmsKeyId" Data..= kmsKeyId)
          ]
      )
