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
-- Module      : Amazonka.CloudTrail.Types.S3ImportSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.S3ImportSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for the source S3 bucket.
--
-- /See:/ 'newS3ImportSource' smart constructor.
data S3ImportSource = S3ImportSource'
  { -- | The URI for the source S3 bucket.
    s3LocationUri :: Prelude.Text,
    -- | The region associated with the source S3 bucket.
    s3BucketRegion :: Prelude.Text,
    -- | The IAM ARN role used to access the source S3 bucket.
    s3BucketAccessRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ImportSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3LocationUri', 's3ImportSource_s3LocationUri' - The URI for the source S3 bucket.
--
-- 's3BucketRegion', 's3ImportSource_s3BucketRegion' - The region associated with the source S3 bucket.
--
-- 's3BucketAccessRoleArn', 's3ImportSource_s3BucketAccessRoleArn' - The IAM ARN role used to access the source S3 bucket.
newS3ImportSource ::
  -- | 's3LocationUri'
  Prelude.Text ->
  -- | 's3BucketRegion'
  Prelude.Text ->
  -- | 's3BucketAccessRoleArn'
  Prelude.Text ->
  S3ImportSource
newS3ImportSource
  pS3LocationUri_
  pS3BucketRegion_
  pS3BucketAccessRoleArn_ =
    S3ImportSource'
      { s3LocationUri = pS3LocationUri_,
        s3BucketRegion = pS3BucketRegion_,
        s3BucketAccessRoleArn = pS3BucketAccessRoleArn_
      }

-- | The URI for the source S3 bucket.
s3ImportSource_s3LocationUri :: Lens.Lens' S3ImportSource Prelude.Text
s3ImportSource_s3LocationUri = Lens.lens (\S3ImportSource' {s3LocationUri} -> s3LocationUri) (\s@S3ImportSource' {} a -> s {s3LocationUri = a} :: S3ImportSource)

-- | The region associated with the source S3 bucket.
s3ImportSource_s3BucketRegion :: Lens.Lens' S3ImportSource Prelude.Text
s3ImportSource_s3BucketRegion = Lens.lens (\S3ImportSource' {s3BucketRegion} -> s3BucketRegion) (\s@S3ImportSource' {} a -> s {s3BucketRegion = a} :: S3ImportSource)

-- | The IAM ARN role used to access the source S3 bucket.
s3ImportSource_s3BucketAccessRoleArn :: Lens.Lens' S3ImportSource Prelude.Text
s3ImportSource_s3BucketAccessRoleArn = Lens.lens (\S3ImportSource' {s3BucketAccessRoleArn} -> s3BucketAccessRoleArn) (\s@S3ImportSource' {} a -> s {s3BucketAccessRoleArn = a} :: S3ImportSource)

instance Data.FromJSON S3ImportSource where
  parseJSON =
    Data.withObject
      "S3ImportSource"
      ( \x ->
          S3ImportSource'
            Prelude.<$> (x Data..: "S3LocationUri")
            Prelude.<*> (x Data..: "S3BucketRegion")
            Prelude.<*> (x Data..: "S3BucketAccessRoleArn")
      )

instance Prelude.Hashable S3ImportSource where
  hashWithSalt _salt S3ImportSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3LocationUri
      `Prelude.hashWithSalt` s3BucketRegion
      `Prelude.hashWithSalt` s3BucketAccessRoleArn

instance Prelude.NFData S3ImportSource where
  rnf S3ImportSource' {..} =
    Prelude.rnf s3LocationUri
      `Prelude.seq` Prelude.rnf s3BucketRegion
      `Prelude.seq` Prelude.rnf s3BucketAccessRoleArn

instance Data.ToJSON S3ImportSource where
  toJSON S3ImportSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("S3LocationUri" Data..= s3LocationUri),
            Prelude.Just
              ("S3BucketRegion" Data..= s3BucketRegion),
            Prelude.Just
              ( "S3BucketAccessRoleArn"
                  Data..= s3BucketAccessRoleArn
              )
          ]
      )
