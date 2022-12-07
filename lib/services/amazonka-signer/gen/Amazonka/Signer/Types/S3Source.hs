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
-- Module      : Amazonka.Signer.Types.S3Source
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.S3Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the S3 bucket where you saved your unsigned code.
--
-- /See:/ 'newS3Source' smart constructor.
data S3Source = S3Source'
  { -- | Name of the S3 bucket.
    bucketName :: Prelude.Text,
    -- | Key name of the bucket object that contains your unsigned code.
    key :: Prelude.Text,
    -- | Version of your source image in your version enabled S3 bucket.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3Source_bucketName' - Name of the S3 bucket.
--
-- 'key', 's3Source_key' - Key name of the bucket object that contains your unsigned code.
--
-- 'version', 's3Source_version' - Version of your source image in your version enabled S3 bucket.
newS3Source ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  S3Source
newS3Source pBucketName_ pKey_ pVersion_ =
  S3Source'
    { bucketName = pBucketName_,
      key = pKey_,
      version = pVersion_
    }

-- | Name of the S3 bucket.
s3Source_bucketName :: Lens.Lens' S3Source Prelude.Text
s3Source_bucketName = Lens.lens (\S3Source' {bucketName} -> bucketName) (\s@S3Source' {} a -> s {bucketName = a} :: S3Source)

-- | Key name of the bucket object that contains your unsigned code.
s3Source_key :: Lens.Lens' S3Source Prelude.Text
s3Source_key = Lens.lens (\S3Source' {key} -> key) (\s@S3Source' {} a -> s {key = a} :: S3Source)

-- | Version of your source image in your version enabled S3 bucket.
s3Source_version :: Lens.Lens' S3Source Prelude.Text
s3Source_version = Lens.lens (\S3Source' {version} -> version) (\s@S3Source' {} a -> s {version = a} :: S3Source)

instance Data.FromJSON S3Source where
  parseJSON =
    Data.withObject
      "S3Source"
      ( \x ->
          S3Source'
            Prelude.<$> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "key")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable S3Source where
  hashWithSalt _salt S3Source' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` version

instance Prelude.NFData S3Source where
  rnf S3Source' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON S3Source where
  toJSON S3Source' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("key" Data..= key),
            Prelude.Just ("version" Data..= version)
          ]
      )
