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
-- Module      : Amazonka.Panorama.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A location in Amazon S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The bucket\'s Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | A bucket name.
    bucketName :: Prelude.Text,
    -- | An object key.
    objectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 's3Location_region' - The bucket\'s Region.
--
-- 'bucketName', 's3Location_bucketName' - A bucket name.
--
-- 'objectKey', 's3Location_objectKey' - An object key.
newS3Location ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'objectKey'
  Prelude.Text ->
  S3Location
newS3Location pBucketName_ pObjectKey_ =
  S3Location'
    { region = Prelude.Nothing,
      bucketName = pBucketName_,
      objectKey = pObjectKey_
    }

-- | The bucket\'s Region.
s3Location_region :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_region = Lens.lens (\S3Location' {region} -> region) (\s@S3Location' {} a -> s {region = a} :: S3Location)

-- | A bucket name.
s3Location_bucketName :: Lens.Lens' S3Location Prelude.Text
s3Location_bucketName = Lens.lens (\S3Location' {bucketName} -> bucketName) (\s@S3Location' {} a -> s {bucketName = a} :: S3Location)

-- | An object key.
s3Location_objectKey :: Lens.Lens' S3Location Prelude.Text
s3Location_objectKey = Lens.lens (\S3Location' {objectKey} -> objectKey) (\s@S3Location' {} a -> s {objectKey = a} :: S3Location)

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Core..:? "Region")
            Prelude.<*> (x Core..: "BucketName")
            Prelude.<*> (x Core..: "ObjectKey")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKey

instance Core.ToJSON S3Location where
  toJSON S3Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Region" Core..=) Prelude.<$> region,
            Prelude.Just ("BucketName" Core..= bucketName),
            Prelude.Just ("ObjectKey" Core..= objectKey)
          ]
      )
