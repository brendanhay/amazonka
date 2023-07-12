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
-- Module      : Amazonka.MacieV2.Types.ObjectLevelStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ObjectLevelStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the total storage size (in bytes) or number
-- of objects that Amazon Macie can\'t analyze in one or more S3 buckets.
-- In a BucketMetadata or MatchingBucket object, this data is for a
-- specific bucket. In a GetBucketStatisticsResponse object, this data is
-- aggregated for all the buckets in the query results. If versioning is
-- enabled for a bucket, storage size values are based on the size of the
-- latest version of each applicable object in the bucket.
--
-- /See:/ 'newObjectLevelStatistics' smart constructor.
data ObjectLevelStatistics = ObjectLevelStatistics'
  { -- | The total storage size (in bytes) or number of objects that Amazon Macie
    -- can\'t analyze because the objects don\'t have a file name extension for
    -- a supported file or storage format.
    fileType :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size (in bytes) or number of objects that Amazon Macie
    -- can\'t analyze because the objects use an unsupported storage class.
    storageClass :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size (in bytes) or number of objects that Amazon Macie
    -- can\'t analyze because the objects use an unsupported storage class or
    -- don\'t have a file name extension for a supported file or storage
    -- format.
    total :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectLevelStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileType', 'objectLevelStatistics_fileType' - The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects don\'t have a file name extension for
-- a supported file or storage format.
--
-- 'storageClass', 'objectLevelStatistics_storageClass' - The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects use an unsupported storage class.
--
-- 'total', 'objectLevelStatistics_total' - The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects use an unsupported storage class or
-- don\'t have a file name extension for a supported file or storage
-- format.
newObjectLevelStatistics ::
  ObjectLevelStatistics
newObjectLevelStatistics =
  ObjectLevelStatistics'
    { fileType = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      total = Prelude.Nothing
    }

-- | The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects don\'t have a file name extension for
-- a supported file or storage format.
objectLevelStatistics_fileType :: Lens.Lens' ObjectLevelStatistics (Prelude.Maybe Prelude.Integer)
objectLevelStatistics_fileType = Lens.lens (\ObjectLevelStatistics' {fileType} -> fileType) (\s@ObjectLevelStatistics' {} a -> s {fileType = a} :: ObjectLevelStatistics)

-- | The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects use an unsupported storage class.
objectLevelStatistics_storageClass :: Lens.Lens' ObjectLevelStatistics (Prelude.Maybe Prelude.Integer)
objectLevelStatistics_storageClass = Lens.lens (\ObjectLevelStatistics' {storageClass} -> storageClass) (\s@ObjectLevelStatistics' {} a -> s {storageClass = a} :: ObjectLevelStatistics)

-- | The total storage size (in bytes) or number of objects that Amazon Macie
-- can\'t analyze because the objects use an unsupported storage class or
-- don\'t have a file name extension for a supported file or storage
-- format.
objectLevelStatistics_total :: Lens.Lens' ObjectLevelStatistics (Prelude.Maybe Prelude.Integer)
objectLevelStatistics_total = Lens.lens (\ObjectLevelStatistics' {total} -> total) (\s@ObjectLevelStatistics' {} a -> s {total = a} :: ObjectLevelStatistics)

instance Data.FromJSON ObjectLevelStatistics where
  parseJSON =
    Data.withObject
      "ObjectLevelStatistics"
      ( \x ->
          ObjectLevelStatistics'
            Prelude.<$> (x Data..:? "fileType")
            Prelude.<*> (x Data..:? "storageClass")
            Prelude.<*> (x Data..:? "total")
      )

instance Prelude.Hashable ObjectLevelStatistics where
  hashWithSalt _salt ObjectLevelStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` fileType
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` total

instance Prelude.NFData ObjectLevelStatistics where
  rnf ObjectLevelStatistics' {..} =
    Prelude.rnf fileType
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf total
