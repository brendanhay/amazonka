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
-- Module      : Amazonka.MacieV2.Types.SensitivityAggregations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SensitivityAggregations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides aggregated statistical data for sensitive data discovery
-- metrics that apply to S3 buckets. Each field contains aggregated data
-- for all the buckets that have a sensitivity score (sensitivityScore) of
-- a specified value or within a specified range
-- (BucketStatisticsBySensitivity). If automated sensitive data discovery
-- is currently disabled for your account, the value for each field is 0.
--
-- /See:/ 'newSensitivityAggregations' smart constructor.
data SensitivityAggregations = SensitivityAggregations'
  { -- | The total storage size, in bytes, of all the objects that Amazon Macie
    -- can analyze in the buckets. These objects use a supported storage class
    -- and have a file name extension for a supported file or storage format.
    --
    -- If versioning is enabled for any of the buckets, this value is based on
    -- the size of the latest version of each applicable object in the buckets.
    -- This value doesn\'t reflect the storage size of all versions of all
    -- applicable objects in the buckets.
    classifiableSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that are publicly accessible based on a
    -- combination of permissions settings for each bucket.
    publiclyAccessibleCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the buckets.
    --
    -- If versioning is enabled for any of the buckets, this value is based on
    -- the size of the latest version of each object in the buckets. This value
    -- doesn\'t reflect the storage size of all versions of the objects in the
    -- buckets.
    totalSizeInBytes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitivityAggregations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classifiableSizeInBytes', 'sensitivityAggregations_classifiableSizeInBytes' - The total storage size, in bytes, of all the objects that Amazon Macie
-- can analyze in the buckets. These objects use a supported storage class
-- and have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of all
-- applicable objects in the buckets.
--
-- 'publiclyAccessibleCount', 'sensitivityAggregations_publiclyAccessibleCount' - The total number of buckets that are publicly accessible based on a
-- combination of permissions settings for each bucket.
--
-- 'totalCount', 'sensitivityAggregations_totalCount' - The total number of buckets.
--
-- 'totalSizeInBytes', 'sensitivityAggregations_totalSizeInBytes' - The total storage size, in bytes, of the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each object in the buckets. This value
-- doesn\'t reflect the storage size of all versions of the objects in the
-- buckets.
newSensitivityAggregations ::
  SensitivityAggregations
newSensitivityAggregations =
  SensitivityAggregations'
    { classifiableSizeInBytes =
        Prelude.Nothing,
      publiclyAccessibleCount = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      totalSizeInBytes = Prelude.Nothing
    }

-- | The total storage size, in bytes, of all the objects that Amazon Macie
-- can analyze in the buckets. These objects use a supported storage class
-- and have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of all
-- applicable objects in the buckets.
sensitivityAggregations_classifiableSizeInBytes :: Lens.Lens' SensitivityAggregations (Prelude.Maybe Prelude.Integer)
sensitivityAggregations_classifiableSizeInBytes = Lens.lens (\SensitivityAggregations' {classifiableSizeInBytes} -> classifiableSizeInBytes) (\s@SensitivityAggregations' {} a -> s {classifiableSizeInBytes = a} :: SensitivityAggregations)

-- | The total number of buckets that are publicly accessible based on a
-- combination of permissions settings for each bucket.
sensitivityAggregations_publiclyAccessibleCount :: Lens.Lens' SensitivityAggregations (Prelude.Maybe Prelude.Integer)
sensitivityAggregations_publiclyAccessibleCount = Lens.lens (\SensitivityAggregations' {publiclyAccessibleCount} -> publiclyAccessibleCount) (\s@SensitivityAggregations' {} a -> s {publiclyAccessibleCount = a} :: SensitivityAggregations)

-- | The total number of buckets.
sensitivityAggregations_totalCount :: Lens.Lens' SensitivityAggregations (Prelude.Maybe Prelude.Integer)
sensitivityAggregations_totalCount = Lens.lens (\SensitivityAggregations' {totalCount} -> totalCount) (\s@SensitivityAggregations' {} a -> s {totalCount = a} :: SensitivityAggregations)

-- | The total storage size, in bytes, of the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each object in the buckets. This value
-- doesn\'t reflect the storage size of all versions of the objects in the
-- buckets.
sensitivityAggregations_totalSizeInBytes :: Lens.Lens' SensitivityAggregations (Prelude.Maybe Prelude.Integer)
sensitivityAggregations_totalSizeInBytes = Lens.lens (\SensitivityAggregations' {totalSizeInBytes} -> totalSizeInBytes) (\s@SensitivityAggregations' {} a -> s {totalSizeInBytes = a} :: SensitivityAggregations)

instance Data.FromJSON SensitivityAggregations where
  parseJSON =
    Data.withObject
      "SensitivityAggregations"
      ( \x ->
          SensitivityAggregations'
            Prelude.<$> (x Data..:? "classifiableSizeInBytes")
            Prelude.<*> (x Data..:? "publiclyAccessibleCount")
            Prelude.<*> (x Data..:? "totalCount")
            Prelude.<*> (x Data..:? "totalSizeInBytes")
      )

instance Prelude.Hashable SensitivityAggregations where
  hashWithSalt _salt SensitivityAggregations' {..} =
    _salt
      `Prelude.hashWithSalt` classifiableSizeInBytes
      `Prelude.hashWithSalt` publiclyAccessibleCount
      `Prelude.hashWithSalt` totalCount
      `Prelude.hashWithSalt` totalSizeInBytes

instance Prelude.NFData SensitivityAggregations where
  rnf SensitivityAggregations' {..} =
    Prelude.rnf classifiableSizeInBytes
      `Prelude.seq` Prelude.rnf publiclyAccessibleCount
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf totalSizeInBytes
