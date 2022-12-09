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
-- Module      : Amazonka.MacieV2.Types.BucketCountByEffectivePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketCountByEffectivePermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the number of S3 buckets that are publicly
-- accessible based on a combination of permissions settings for each
-- bucket.
--
-- /See:/ 'newBucketCountByEffectivePermission' smart constructor.
data BucketCountByEffectivePermission = BucketCountByEffectivePermission'
  { -- | The total number of buckets that allow the general public to have read
    -- or write access to the bucket.
    publiclyAccessible :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that allow the general public to have read
    -- access to the bucket.
    publiclyReadable :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that allow the general public to have write
    -- access to the bucket.
    publiclyWritable :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
    -- permissions settings for. Macie can\'t determine whether these buckets
    -- are publicly accessible.
    unknown :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketCountByEffectivePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publiclyAccessible', 'bucketCountByEffectivePermission_publiclyAccessible' - The total number of buckets that allow the general public to have read
-- or write access to the bucket.
--
-- 'publiclyReadable', 'bucketCountByEffectivePermission_publiclyReadable' - The total number of buckets that allow the general public to have read
-- access to the bucket.
--
-- 'publiclyWritable', 'bucketCountByEffectivePermission_publiclyWritable' - The total number of buckets that allow the general public to have write
-- access to the bucket.
--
-- 'unknown', 'bucketCountByEffectivePermission_unknown' - The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- permissions settings for. Macie can\'t determine whether these buckets
-- are publicly accessible.
newBucketCountByEffectivePermission ::
  BucketCountByEffectivePermission
newBucketCountByEffectivePermission =
  BucketCountByEffectivePermission'
    { publiclyAccessible =
        Prelude.Nothing,
      publiclyReadable = Prelude.Nothing,
      publiclyWritable = Prelude.Nothing,
      unknown = Prelude.Nothing
    }

-- | The total number of buckets that allow the general public to have read
-- or write access to the bucket.
bucketCountByEffectivePermission_publiclyAccessible :: Lens.Lens' BucketCountByEffectivePermission (Prelude.Maybe Prelude.Integer)
bucketCountByEffectivePermission_publiclyAccessible = Lens.lens (\BucketCountByEffectivePermission' {publiclyAccessible} -> publiclyAccessible) (\s@BucketCountByEffectivePermission' {} a -> s {publiclyAccessible = a} :: BucketCountByEffectivePermission)

-- | The total number of buckets that allow the general public to have read
-- access to the bucket.
bucketCountByEffectivePermission_publiclyReadable :: Lens.Lens' BucketCountByEffectivePermission (Prelude.Maybe Prelude.Integer)
bucketCountByEffectivePermission_publiclyReadable = Lens.lens (\BucketCountByEffectivePermission' {publiclyReadable} -> publiclyReadable) (\s@BucketCountByEffectivePermission' {} a -> s {publiclyReadable = a} :: BucketCountByEffectivePermission)

-- | The total number of buckets that allow the general public to have write
-- access to the bucket.
bucketCountByEffectivePermission_publiclyWritable :: Lens.Lens' BucketCountByEffectivePermission (Prelude.Maybe Prelude.Integer)
bucketCountByEffectivePermission_publiclyWritable = Lens.lens (\BucketCountByEffectivePermission' {publiclyWritable} -> publiclyWritable) (\s@BucketCountByEffectivePermission' {} a -> s {publiclyWritable = a} :: BucketCountByEffectivePermission)

-- | The total number of buckets that Amazon Macie wasn\'t able to evaluate
-- permissions settings for. Macie can\'t determine whether these buckets
-- are publicly accessible.
bucketCountByEffectivePermission_unknown :: Lens.Lens' BucketCountByEffectivePermission (Prelude.Maybe Prelude.Integer)
bucketCountByEffectivePermission_unknown = Lens.lens (\BucketCountByEffectivePermission' {unknown} -> unknown) (\s@BucketCountByEffectivePermission' {} a -> s {unknown = a} :: BucketCountByEffectivePermission)

instance
  Data.FromJSON
    BucketCountByEffectivePermission
  where
  parseJSON =
    Data.withObject
      "BucketCountByEffectivePermission"
      ( \x ->
          BucketCountByEffectivePermission'
            Prelude.<$> (x Data..:? "publiclyAccessible")
            Prelude.<*> (x Data..:? "publiclyReadable")
            Prelude.<*> (x Data..:? "publiclyWritable")
            Prelude.<*> (x Data..:? "unknown")
      )

instance
  Prelude.Hashable
    BucketCountByEffectivePermission
  where
  hashWithSalt
    _salt
    BucketCountByEffectivePermission' {..} =
      _salt `Prelude.hashWithSalt` publiclyAccessible
        `Prelude.hashWithSalt` publiclyReadable
        `Prelude.hashWithSalt` publiclyWritable
        `Prelude.hashWithSalt` unknown

instance
  Prelude.NFData
    BucketCountByEffectivePermission
  where
  rnf BucketCountByEffectivePermission' {..} =
    Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf publiclyReadable
      `Prelude.seq` Prelude.rnf publiclyWritable
      `Prelude.seq` Prelude.rnf unknown
