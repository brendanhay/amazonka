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
-- Module      : Amazonka.GuardDuty.Types.BucketPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.BucketPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the current bucket policies for the S3 bucket.
--
-- /See:/ 'newBucketPolicy' smart constructor.
data BucketPolicy = BucketPolicy'
  { -- | A value that indicates whether public read access for the bucket is
    -- enabled through a bucket policy.
    allowsPublicReadAccess :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether public write access for the bucket is
    -- enabled through a bucket policy.
    allowsPublicWriteAccess :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsPublicReadAccess', 'bucketPolicy_allowsPublicReadAccess' - A value that indicates whether public read access for the bucket is
-- enabled through a bucket policy.
--
-- 'allowsPublicWriteAccess', 'bucketPolicy_allowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is
-- enabled through a bucket policy.
newBucketPolicy ::
  BucketPolicy
newBucketPolicy =
  BucketPolicy'
    { allowsPublicReadAccess =
        Prelude.Nothing,
      allowsPublicWriteAccess = Prelude.Nothing
    }

-- | A value that indicates whether public read access for the bucket is
-- enabled through a bucket policy.
bucketPolicy_allowsPublicReadAccess :: Lens.Lens' BucketPolicy (Prelude.Maybe Prelude.Bool)
bucketPolicy_allowsPublicReadAccess = Lens.lens (\BucketPolicy' {allowsPublicReadAccess} -> allowsPublicReadAccess) (\s@BucketPolicy' {} a -> s {allowsPublicReadAccess = a} :: BucketPolicy)

-- | A value that indicates whether public write access for the bucket is
-- enabled through a bucket policy.
bucketPolicy_allowsPublicWriteAccess :: Lens.Lens' BucketPolicy (Prelude.Maybe Prelude.Bool)
bucketPolicy_allowsPublicWriteAccess = Lens.lens (\BucketPolicy' {allowsPublicWriteAccess} -> allowsPublicWriteAccess) (\s@BucketPolicy' {} a -> s {allowsPublicWriteAccess = a} :: BucketPolicy)

instance Data.FromJSON BucketPolicy where
  parseJSON =
    Data.withObject
      "BucketPolicy"
      ( \x ->
          BucketPolicy'
            Prelude.<$> (x Data..:? "allowsPublicReadAccess")
            Prelude.<*> (x Data..:? "allowsPublicWriteAccess")
      )

instance Prelude.Hashable BucketPolicy where
  hashWithSalt _salt BucketPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` allowsPublicReadAccess
      `Prelude.hashWithSalt` allowsPublicWriteAccess

instance Prelude.NFData BucketPolicy where
  rnf BucketPolicy' {..} =
    Prelude.rnf allowsPublicReadAccess
      `Prelude.seq` Prelude.rnf allowsPublicWriteAccess
