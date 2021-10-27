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
-- Module      : Network.AWS.MacieV2.Types.BucketPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.BucketPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the permissions settings of the bucket policy
-- for an S3 bucket.
--
-- /See:/ 'newBucketPolicy' smart constructor.
data BucketPolicy = BucketPolicy'
  { -- | Specifies whether the bucket policy allows the general public to have
    -- write access to the bucket.
    allowsPublicWriteAccess :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the bucket policy allows the general public to have
    -- read access to the bucket.
    allowsPublicReadAccess :: Prelude.Maybe Prelude.Bool
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
-- 'allowsPublicWriteAccess', 'bucketPolicy_allowsPublicWriteAccess' - Specifies whether the bucket policy allows the general public to have
-- write access to the bucket.
--
-- 'allowsPublicReadAccess', 'bucketPolicy_allowsPublicReadAccess' - Specifies whether the bucket policy allows the general public to have
-- read access to the bucket.
newBucketPolicy ::
  BucketPolicy
newBucketPolicy =
  BucketPolicy'
    { allowsPublicWriteAccess =
        Prelude.Nothing,
      allowsPublicReadAccess = Prelude.Nothing
    }

-- | Specifies whether the bucket policy allows the general public to have
-- write access to the bucket.
bucketPolicy_allowsPublicWriteAccess :: Lens.Lens' BucketPolicy (Prelude.Maybe Prelude.Bool)
bucketPolicy_allowsPublicWriteAccess = Lens.lens (\BucketPolicy' {allowsPublicWriteAccess} -> allowsPublicWriteAccess) (\s@BucketPolicy' {} a -> s {allowsPublicWriteAccess = a} :: BucketPolicy)

-- | Specifies whether the bucket policy allows the general public to have
-- read access to the bucket.
bucketPolicy_allowsPublicReadAccess :: Lens.Lens' BucketPolicy (Prelude.Maybe Prelude.Bool)
bucketPolicy_allowsPublicReadAccess = Lens.lens (\BucketPolicy' {allowsPublicReadAccess} -> allowsPublicReadAccess) (\s@BucketPolicy' {} a -> s {allowsPublicReadAccess = a} :: BucketPolicy)

instance Core.FromJSON BucketPolicy where
  parseJSON =
    Core.withObject
      "BucketPolicy"
      ( \x ->
          BucketPolicy'
            Prelude.<$> (x Core..:? "allowsPublicWriteAccess")
            Prelude.<*> (x Core..:? "allowsPublicReadAccess")
      )

instance Prelude.Hashable BucketPolicy

instance Prelude.NFData BucketPolicy
