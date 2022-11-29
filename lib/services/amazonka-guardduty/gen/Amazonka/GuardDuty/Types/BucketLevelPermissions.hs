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
-- Module      : Amazonka.GuardDuty.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.BucketLevelPermissions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.AccessControlList
import Amazonka.GuardDuty.Types.BlockPublicAccess
import Amazonka.GuardDuty.Types.BucketPolicy
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the bucket level permissions for the S3
-- bucket.
--
-- /See:/ 'newBucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { -- | Contains information on how Access Control Policies are applied to the
    -- bucket.
    accessControlList :: Prelude.Maybe AccessControlList,
    -- | Contains information on which account level S3 Block Public Access
    -- settings are applied to the S3 bucket.
    blockPublicAccess :: Prelude.Maybe BlockPublicAccess,
    -- | Contains information on the bucket policies for the S3 bucket.
    bucketPolicy :: Prelude.Maybe BucketPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketLevelPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'bucketLevelPermissions_accessControlList' - Contains information on how Access Control Policies are applied to the
-- bucket.
--
-- 'blockPublicAccess', 'bucketLevelPermissions_blockPublicAccess' - Contains information on which account level S3 Block Public Access
-- settings are applied to the S3 bucket.
--
-- 'bucketPolicy', 'bucketLevelPermissions_bucketPolicy' - Contains information on the bucket policies for the S3 bucket.
newBucketLevelPermissions ::
  BucketLevelPermissions
newBucketLevelPermissions =
  BucketLevelPermissions'
    { accessControlList =
        Prelude.Nothing,
      blockPublicAccess = Prelude.Nothing,
      bucketPolicy = Prelude.Nothing
    }

-- | Contains information on how Access Control Policies are applied to the
-- bucket.
bucketLevelPermissions_accessControlList :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe AccessControlList)
bucketLevelPermissions_accessControlList = Lens.lens (\BucketLevelPermissions' {accessControlList} -> accessControlList) (\s@BucketLevelPermissions' {} a -> s {accessControlList = a} :: BucketLevelPermissions)

-- | Contains information on which account level S3 Block Public Access
-- settings are applied to the S3 bucket.
bucketLevelPermissions_blockPublicAccess :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BlockPublicAccess)
bucketLevelPermissions_blockPublicAccess = Lens.lens (\BucketLevelPermissions' {blockPublicAccess} -> blockPublicAccess) (\s@BucketLevelPermissions' {} a -> s {blockPublicAccess = a} :: BucketLevelPermissions)

-- | Contains information on the bucket policies for the S3 bucket.
bucketLevelPermissions_bucketPolicy :: Lens.Lens' BucketLevelPermissions (Prelude.Maybe BucketPolicy)
bucketLevelPermissions_bucketPolicy = Lens.lens (\BucketLevelPermissions' {bucketPolicy} -> bucketPolicy) (\s@BucketLevelPermissions' {} a -> s {bucketPolicy = a} :: BucketLevelPermissions)

instance Core.FromJSON BucketLevelPermissions where
  parseJSON =
    Core.withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            Prelude.<$> (x Core..:? "accessControlList")
            Prelude.<*> (x Core..:? "blockPublicAccess")
            Prelude.<*> (x Core..:? "bucketPolicy")
      )

instance Prelude.Hashable BucketLevelPermissions where
  hashWithSalt _salt BucketLevelPermissions' {..} =
    _salt `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` blockPublicAccess
      `Prelude.hashWithSalt` bucketPolicy

instance Prelude.NFData BucketLevelPermissions where
  rnf BucketLevelPermissions' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf blockPublicAccess
      `Prelude.seq` Prelude.rnf bucketPolicy
