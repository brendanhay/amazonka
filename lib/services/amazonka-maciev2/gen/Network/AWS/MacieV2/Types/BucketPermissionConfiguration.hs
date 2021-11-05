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
-- Module      : Amazonka.MacieV2.Types.BucketPermissionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketPermissionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.AccountLevelPermissions
import Amazonka.MacieV2.Types.BucketLevelPermissions
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the account-level and bucket-level
-- permissions settings for an S3 bucket.
--
-- /See:/ 'newBucketPermissionConfiguration' smart constructor.
data BucketPermissionConfiguration = BucketPermissionConfiguration'
  { -- | The bucket-level permissions settings for the bucket.
    bucketLevelPermissions :: Prelude.Maybe BucketLevelPermissions,
    -- | The account-level permissions settings that apply to the bucket.
    accountLevelPermissions :: Prelude.Maybe AccountLevelPermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketPermissionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketLevelPermissions', 'bucketPermissionConfiguration_bucketLevelPermissions' - The bucket-level permissions settings for the bucket.
--
-- 'accountLevelPermissions', 'bucketPermissionConfiguration_accountLevelPermissions' - The account-level permissions settings that apply to the bucket.
newBucketPermissionConfiguration ::
  BucketPermissionConfiguration
newBucketPermissionConfiguration =
  BucketPermissionConfiguration'
    { bucketLevelPermissions =
        Prelude.Nothing,
      accountLevelPermissions = Prelude.Nothing
    }

-- | The bucket-level permissions settings for the bucket.
bucketPermissionConfiguration_bucketLevelPermissions :: Lens.Lens' BucketPermissionConfiguration (Prelude.Maybe BucketLevelPermissions)
bucketPermissionConfiguration_bucketLevelPermissions = Lens.lens (\BucketPermissionConfiguration' {bucketLevelPermissions} -> bucketLevelPermissions) (\s@BucketPermissionConfiguration' {} a -> s {bucketLevelPermissions = a} :: BucketPermissionConfiguration)

-- | The account-level permissions settings that apply to the bucket.
bucketPermissionConfiguration_accountLevelPermissions :: Lens.Lens' BucketPermissionConfiguration (Prelude.Maybe AccountLevelPermissions)
bucketPermissionConfiguration_accountLevelPermissions = Lens.lens (\BucketPermissionConfiguration' {accountLevelPermissions} -> accountLevelPermissions) (\s@BucketPermissionConfiguration' {} a -> s {accountLevelPermissions = a} :: BucketPermissionConfiguration)

instance Core.FromJSON BucketPermissionConfiguration where
  parseJSON =
    Core.withObject
      "BucketPermissionConfiguration"
      ( \x ->
          BucketPermissionConfiguration'
            Prelude.<$> (x Core..:? "bucketLevelPermissions")
            Prelude.<*> (x Core..:? "accountLevelPermissions")
      )

instance
  Prelude.Hashable
    BucketPermissionConfiguration

instance Prelude.NFData BucketPermissionConfiguration
