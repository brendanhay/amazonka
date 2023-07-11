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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketPermissionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.AccountLevelPermissions
import Amazonka.MacieV2.Types.BucketLevelPermissions
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the account-level and bucket-level
-- permissions settings for an S3 bucket.
--
-- /See:/ 'newBucketPermissionConfiguration' smart constructor.
data BucketPermissionConfiguration = BucketPermissionConfiguration'
  { -- | The account-level permissions settings that apply to the bucket.
    accountLevelPermissions :: Prelude.Maybe AccountLevelPermissions,
    -- | The bucket-level permissions settings for the bucket.
    bucketLevelPermissions :: Prelude.Maybe BucketLevelPermissions
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
-- 'accountLevelPermissions', 'bucketPermissionConfiguration_accountLevelPermissions' - The account-level permissions settings that apply to the bucket.
--
-- 'bucketLevelPermissions', 'bucketPermissionConfiguration_bucketLevelPermissions' - The bucket-level permissions settings for the bucket.
newBucketPermissionConfiguration ::
  BucketPermissionConfiguration
newBucketPermissionConfiguration =
  BucketPermissionConfiguration'
    { accountLevelPermissions =
        Prelude.Nothing,
      bucketLevelPermissions = Prelude.Nothing
    }

-- | The account-level permissions settings that apply to the bucket.
bucketPermissionConfiguration_accountLevelPermissions :: Lens.Lens' BucketPermissionConfiguration (Prelude.Maybe AccountLevelPermissions)
bucketPermissionConfiguration_accountLevelPermissions = Lens.lens (\BucketPermissionConfiguration' {accountLevelPermissions} -> accountLevelPermissions) (\s@BucketPermissionConfiguration' {} a -> s {accountLevelPermissions = a} :: BucketPermissionConfiguration)

-- | The bucket-level permissions settings for the bucket.
bucketPermissionConfiguration_bucketLevelPermissions :: Lens.Lens' BucketPermissionConfiguration (Prelude.Maybe BucketLevelPermissions)
bucketPermissionConfiguration_bucketLevelPermissions = Lens.lens (\BucketPermissionConfiguration' {bucketLevelPermissions} -> bucketLevelPermissions) (\s@BucketPermissionConfiguration' {} a -> s {bucketLevelPermissions = a} :: BucketPermissionConfiguration)

instance Data.FromJSON BucketPermissionConfiguration where
  parseJSON =
    Data.withObject
      "BucketPermissionConfiguration"
      ( \x ->
          BucketPermissionConfiguration'
            Prelude.<$> (x Data..:? "accountLevelPermissions")
            Prelude.<*> (x Data..:? "bucketLevelPermissions")
      )

instance
  Prelude.Hashable
    BucketPermissionConfiguration
  where
  hashWithSalt _salt BucketPermissionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` accountLevelPermissions
      `Prelude.hashWithSalt` bucketLevelPermissions

instance Prelude.NFData BucketPermissionConfiguration where
  rnf BucketPermissionConfiguration' {..} =
    Prelude.rnf accountLevelPermissions
      `Prelude.seq` Prelude.rnf bucketLevelPermissions
