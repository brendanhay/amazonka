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
-- Module      : Amazonka.MacieV2.Types.BucketPublicAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketPublicAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.BucketPermissionConfiguration
import Amazonka.MacieV2.Types.EffectivePermission
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the permissions settings that determine
-- whether an S3 bucket is publicly accessible.
--
-- /See:/ 'newBucketPublicAccess' smart constructor.
data BucketPublicAccess = BucketPublicAccess'
  { -- | The account-level and bucket-level permissions settings for the bucket.
    permissionConfiguration :: Prelude.Maybe BucketPermissionConfiguration,
    -- | Specifies whether the bucket is publicly accessible due to the
    -- combination of permissions settings that apply to the bucket. Possible
    -- values are:
    --
    -- -   NOT_PUBLIC - The bucket isn\'t publicly accessible.
    --
    -- -   PUBLIC - The bucket is publicly accessible.
    --
    -- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket is
    --     publicly accessible.
    effectivePermission :: Prelude.Maybe EffectivePermission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketPublicAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionConfiguration', 'bucketPublicAccess_permissionConfiguration' - The account-level and bucket-level permissions settings for the bucket.
--
-- 'effectivePermission', 'bucketPublicAccess_effectivePermission' - Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket. Possible
-- values are:
--
-- -   NOT_PUBLIC - The bucket isn\'t publicly accessible.
--
-- -   PUBLIC - The bucket is publicly accessible.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket is
--     publicly accessible.
newBucketPublicAccess ::
  BucketPublicAccess
newBucketPublicAccess =
  BucketPublicAccess'
    { permissionConfiguration =
        Prelude.Nothing,
      effectivePermission = Prelude.Nothing
    }

-- | The account-level and bucket-level permissions settings for the bucket.
bucketPublicAccess_permissionConfiguration :: Lens.Lens' BucketPublicAccess (Prelude.Maybe BucketPermissionConfiguration)
bucketPublicAccess_permissionConfiguration = Lens.lens (\BucketPublicAccess' {permissionConfiguration} -> permissionConfiguration) (\s@BucketPublicAccess' {} a -> s {permissionConfiguration = a} :: BucketPublicAccess)

-- | Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket. Possible
-- values are:
--
-- -   NOT_PUBLIC - The bucket isn\'t publicly accessible.
--
-- -   PUBLIC - The bucket is publicly accessible.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket is
--     publicly accessible.
bucketPublicAccess_effectivePermission :: Lens.Lens' BucketPublicAccess (Prelude.Maybe EffectivePermission)
bucketPublicAccess_effectivePermission = Lens.lens (\BucketPublicAccess' {effectivePermission} -> effectivePermission) (\s@BucketPublicAccess' {} a -> s {effectivePermission = a} :: BucketPublicAccess)

instance Core.FromJSON BucketPublicAccess where
  parseJSON =
    Core.withObject
      "BucketPublicAccess"
      ( \x ->
          BucketPublicAccess'
            Prelude.<$> (x Core..:? "permissionConfiguration")
            Prelude.<*> (x Core..:? "effectivePermission")
      )

instance Prelude.Hashable BucketPublicAccess where
  hashWithSalt _salt BucketPublicAccess' {..} =
    _salt
      `Prelude.hashWithSalt` permissionConfiguration
      `Prelude.hashWithSalt` effectivePermission

instance Prelude.NFData BucketPublicAccess where
  rnf BucketPublicAccess' {..} =
    Prelude.rnf permissionConfiguration
      `Prelude.seq` Prelude.rnf effectivePermission
