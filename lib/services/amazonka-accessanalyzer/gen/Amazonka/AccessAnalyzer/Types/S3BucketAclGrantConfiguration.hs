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
-- Module      : Amazonka.AccessAnalyzer.Types.S3BucketAclGrantConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.S3BucketAclGrantConfiguration where

import Amazonka.AccessAnalyzer.Types.AclGrantee
import Amazonka.AccessAnalyzer.Types.AclPermission
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A proposed access control list grant configuration for an Amazon S3
-- bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#setting-acls How to Specify an ACL>.
--
-- /See:/ 'newS3BucketAclGrantConfiguration' smart constructor.
data S3BucketAclGrantConfiguration = S3BucketAclGrantConfiguration'
  { -- | The permissions being granted.
    permission :: AclPermission,
    -- | The grantee to whom you’re assigning access rights.
    grantee :: AclGrantee
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketAclGrantConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 's3BucketAclGrantConfiguration_permission' - The permissions being granted.
--
-- 'grantee', 's3BucketAclGrantConfiguration_grantee' - The grantee to whom you’re assigning access rights.
newS3BucketAclGrantConfiguration ::
  -- | 'permission'
  AclPermission ->
  -- | 'grantee'
  AclGrantee ->
  S3BucketAclGrantConfiguration
newS3BucketAclGrantConfiguration
  pPermission_
  pGrantee_ =
    S3BucketAclGrantConfiguration'
      { permission =
          pPermission_,
        grantee = pGrantee_
      }

-- | The permissions being granted.
s3BucketAclGrantConfiguration_permission :: Lens.Lens' S3BucketAclGrantConfiguration AclPermission
s3BucketAclGrantConfiguration_permission = Lens.lens (\S3BucketAclGrantConfiguration' {permission} -> permission) (\s@S3BucketAclGrantConfiguration' {} a -> s {permission = a} :: S3BucketAclGrantConfiguration)

-- | The grantee to whom you’re assigning access rights.
s3BucketAclGrantConfiguration_grantee :: Lens.Lens' S3BucketAclGrantConfiguration AclGrantee
s3BucketAclGrantConfiguration_grantee = Lens.lens (\S3BucketAclGrantConfiguration' {grantee} -> grantee) (\s@S3BucketAclGrantConfiguration' {} a -> s {grantee = a} :: S3BucketAclGrantConfiguration)

instance Data.FromJSON S3BucketAclGrantConfiguration where
  parseJSON =
    Data.withObject
      "S3BucketAclGrantConfiguration"
      ( \x ->
          S3BucketAclGrantConfiguration'
            Prelude.<$> (x Data..: "permission")
            Prelude.<*> (x Data..: "grantee")
      )

instance
  Prelude.Hashable
    S3BucketAclGrantConfiguration
  where
  hashWithSalt _salt S3BucketAclGrantConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` grantee

instance Prelude.NFData S3BucketAclGrantConfiguration where
  rnf S3BucketAclGrantConfiguration' {..} =
    Prelude.rnf permission
      `Prelude.seq` Prelude.rnf grantee

instance Data.ToJSON S3BucketAclGrantConfiguration where
  toJSON S3BucketAclGrantConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("permission" Data..= permission),
            Prelude.Just ("grantee" Data..= grantee)
          ]
      )
