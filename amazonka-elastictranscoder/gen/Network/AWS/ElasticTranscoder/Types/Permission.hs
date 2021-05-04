{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticTranscoder.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Permission where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @Permission@ structure.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | The permission that you want to give to the AWS user that is listed in
    -- Grantee. Valid values include:
    --
    -- -   @READ@: The grantee can read the thumbnails and metadata for
    --     thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
    --
    -- -   @READ_ACP@: The grantee can read the object ACL for thumbnails that
    --     Elastic Transcoder adds to the Amazon S3 bucket.
    --
    -- -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails that
    --     Elastic Transcoder adds to the Amazon S3 bucket.
    --
    -- -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and WRITE_ACP
    --     permissions for the thumbnails that Elastic Transcoder adds to the
    --     Amazon S3 bucket.
    access :: Prelude.Maybe [Prelude.Text],
    -- | The type of value that appears in the Grantee object:
    --
    -- -   @Canonical@: Either the canonical user ID for an AWS account or an
    --     origin access identity for an Amazon CloudFront distribution.
    --
    --     A canonical user ID is not the same as an AWS account number.
    --
    -- -   @Email@: The registered email address of an AWS account.
    --
    -- -   @Group@: One of the following predefined Amazon S3 groups:
    --     @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
    granteeType :: Prelude.Maybe Prelude.Text,
    -- | The AWS user or group that you want to have access to transcoded files
    -- and playlists. To identify the user or group, you can specify the
    -- canonical user ID for an AWS account, an origin access identity for a
    -- CloudFront distribution, the registered email address of an AWS account,
    -- or a predefined Amazon S3 group.
    grantee :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Permission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'access', 'permission_access' - The permission that you want to give to the AWS user that is listed in
-- Grantee. Valid values include:
--
-- -   @READ@: The grantee can read the thumbnails and metadata for
--     thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @READ_ACP@: The grantee can read the object ACL for thumbnails that
--     Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails that
--     Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and WRITE_ACP
--     permissions for the thumbnails that Elastic Transcoder adds to the
--     Amazon S3 bucket.
--
-- 'granteeType', 'permission_granteeType' - The type of value that appears in the Grantee object:
--
-- -   @Canonical@: Either the canonical user ID for an AWS account or an
--     origin access identity for an Amazon CloudFront distribution.
--
--     A canonical user ID is not the same as an AWS account number.
--
-- -   @Email@: The registered email address of an AWS account.
--
-- -   @Group@: One of the following predefined Amazon S3 groups:
--     @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
--
-- 'grantee', 'permission_grantee' - The AWS user or group that you want to have access to transcoded files
-- and playlists. To identify the user or group, you can specify the
-- canonical user ID for an AWS account, an origin access identity for a
-- CloudFront distribution, the registered email address of an AWS account,
-- or a predefined Amazon S3 group.
newPermission ::
  Permission
newPermission =
  Permission'
    { access = Prelude.Nothing,
      granteeType = Prelude.Nothing,
      grantee = Prelude.Nothing
    }

-- | The permission that you want to give to the AWS user that is listed in
-- Grantee. Valid values include:
--
-- -   @READ@: The grantee can read the thumbnails and metadata for
--     thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @READ_ACP@: The grantee can read the object ACL for thumbnails that
--     Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @WRITE_ACP@: The grantee can write the ACL for the thumbnails that
--     Elastic Transcoder adds to the Amazon S3 bucket.
--
-- -   @FULL_CONTROL@: The grantee has READ, READ_ACP, and WRITE_ACP
--     permissions for the thumbnails that Elastic Transcoder adds to the
--     Amazon S3 bucket.
permission_access :: Lens.Lens' Permission (Prelude.Maybe [Prelude.Text])
permission_access = Lens.lens (\Permission' {access} -> access) (\s@Permission' {} a -> s {access = a} :: Permission) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of value that appears in the Grantee object:
--
-- -   @Canonical@: Either the canonical user ID for an AWS account or an
--     origin access identity for an Amazon CloudFront distribution.
--
--     A canonical user ID is not the same as an AWS account number.
--
-- -   @Email@: The registered email address of an AWS account.
--
-- -   @Group@: One of the following predefined Amazon S3 groups:
--     @AllUsers@, @AuthenticatedUsers@, or @LogDelivery@.
permission_granteeType :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_granteeType = Lens.lens (\Permission' {granteeType} -> granteeType) (\s@Permission' {} a -> s {granteeType = a} :: Permission)

-- | The AWS user or group that you want to have access to transcoded files
-- and playlists. To identify the user or group, you can specify the
-- canonical user ID for an AWS account, an origin access identity for a
-- CloudFront distribution, the registered email address of an AWS account,
-- or a predefined Amazon S3 group.
permission_grantee :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_grantee = Lens.lens (\Permission' {grantee} -> grantee) (\s@Permission' {} a -> s {grantee = a} :: Permission)

instance Prelude.FromJSON Permission where
  parseJSON =
    Prelude.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Prelude..:? "Access" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "GranteeType")
            Prelude.<*> (x Prelude..:? "Grantee")
      )

instance Prelude.Hashable Permission

instance Prelude.NFData Permission

instance Prelude.ToJSON Permission where
  toJSON Permission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Access" Prelude..=) Prelude.<$> access,
            ("GranteeType" Prelude..=) Prelude.<$> granteeType,
            ("Grantee" Prelude..=) Prelude.<$> grantee
          ]
      )
