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
-- Module      : Network.AWS.IoT.Types.S3Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Action where

import Network.AWS.IoT.Types.CannedAccessControlList
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action to write data to an Amazon S3 bucket.
--
-- /See:/ 'newS3Action' smart constructor.
data S3Action = S3Action'
  { -- | The Amazon S3 canned ACL that controls access to the object identified
    -- by the object key. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs>.
    cannedAcl :: Prelude.Maybe CannedAccessControlList,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Text,
    -- | The Amazon S3 bucket.
    bucketName :: Prelude.Text,
    -- | The object key. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3>.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cannedAcl', 's3Action_cannedAcl' - The Amazon S3 canned ACL that controls access to the object identified
-- by the object key. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs>.
--
-- 'roleArn', 's3Action_roleArn' - The ARN of the IAM role that grants access.
--
-- 'bucketName', 's3Action_bucketName' - The Amazon S3 bucket.
--
-- 'key', 's3Action_key' - The object key. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3>.
newS3Action ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  S3Action
newS3Action pRoleArn_ pBucketName_ pKey_ =
  S3Action'
    { cannedAcl = Prelude.Nothing,
      roleArn = pRoleArn_,
      bucketName = pBucketName_,
      key = pKey_
    }

-- | The Amazon S3 canned ACL that controls access to the object identified
-- by the object key. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs>.
s3Action_cannedAcl :: Lens.Lens' S3Action (Prelude.Maybe CannedAccessControlList)
s3Action_cannedAcl = Lens.lens (\S3Action' {cannedAcl} -> cannedAcl) (\s@S3Action' {} a -> s {cannedAcl = a} :: S3Action)

-- | The ARN of the IAM role that grants access.
s3Action_roleArn :: Lens.Lens' S3Action Prelude.Text
s3Action_roleArn = Lens.lens (\S3Action' {roleArn} -> roleArn) (\s@S3Action' {} a -> s {roleArn = a} :: S3Action)

-- | The Amazon S3 bucket.
s3Action_bucketName :: Lens.Lens' S3Action Prelude.Text
s3Action_bucketName = Lens.lens (\S3Action' {bucketName} -> bucketName) (\s@S3Action' {} a -> s {bucketName = a} :: S3Action)

-- | The object key. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3>.
s3Action_key :: Lens.Lens' S3Action Prelude.Text
s3Action_key = Lens.lens (\S3Action' {key} -> key) (\s@S3Action' {} a -> s {key = a} :: S3Action)

instance Prelude.FromJSON S3Action where
  parseJSON =
    Prelude.withObject
      "S3Action"
      ( \x ->
          S3Action'
            Prelude.<$> (x Prelude..:? "cannedAcl")
            Prelude.<*> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "bucketName")
            Prelude.<*> (x Prelude..: "key")
      )

instance Prelude.Hashable S3Action

instance Prelude.NFData S3Action

instance Prelude.ToJSON S3Action where
  toJSON S3Action' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("cannedAcl" Prelude..=) Prelude.<$> cannedAcl,
            Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just ("bucketName" Prelude..= bucketName),
            Prelude.Just ("key" Prelude..= key)
          ]
      )
