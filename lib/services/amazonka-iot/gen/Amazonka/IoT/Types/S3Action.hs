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
-- Module      : Amazonka.IoT.Types.S3Action
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.S3Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CannedAccessControlList
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON S3Action where
  parseJSON =
    Data.withObject
      "S3Action"
      ( \x ->
          S3Action'
            Prelude.<$> (x Data..:? "cannedAcl")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable S3Action where
  hashWithSalt _salt S3Action' {..} =
    _salt `Prelude.hashWithSalt` cannedAcl
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` key

instance Prelude.NFData S3Action where
  rnf S3Action' {..} =
    Prelude.rnf cannedAcl
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON S3Action where
  toJSON S3Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cannedAcl" Data..=) Prelude.<$> cannedAcl,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("key" Data..= key)
          ]
      )
