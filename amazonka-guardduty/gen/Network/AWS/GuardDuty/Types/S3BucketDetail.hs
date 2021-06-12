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
-- Module      : Network.AWS.GuardDuty.Types.S3BucketDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3BucketDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Owner
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains information on the S3 bucket.
--
-- /See:/ 'newS3BucketDetail' smart constructor.
data S3BucketDetail = S3BucketDetail'
  { -- | The Amazon Resource Name (ARN) of the S3 bucket.
    arn :: Core.Maybe Core.Text,
    -- | Describes the public access policies that apply to the S3 bucket.
    publicAccess :: Core.Maybe PublicAccess,
    -- | The date and time the bucket was created at.
    createdAt :: Core.Maybe Core.POSIX,
    -- | Describes the server side encryption method used in the S3 bucket.
    defaultServerSideEncryption :: Core.Maybe DefaultServerSideEncryption,
    -- | The name of the S3 bucket.
    name :: Core.Maybe Core.Text,
    -- | All tags attached to the S3 bucket
    tags :: Core.Maybe [Tag],
    -- | The owner of the S3 bucket.
    owner :: Core.Maybe Owner,
    -- | Describes whether the bucket is a source or destination bucket.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3BucketDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 's3BucketDetail_arn' - The Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'publicAccess', 's3BucketDetail_publicAccess' - Describes the public access policies that apply to the S3 bucket.
--
-- 'createdAt', 's3BucketDetail_createdAt' - The date and time the bucket was created at.
--
-- 'defaultServerSideEncryption', 's3BucketDetail_defaultServerSideEncryption' - Describes the server side encryption method used in the S3 bucket.
--
-- 'name', 's3BucketDetail_name' - The name of the S3 bucket.
--
-- 'tags', 's3BucketDetail_tags' - All tags attached to the S3 bucket
--
-- 'owner', 's3BucketDetail_owner' - The owner of the S3 bucket.
--
-- 'type'', 's3BucketDetail_type' - Describes whether the bucket is a source or destination bucket.
newS3BucketDetail ::
  S3BucketDetail
newS3BucketDetail =
  S3BucketDetail'
    { arn = Core.Nothing,
      publicAccess = Core.Nothing,
      createdAt = Core.Nothing,
      defaultServerSideEncryption = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      owner = Core.Nothing,
      type' = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3BucketDetail_arn :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
s3BucketDetail_arn = Lens.lens (\S3BucketDetail' {arn} -> arn) (\s@S3BucketDetail' {} a -> s {arn = a} :: S3BucketDetail)

-- | Describes the public access policies that apply to the S3 bucket.
s3BucketDetail_publicAccess :: Lens.Lens' S3BucketDetail (Core.Maybe PublicAccess)
s3BucketDetail_publicAccess = Lens.lens (\S3BucketDetail' {publicAccess} -> publicAccess) (\s@S3BucketDetail' {} a -> s {publicAccess = a} :: S3BucketDetail)

-- | The date and time the bucket was created at.
s3BucketDetail_createdAt :: Lens.Lens' S3BucketDetail (Core.Maybe Core.UTCTime)
s3BucketDetail_createdAt = Lens.lens (\S3BucketDetail' {createdAt} -> createdAt) (\s@S3BucketDetail' {} a -> s {createdAt = a} :: S3BucketDetail) Core.. Lens.mapping Core._Time

-- | Describes the server side encryption method used in the S3 bucket.
s3BucketDetail_defaultServerSideEncryption :: Lens.Lens' S3BucketDetail (Core.Maybe DefaultServerSideEncryption)
s3BucketDetail_defaultServerSideEncryption = Lens.lens (\S3BucketDetail' {defaultServerSideEncryption} -> defaultServerSideEncryption) (\s@S3BucketDetail' {} a -> s {defaultServerSideEncryption = a} :: S3BucketDetail)

-- | The name of the S3 bucket.
s3BucketDetail_name :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
s3BucketDetail_name = Lens.lens (\S3BucketDetail' {name} -> name) (\s@S3BucketDetail' {} a -> s {name = a} :: S3BucketDetail)

-- | All tags attached to the S3 bucket
s3BucketDetail_tags :: Lens.Lens' S3BucketDetail (Core.Maybe [Tag])
s3BucketDetail_tags = Lens.lens (\S3BucketDetail' {tags} -> tags) (\s@S3BucketDetail' {} a -> s {tags = a} :: S3BucketDetail) Core.. Lens.mapping Lens._Coerce

-- | The owner of the S3 bucket.
s3BucketDetail_owner :: Lens.Lens' S3BucketDetail (Core.Maybe Owner)
s3BucketDetail_owner = Lens.lens (\S3BucketDetail' {owner} -> owner) (\s@S3BucketDetail' {} a -> s {owner = a} :: S3BucketDetail)

-- | Describes whether the bucket is a source or destination bucket.
s3BucketDetail_type :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
s3BucketDetail_type = Lens.lens (\S3BucketDetail' {type'} -> type') (\s@S3BucketDetail' {} a -> s {type' = a} :: S3BucketDetail)

instance Core.FromJSON S3BucketDetail where
  parseJSON =
    Core.withObject
      "S3BucketDetail"
      ( \x ->
          S3BucketDetail'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "publicAccess")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "defaultServerSideEncryption")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "owner")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable S3BucketDetail

instance Core.NFData S3BucketDetail
