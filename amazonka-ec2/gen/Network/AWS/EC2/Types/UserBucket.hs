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
-- Module      : Network.AWS.EC2.Types.UserBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucket where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'newUserBucket' smart constructor.
data UserBucket = UserBucket'
  { -- | The name of the Amazon S3 bucket where the disk image is located.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The file name of the disk image.
    s3Key :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'userBucket_s3Bucket' - The name of the Amazon S3 bucket where the disk image is located.
--
-- 's3Key', 'userBucket_s3Key' - The file name of the disk image.
newUserBucket ::
  UserBucket
newUserBucket =
  UserBucket'
    { s3Bucket = Core.Nothing,
      s3Key = Core.Nothing
    }

-- | The name of the Amazon S3 bucket where the disk image is located.
userBucket_s3Bucket :: Lens.Lens' UserBucket (Core.Maybe Core.Text)
userBucket_s3Bucket = Lens.lens (\UserBucket' {s3Bucket} -> s3Bucket) (\s@UserBucket' {} a -> s {s3Bucket = a} :: UserBucket)

-- | The file name of the disk image.
userBucket_s3Key :: Lens.Lens' UserBucket (Core.Maybe Core.Text)
userBucket_s3Key = Lens.lens (\UserBucket' {s3Key} -> s3Key) (\s@UserBucket' {} a -> s {s3Key = a} :: UserBucket)

instance Core.Hashable UserBucket

instance Core.NFData UserBucket

instance Core.ToQuery UserBucket where
  toQuery UserBucket' {..} =
    Core.mconcat
      ["S3Bucket" Core.=: s3Bucket, "S3Key" Core.=: s3Key]
