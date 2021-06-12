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
-- Module      : Network.AWS.EC2.Types.UserBucketDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucketDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'newUserBucketDetails' smart constructor.
data UserBucketDetails = UserBucketDetails'
  { -- | The Amazon S3 bucket from which the disk image was created.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The file name of the disk image.
    s3Key :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserBucketDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'userBucketDetails_s3Bucket' - The Amazon S3 bucket from which the disk image was created.
--
-- 's3Key', 'userBucketDetails_s3Key' - The file name of the disk image.
newUserBucketDetails ::
  UserBucketDetails
newUserBucketDetails =
  UserBucketDetails'
    { s3Bucket = Core.Nothing,
      s3Key = Core.Nothing
    }

-- | The Amazon S3 bucket from which the disk image was created.
userBucketDetails_s3Bucket :: Lens.Lens' UserBucketDetails (Core.Maybe Core.Text)
userBucketDetails_s3Bucket = Lens.lens (\UserBucketDetails' {s3Bucket} -> s3Bucket) (\s@UserBucketDetails' {} a -> s {s3Bucket = a} :: UserBucketDetails)

-- | The file name of the disk image.
userBucketDetails_s3Key :: Lens.Lens' UserBucketDetails (Core.Maybe Core.Text)
userBucketDetails_s3Key = Lens.lens (\UserBucketDetails' {s3Key} -> s3Key) (\s@UserBucketDetails' {} a -> s {s3Key = a} :: UserBucketDetails)

instance Core.FromXML UserBucketDetails where
  parseXML x =
    UserBucketDetails'
      Core.<$> (x Core..@? "s3Bucket") Core.<*> (x Core..@? "s3Key")

instance Core.Hashable UserBucketDetails

instance Core.NFData UserBucketDetails
