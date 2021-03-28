{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.S3Action
  ( S3Action (..)
  -- * Smart constructor
  , mkS3Action
  -- * Lenses
  , sRoleArn
  , sBucketName
  , sKey
  , sCannedAcl
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.BucketName as Types
import qualified Network.AWS.IoT.Types.CannedAccessControlList as Types
import qualified Network.AWS.IoT.Types.Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to write data to an Amazon S3 bucket.
--
-- /See:/ 'mkS3Action' smart constructor.
data S3Action = S3Action'
  { roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , bucketName :: Types.BucketName
    -- ^ The Amazon S3 bucket.
  , key :: Types.Key
    -- ^ The object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3> .
  , cannedAcl :: Core.Maybe Types.CannedAccessControlList
    -- ^ The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Action' value with any optional fields omitted.
mkS3Action
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.BucketName -- ^ 'bucketName'
    -> Types.Key -- ^ 'key'
    -> S3Action
mkS3Action roleArn bucketName key
  = S3Action'{roleArn, bucketName, key, cannedAcl = Core.Nothing}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRoleArn :: Lens.Lens' S3Action Types.AwsArn
sRoleArn = Lens.field @"roleArn"
{-# INLINEABLE sRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The Amazon S3 bucket.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBucketName :: Lens.Lens' S3Action Types.BucketName
sBucketName = Lens.field @"bucketName"
{-# INLINEABLE sBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | The object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3> .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKey :: Lens.Lens' S3Action Types.Key
sKey = Lens.field @"key"
{-# INLINEABLE sKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
--
-- /Note:/ Consider using 'cannedAcl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCannedAcl :: Lens.Lens' S3Action (Core.Maybe Types.CannedAccessControlList)
sCannedAcl = Lens.field @"cannedAcl"
{-# INLINEABLE sCannedAcl #-}
{-# DEPRECATED cannedAcl "Use generic-lens or generic-optics with 'cannedAcl' instead"  #-}

instance Core.FromJSON S3Action where
        toJSON S3Action{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("bucketName" Core..= bucketName),
                  Core.Just ("key" Core..= key),
                  ("cannedAcl" Core..=) Core.<$> cannedAcl])

instance Core.FromJSON S3Action where
        parseJSON
          = Core.withObject "S3Action" Core.$
              \ x ->
                S3Action' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "bucketName" Core.<*>
                    x Core..: "key"
                    Core.<*> x Core..:? "cannedAcl"
