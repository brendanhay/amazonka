{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.S3Location
  ( S3Location (..)
  -- * Smart constructor
  , mkS3Location
  -- * Lenses
  , slBucket
  , slKey
  , slObjectVersion
  , slRoleArn
  ) where

import qualified Network.AWS.GameLift.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location in S3 where build or script files are stored for access by Amazon GameLift. This location is specified in 'CreateBuild' , 'CreateScript' , and 'UpdateScript' requests. 
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bucket :: Core.Maybe Types.NonEmptyString
    -- ^ An S3 bucket identifier. This is the name of the S3 bucket.
  , key :: Core.Maybe Types.NonEmptyString
    -- ^ The name of the zip file that contains the build files or script files. 
  , objectVersion :: Core.Maybe Types.NonEmptyString
    -- ^ The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved. 
  , roleArn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location
    :: S3Location
mkS3Location
  = S3Location'{bucket = Core.Nothing, key = Core.Nothing,
                objectVersion = Core.Nothing, roleArn = Core.Nothing}

-- | An S3 bucket identifier. This is the name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Core.Maybe Types.NonEmptyString)
slBucket = Lens.field @"bucket"
{-# INLINEABLE slBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The name of the zip file that contains the build files or script files. 
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Core.Maybe Types.NonEmptyString)
slKey = Lens.field @"key"
{-# INLINEABLE slKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved. 
--
-- /Note:/ Consider using 'objectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slObjectVersion :: Lens.Lens' S3Location (Core.Maybe Types.NonEmptyString)
slObjectVersion = Lens.field @"objectVersion"
{-# INLINEABLE slObjectVersion #-}
{-# DEPRECATED objectVersion "Use generic-lens or generic-optics with 'objectVersion' instead"  #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slRoleArn :: Lens.Lens' S3Location (Core.Maybe Types.NonEmptyString)
slRoleArn = Lens.field @"roleArn"
{-# INLINEABLE slRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON S3Location where
        toJSON S3Location{..}
          = Core.object
              (Core.catMaybes
                 [("Bucket" Core..=) Core.<$> bucket, ("Key" Core..=) Core.<$> key,
                  ("ObjectVersion" Core..=) Core.<$> objectVersion,
                  ("RoleArn" Core..=) Core.<$> roleArn])

instance Core.FromJSON S3Location where
        parseJSON
          = Core.withObject "S3Location" Core.$
              \ x ->
                S3Location' Core.<$>
                  (x Core..:? "Bucket") Core.<*> x Core..:? "Key" Core.<*>
                    x Core..:? "ObjectVersion"
                    Core.<*> x Core..:? "RoleArn"
