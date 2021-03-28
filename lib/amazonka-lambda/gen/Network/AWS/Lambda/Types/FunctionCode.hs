{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.FunctionCode
  ( FunctionCode (..)
  -- * Smart constructor
  , mkFunctionCode
  -- * Lenses
  , fcS3Bucket
  , fcS3Key
  , fcS3ObjectVersion
  , fcZipFile
  ) where

import qualified Network.AWS.Lambda.Types.S3Bucket as Types
import qualified Network.AWS.Lambda.Types.S3Key as Types
import qualified Network.AWS.Lambda.Types.S3ObjectVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The code for the Lambda function. You can specify either an object in Amazon S3, or upload a deployment package directly.
--
-- /See:/ 'mkFunctionCode' smart constructor.
data FunctionCode = FunctionCode'
  { s3Bucket :: Core.Maybe Types.S3Bucket
    -- ^ An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
  , s3Key :: Core.Maybe Types.S3Key
    -- ^ The Amazon S3 key of the deployment package.
  , s3ObjectVersion :: Core.Maybe Types.S3ObjectVersion
    -- ^ For versioned objects, the version of the deployment package object to use.
  , zipFile :: Core.Maybe (Core.Sensitive Core.Base64)
    -- ^ The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionCode' value with any optional fields omitted.
mkFunctionCode
    :: FunctionCode
mkFunctionCode
  = FunctionCode'{s3Bucket = Core.Nothing, s3Key = Core.Nothing,
                  s3ObjectVersion = Core.Nothing, zipFile = Core.Nothing}

-- | An Amazon S3 bucket in the same AWS Region as your function. The bucket can be in a different AWS account.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3Bucket :: Lens.Lens' FunctionCode (Core.Maybe Types.S3Bucket)
fcS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE fcS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The Amazon S3 key of the deployment package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3Key :: Lens.Lens' FunctionCode (Core.Maybe Types.S3Key)
fcS3Key = Lens.field @"s3Key"
{-# INLINEABLE fcS3Key #-}
{-# DEPRECATED s3Key "Use generic-lens or generic-optics with 's3Key' instead"  #-}

-- | For versioned objects, the version of the deployment package object to use.
--
-- /Note:/ Consider using 's3ObjectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcS3ObjectVersion :: Lens.Lens' FunctionCode (Core.Maybe Types.S3ObjectVersion)
fcS3ObjectVersion = Lens.field @"s3ObjectVersion"
{-# INLINEABLE fcS3ObjectVersion #-}
{-# DEPRECATED s3ObjectVersion "Use generic-lens or generic-optics with 's3ObjectVersion' instead"  #-}

-- | The base64-encoded contents of the deployment package. AWS SDK and AWS CLI clients handle the encoding for you.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'zipFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcZipFile :: Lens.Lens' FunctionCode (Core.Maybe (Core.Sensitive Core.Base64))
fcZipFile = Lens.field @"zipFile"
{-# INLINEABLE fcZipFile #-}
{-# DEPRECATED zipFile "Use generic-lens or generic-optics with 'zipFile' instead"  #-}

instance Core.FromJSON FunctionCode where
        toJSON FunctionCode{..}
          = Core.object
              (Core.catMaybes
                 [("S3Bucket" Core..=) Core.<$> s3Bucket,
                  ("S3Key" Core..=) Core.<$> s3Key,
                  ("S3ObjectVersion" Core..=) Core.<$> s3ObjectVersion,
                  ("ZipFile" Core..=) Core.<$> zipFile])
