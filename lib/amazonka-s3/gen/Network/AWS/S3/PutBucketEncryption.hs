{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @PUT@ operation uses the @encryption@ subresource to set the default encryption state of an existing bucket.
--
-- This implementation of the @PUT@ operation sets default encryption for a bucket using server-side encryption with Amazon S3-managed keys SSE-S3 or AWS KMS customer master keys (CMKs) (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
-- /Important:/ This operation requires AWS Signature Version 4. For more information, see <sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)> . 
-- To use this operation, you must have permissions to perform the @s3:PutEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the Amazon Simple Storage Service Developer Guide. 
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption> 
--
--
module Network.AWS.S3.PutBucketEncryption
    (
    -- * Creating a request
      PutBucketEncryption (..)
    , mkPutBucketEncryption
    -- ** Request lenses
    , pbeBucket
    , pbeServerSideEncryptionConfiguration
    , pbeContentMD5
    , pbeExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketEncryptionResponse (..)
    , mkPutBucketEncryptionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
  { bucket :: Types.BucketName
    -- ^ Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
  , serverSideEncryptionConfiguration :: Types.ServerSideEncryptionConfiguration
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketEncryption' value with any optional fields omitted.
mkPutBucketEncryption
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ServerSideEncryptionConfiguration -- ^ 'serverSideEncryptionConfiguration'
    -> PutBucketEncryption
mkPutBucketEncryption bucket serverSideEncryptionConfiguration
  = PutBucketEncryption'{bucket, serverSideEncryptionConfiguration,
                         contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing}

-- | Specifies default encryption for a bucket using server-side encryption with Amazon S3-managed keys (SSE-S3) or customer master keys stored in AWS KMS (SSE-KMS). For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeBucket :: Lens.Lens' PutBucketEncryption Types.BucketName
pbeBucket = Lens.field @"bucket"
{-# INLINEABLE pbeBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'serverSideEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeServerSideEncryptionConfiguration :: Lens.Lens' PutBucketEncryption Types.ServerSideEncryptionConfiguration
pbeServerSideEncryptionConfiguration = Lens.field @"serverSideEncryptionConfiguration"
{-# INLINEABLE pbeServerSideEncryptionConfiguration #-}
{-# DEPRECATED serverSideEncryptionConfiguration "Use generic-lens or generic-optics with 'serverSideEncryptionConfiguration' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the server-side encryption configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeContentMD5 :: Lens.Lens' PutBucketEncryption (Core.Maybe Types.ContentMD5)
pbeContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbeContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbeExpectedBucketOwner :: Lens.Lens' PutBucketEncryption (Core.Maybe Types.ExpectedBucketOwner)
pbeExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbeExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketEncryption where
        toQuery PutBucketEncryption{..}
          = Core.toQueryPair "encryption" ("" :: Core.Text)

instance Core.ToHeaders PutBucketEncryption where
        toHeaders PutBucketEncryption{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketEncryption where
        type Rs PutBucketEncryption = PutBucketEncryptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutBucketEncryptionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse = PutBucketEncryptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketEncryptionResponse' value with any optional fields omitted.
mkPutBucketEncryptionResponse
    :: PutBucketEncryptionResponse
mkPutBucketEncryptionResponse = PutBucketEncryptionResponse'
