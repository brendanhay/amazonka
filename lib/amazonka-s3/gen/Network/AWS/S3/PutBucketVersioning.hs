{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
--
-- You can set the versioning state with one of the following values:
-- __Enabled__ —Enables versioning for the objects in the bucket. All objects added to the bucket receive a unique version ID.
-- __Suspended__ —Disables versioning for the objects in the bucket. All objects added to the bucket receive the version ID null.
-- If the versioning state has never been set on a bucket, it has no versioning state; a <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> request does not return a versioning state value.
-- If the bucket owner enables MFA Delete in the bucket versioning configuration, the bucket owner must include the @x-amz-mfa request@ header and the @Status@ and the @MfaDelete@ request elements in a request to set the versioning state of the bucket.
-- /Important:/ If you have an object expiration lifecycle policy in your non-versioned bucket and you want to maintain the same permanent delete behavior when you enable versioning, you must add a noncurrent expiration policy. The noncurrent expiration lifecycle policy will manage the deletes of the noncurrent object versions in the version-enabled bucket. (A version-enabled bucket maintains one current and zero or more noncurrent object versions.) For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-and-other-bucket-config Lifecycle and Versioning> .
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> 
--
--
module Network.AWS.S3.PutBucketVersioning
    (
    -- * Creating a request
      PutBucketVersioning (..)
    , mkPutBucketVersioning
    -- ** Request lenses
    , pbvBucket
    , pbvVersioningConfiguration
    , pbvContentMD5
    , pbvExpectedBucketOwner
    , pbvMFA

    -- * Destructuring the response
    , PutBucketVersioningResponse (..)
    , mkPutBucketVersioningResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketVersioning' smart constructor.
data PutBucketVersioning = PutBucketVersioning'
  { bucket :: Types.BucketName
    -- ^ The bucket name.
  , versioningConfiguration :: Types.VersioningConfiguration
    -- ^ Container for setting the versioning state.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , mfa :: Core.Maybe Types.MFA
    -- ^ The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketVersioning' value with any optional fields omitted.
mkPutBucketVersioning
    :: Types.BucketName -- ^ 'bucket'
    -> Types.VersioningConfiguration -- ^ 'versioningConfiguration'
    -> PutBucketVersioning
mkPutBucketVersioning bucket versioningConfiguration
  = PutBucketVersioning'{bucket, versioningConfiguration,
                         contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing,
                         mfa = Core.Nothing}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvBucket :: Lens.Lens' PutBucketVersioning Types.BucketName
pbvBucket = Lens.field @"bucket"
{-# INLINEABLE pbvBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Container for setting the versioning state.
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvVersioningConfiguration :: Lens.Lens' PutBucketVersioning Types.VersioningConfiguration
pbvVersioningConfiguration = Lens.field @"versioningConfiguration"
{-# INLINEABLE pbvVersioningConfiguration #-}
{-# DEPRECATED versioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead"  #-}

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvContentMD5 :: Lens.Lens' PutBucketVersioning (Core.Maybe Types.ContentMD5)
pbvContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbvContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvExpectedBucketOwner :: Lens.Lens' PutBucketVersioning (Core.Maybe Types.AccountId)
pbvExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbvExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvMFA :: Lens.Lens' PutBucketVersioning (Core.Maybe Types.MFA)
pbvMFA = Lens.field @"mfa"
{-# INLINEABLE pbvMFA #-}
{-# DEPRECATED mfa "Use generic-lens or generic-optics with 'mfa' instead"  #-}

instance Core.ToQuery PutBucketVersioning where
        toQuery PutBucketVersioning{..}
          = Core.toQueryPair "versioning" ("" :: Core.Text)

instance Core.ToHeaders PutBucketVersioning where
        toHeaders PutBucketVersioning{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-mfa" mfa

instance Core.AWSRequest PutBucketVersioning where
        type Rs PutBucketVersioning = PutBucketVersioningResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutBucketVersioningResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse = PutBucketVersioningResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketVersioningResponse' value with any optional fields omitted.
mkPutBucketVersioningResponse
    :: PutBucketVersioningResponse
mkPutBucketVersioningResponse = PutBucketVersioningResponse'
