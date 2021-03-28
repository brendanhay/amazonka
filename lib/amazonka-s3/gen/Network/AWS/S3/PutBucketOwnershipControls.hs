{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketOwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies @OwnershipControls@ for an Amazon S3 bucket. To use this operation, you must have the @s3:PutBucketOwnershipControls@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> . 
--
-- For information about Amazon S3 Object Ownership, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership> . 
-- The following operations are related to @PutBucketOwnershipControls@ :
--
--     * 'GetBucketOwnershipControls' 
--
--
--     * 'DeleteBucketOwnershipControls' 
--
--
module Network.AWS.S3.PutBucketOwnershipControls
    (
    -- * Creating a request
      PutBucketOwnershipControls (..)
    , mkPutBucketOwnershipControls
    -- ** Request lenses
    , pbocBucket
    , pbocOwnershipControls
    , pbocContentMD5
    , pbocExpectedBucketOwner

    -- * Destructuring the response
    , PutBucketOwnershipControlsResponse (..)
    , mkPutBucketOwnershipControlsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketOwnershipControls' smart constructor.
data PutBucketOwnershipControls = PutBucketOwnershipControls'
  { bucket :: Types.BucketName
    -- ^ The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
  , ownershipControls :: Types.OwnershipControls
    -- ^ The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The MD5 hash of the @OwnershipControls@ request body. 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketOwnershipControls' value with any optional fields omitted.
mkPutBucketOwnershipControls
    :: Types.BucketName -- ^ 'bucket'
    -> Types.OwnershipControls -- ^ 'ownershipControls'
    -> PutBucketOwnershipControls
mkPutBucketOwnershipControls bucket ownershipControls
  = PutBucketOwnershipControls'{bucket, ownershipControls,
                                contentMD5 = Core.Nothing, expectedBucketOwner = Core.Nothing}

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to set.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocBucket :: Lens.Lens' PutBucketOwnershipControls Types.BucketName
pbocBucket = Lens.field @"bucket"
{-# INLINEABLE pbocBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you want to apply to this Amazon S3 bucket.
--
-- /Note:/ Consider using 'ownershipControls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocOwnershipControls :: Lens.Lens' PutBucketOwnershipControls Types.OwnershipControls
pbocOwnershipControls = Lens.field @"ownershipControls"
{-# INLINEABLE pbocOwnershipControls #-}
{-# DEPRECATED ownershipControls "Use generic-lens or generic-optics with 'ownershipControls' instead"  #-}

-- | The MD5 hash of the @OwnershipControls@ request body. 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocContentMD5 :: Lens.Lens' PutBucketOwnershipControls (Core.Maybe Types.ContentMD5)
pbocContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbocContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbocExpectedBucketOwner :: Lens.Lens' PutBucketOwnershipControls (Core.Maybe Types.AccountId)
pbocExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbocExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery PutBucketOwnershipControls where
        toQuery PutBucketOwnershipControls{..}
          = Core.toQueryPair "ownershipControls" ("" :: Core.Text)

instance Core.ToHeaders PutBucketOwnershipControls where
        toHeaders PutBucketOwnershipControls{..}
          = Core.toHeaders "Content-MD5" contentMD5 Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest PutBucketOwnershipControls where
        type Rs PutBucketOwnershipControls =
             PutBucketOwnershipControlsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull PutBucketOwnershipControlsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketOwnershipControlsResponse' smart constructor.
data PutBucketOwnershipControlsResponse = PutBucketOwnershipControlsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketOwnershipControlsResponse' value with any optional fields omitted.
mkPutBucketOwnershipControlsResponse
    :: PutBucketOwnershipControlsResponse
mkPutBucketOwnershipControlsResponse
  = PutBucketOwnershipControlsResponse'
