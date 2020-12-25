{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns torrent files from a bucket. BitTorrent can save you bandwidth when you're distributing large files. For more information about BitTorrent, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Using BitTorrent with Amazon S3> .
--
-- To use GET, you must have READ access to the object.
-- This action is not supported by Amazon S3 on Outposts.
-- The following operation is related to @GetObjectTorrent@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.GetObjectTorrent
  ( -- * Creating a request
    GetObjectTorrent (..),
    mkGetObjectTorrent,

    -- ** Request lenses
    gotBucket,
    gotKey,
    gotExpectedBucketOwner,
    gotRequestPayer,

    -- * Destructuring the response
    GetObjectTorrentResponse (..),
    mkGetObjectTorrentResponse,

    -- ** Response lenses
    gotrrsBody,
    gotrrsRequestCharged,
    gotrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObjectTorrent' smart constructor.
data GetObjectTorrent = GetObjectTorrent'
  { -- | The name of the bucket containing the object for which to get the torrent files.
    bucket :: Types.BucketName,
    -- | The object key for which to get the information.
    key :: Types.Key,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectTorrent' value with any optional fields omitted.
mkGetObjectTorrent ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'key'
  Types.Key ->
  GetObjectTorrent
mkGetObjectTorrent bucket key =
  GetObjectTorrent'
    { bucket,
      key,
      expectedBucketOwner = Core.Nothing,
      requestPayer = Core.Nothing
    }

-- | The name of the bucket containing the object for which to get the torrent files.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotBucket :: Lens.Lens' GetObjectTorrent Types.BucketName
gotBucket = Lens.field @"bucket"
{-# DEPRECATED gotBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The object key for which to get the information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotKey :: Lens.Lens' GetObjectTorrent Types.Key
gotKey = Lens.field @"key"
{-# DEPRECATED gotKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotExpectedBucketOwner :: Lens.Lens' GetObjectTorrent (Core.Maybe Types.ExpectedBucketOwner)
gotExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED gotExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotRequestPayer :: Lens.Lens' GetObjectTorrent (Core.Maybe Types.RequestPayer)
gotRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED gotRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

instance Core.AWSRequest GetObjectTorrent where
  type Rs GetObjectTorrent = GetObjectTorrentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText bucket) Core.<> ("/")
                Core.<> (Core.toText key)
            ),
        Core._rqQuery = Core.pure ("torrent", ""),
        Core._rqHeaders =
          Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
            Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
        Core._rqBody = ""
      }
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectTorrentResponse'
            Core.<$> (Core.pure x)
            Core.<*> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetObjectTorrentResponse' smart constructor.
data GetObjectTorrentResponse = GetObjectTorrentResponse'
  { -- | A Bencoded dictionary as defined by the BitTorrent specification
    body :: Core.RsBody,
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'GetObjectTorrentResponse' value with any optional fields omitted.
mkGetObjectTorrentResponse ::
  -- | 'body'
  Core.RsBody ->
  -- | 'responseStatus'
  Core.Int ->
  GetObjectTorrentResponse
mkGetObjectTorrentResponse body responseStatus =
  GetObjectTorrentResponse'
    { body,
      requestCharged = Core.Nothing,
      responseStatus
    }

-- | A Bencoded dictionary as defined by the BitTorrent specification
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotrrsBody :: Lens.Lens' GetObjectTorrentResponse Core.RsBody
gotrrsBody = Lens.field @"body"
{-# DEPRECATED gotrrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotrrsRequestCharged :: Lens.Lens' GetObjectTorrentResponse (Core.Maybe Types.RequestCharged)
gotrrsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED gotrrsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gotrrsResponseStatus :: Lens.Lens' GetObjectTorrentResponse Core.Int
gotrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gotrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
