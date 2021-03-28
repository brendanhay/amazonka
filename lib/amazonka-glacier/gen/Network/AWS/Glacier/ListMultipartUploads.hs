{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListMultipartUploads
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads for the specified vault. An in-progress multipart upload is a multipart upload that has been initiated by an 'InitiateMultipartUpload' request, but has not yet been completed or aborted. The list returned in the List Multipart Upload response has no guaranteed order. 
--
-- The List Multipart Uploads operation supports pagination. By default, this operation returns up to 50 multipart uploads in the response. You should always check the response for a @marker@ at which to continue the list; if there are no more items the @marker@ is @null@ . To return a list of multipart uploads that begins at a specific upload, set the @marker@ request parameter to the value you obtained from a previous List Multipart Upload request. You can also limit the number of uploads returned in the response by specifying the @limit@ parameter in the request.
-- Note the difference between this operation and listing parts ('ListParts' ). The List Multipart Uploads operation lists all multipart uploads for a vault and does not require a multipart upload ID. The List Parts operation requires a multipart upload ID since parts are associated with a single upload.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html List Multipart Uploads > in the /Amazon Glacier Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListMultipartUploads
    (
    -- * Creating a request
      ListMultipartUploads (..)
    , mkListMultipartUploads
    -- ** Request lenses
    , lmuAccountId
    , lmuVaultName
    , lmuLimit
    , lmuMarker

    -- * Destructuring the response
    , ListMultipartUploadsResponse (..)
    , mkListMultipartUploadsResponse
    -- ** Response lenses
    , lmurrsMarker
    , lmurrsUploadsList
    , lmurrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for retrieving list of in-progress multipart uploads for an Amazon Glacier vault.
--
-- /See:/ 'mkListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , limit :: Core.Maybe Core.Text
    -- ^ Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 50 uploads.
  , marker :: Core.Maybe Core.Text
    -- ^ An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultipartUploads' value with any optional fields omitted.
mkListMultipartUploads
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> ListMultipartUploads
mkListMultipartUploads accountId vaultName
  = ListMultipartUploads'{accountId, vaultName, limit = Core.Nothing,
                          marker = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuAccountId :: Lens.Lens' ListMultipartUploads Core.Text
lmuAccountId = Lens.field @"accountId"
{-# INLINEABLE lmuAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuVaultName :: Lens.Lens' ListMultipartUploads Core.Text
lmuVaultName = Lens.field @"vaultName"
{-# INLINEABLE lmuVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 50 uploads.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuLimit :: Lens.Lens' ListMultipartUploads (Core.Maybe Core.Text)
lmuLimit = Lens.field @"limit"
{-# INLINEABLE lmuLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuMarker :: Lens.Lens' ListMultipartUploads (Core.Maybe Core.Text)
lmuMarker = Lens.field @"marker"
{-# INLINEABLE lmuMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery ListMultipartUploads where
        toQuery ListMultipartUploads{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "marker") marker

instance Core.ToHeaders ListMultipartUploads where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListMultipartUploads where
        type Rs ListMultipartUploads = ListMultipartUploadsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/multipart-uploads",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMultipartUploadsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "UploadsList" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMultipartUploads where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"uploadsList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
  , uploadsList :: Core.Maybe [Types.UploadListElement]
    -- ^ A list of in-progress multipart uploads.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMultipartUploadsResponse' value with any optional fields omitted.
mkListMultipartUploadsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMultipartUploadsResponse
mkListMultipartUploadsResponse responseStatus
  = ListMultipartUploadsResponse'{marker = Core.Nothing,
                                  uploadsList = Core.Nothing, responseStatus}

-- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsMarker :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe Core.Text)
lmurrsMarker = Lens.field @"marker"
{-# INLINEABLE lmurrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | A list of in-progress multipart uploads.
--
-- /Note:/ Consider using 'uploadsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsUploadsList :: Lens.Lens' ListMultipartUploadsResponse (Core.Maybe [Types.UploadListElement])
lmurrsUploadsList = Lens.field @"uploadsList"
{-# INLINEABLE lmurrsUploadsList #-}
{-# DEPRECATED uploadsList "Use generic-lens or generic-optics with 'uploadsList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmurrsResponseStatus :: Lens.Lens' ListMultipartUploadsResponse Core.Int
lmurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
