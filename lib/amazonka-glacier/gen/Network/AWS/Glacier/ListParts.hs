{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the parts of an archive that have been uploaded in a specific multipart upload. You can make this request at any time during an in-progress multipart upload before you complete the upload (see 'CompleteMultipartUpload' . List Parts returns an error for completed uploads. The list returned in the List Parts response is sorted by part range.
--
-- The List Parts operation supports pagination. By default, this operation returns up to 50 uploaded parts in the response. You should always check the response for a @marker@ at which to continue the list; if there are no more items the @marker@ is @null@ . To return a list of parts that begins at a specific part, set the @marker@ request parameter to the value you obtained from a previous List Parts request. You can also limit the number of parts returned in the response by specifying the @limit@ parameter in the request.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html List Parts> in the /Amazon Glacier Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListParts
  ( -- * Creating a request
    ListParts (..),
    mkListParts,

    -- ** Request lenses
    lpAccountId,
    lpVaultName,
    lpUploadId,
    lpLimit,
    lpMarker,

    -- * Destructuring the response
    ListPartsResponse (..),
    mkListPartsResponse,

    -- ** Response lenses
    lprrsArchiveDescription,
    lprrsCreationDate,
    lprrsMarker,
    lprrsMultipartUploadId,
    lprrsPartSizeInBytes,
    lprrsParts,
    lprrsVaultARN,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for retrieving a list of parts of an archive that have been uploaded in a specific multipart upload.
--
-- /See:/ 'mkListParts' smart constructor.
data ListParts = ListParts'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | The upload ID of the multipart upload.
    uploadId :: Types.String,
    -- | The maximum number of parts to be returned. The default limit is 50. The number of parts returned might be fewer than the specified limit, but the number of returned parts never exceeds the limit.
    limit :: Core.Maybe Types.String,
    -- | An opaque string used for pagination. This value specifies the part at which the listing of parts should begin. Get the marker value from the response of a previous List Parts response. You need only include the marker if you are continuing the pagination of results started in a previous List Parts request.
    marker :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListParts' value with any optional fields omitted.
mkListParts ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  -- | 'uploadId'
  Types.String ->
  ListParts
mkListParts accountId vaultName uploadId =
  ListParts'
    { accountId,
      vaultName,
      uploadId,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAccountId :: Lens.Lens' ListParts Types.String
lpAccountId = Lens.field @"accountId"
{-# DEPRECATED lpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpVaultName :: Lens.Lens' ListParts Types.String
lpVaultName = Lens.field @"vaultName"
{-# DEPRECATED lpVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUploadId :: Lens.Lens' ListParts Types.String
lpUploadId = Lens.field @"uploadId"
{-# DEPRECATED lpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The maximum number of parts to be returned. The default limit is 50. The number of parts returned might be fewer than the specified limit, but the number of returned parts never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpLimit :: Lens.Lens' ListParts (Core.Maybe Types.String)
lpLimit = Lens.field @"limit"
{-# DEPRECATED lpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An opaque string used for pagination. This value specifies the part at which the listing of parts should begin. Get the marker value from the response of a previous List Parts response. You need only include the marker if you are continuing the pagination of results started in a previous List Parts request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListParts (Core.Maybe Types.String)
lpMarker = Lens.field @"marker"
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.AWSRequest ListParts where
  type Rs ListParts = ListPartsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/multipart-uploads/")
                Core.<> (Core.toText uploadId)
            ),
        Core._rqQuery =
          Core.toQueryValue "limit" Core.<$> limit
            Core.<> (Core.toQueryValue "marker" Core.<$> marker),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartsResponse'
            Core.<$> (x Core..:? "ArchiveDescription")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (x Core..:? "MultipartUploadId")
            Core.<*> (x Core..:? "PartSizeInBytes")
            Core.<*> (x Core..:? "Parts")
            Core.<*> (x Core..:? "VaultARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListParts where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"parts" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { -- | The description of the archive that was specified in the Initiate Multipart Upload request.
    archiveDescription :: Core.Maybe Types.String,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Core.Maybe Types.String,
    -- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Parts request to obtain more jobs in the list. If there are no more parts, this value is @null@ .
    marker :: Core.Maybe Types.String,
    -- | The ID of the upload to which the parts are associated.
    multipartUploadId :: Core.Maybe Types.String,
    -- | The part size in bytes. This is the same value that you specified in the Initiate Multipart Upload request.
    partSizeInBytes :: Core.Maybe Core.Integer,
    -- | A list of the part sizes of the multipart upload. Each object in the array contains a @RangeBytes@ and @sha256-tree-hash@ name/value pair.
    parts :: Core.Maybe [Types.PartListElement],
    -- | The Amazon Resource Name (ARN) of the vault to which the multipart upload was initiated.
    vaultARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPartsResponse' value with any optional fields omitted.
mkListPartsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPartsResponse
mkListPartsResponse responseStatus =
  ListPartsResponse'
    { archiveDescription = Core.Nothing,
      creationDate = Core.Nothing,
      marker = Core.Nothing,
      multipartUploadId = Core.Nothing,
      partSizeInBytes = Core.Nothing,
      parts = Core.Nothing,
      vaultARN = Core.Nothing,
      responseStatus
    }

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsArchiveDescription :: Lens.Lens' ListPartsResponse (Core.Maybe Types.String)
lprrsArchiveDescription = Lens.field @"archiveDescription"
{-# DEPRECATED lprrsArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The UTC time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsCreationDate :: Lens.Lens' ListPartsResponse (Core.Maybe Types.String)
lprrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED lprrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Parts request to obtain more jobs in the list. If there are no more parts, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsMarker :: Lens.Lens' ListPartsResponse (Core.Maybe Types.String)
lprrsMarker = Lens.field @"marker"
{-# DEPRECATED lprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The ID of the upload to which the parts are associated.
--
-- /Note:/ Consider using 'multipartUploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsMultipartUploadId :: Lens.Lens' ListPartsResponse (Core.Maybe Types.String)
lprrsMultipartUploadId = Lens.field @"multipartUploadId"
{-# DEPRECATED lprrsMultipartUploadId "Use generic-lens or generic-optics with 'multipartUploadId' instead." #-}

-- | The part size in bytes. This is the same value that you specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'partSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPartSizeInBytes :: Lens.Lens' ListPartsResponse (Core.Maybe Core.Integer)
lprrsPartSizeInBytes = Lens.field @"partSizeInBytes"
{-# DEPRECATED lprrsPartSizeInBytes "Use generic-lens or generic-optics with 'partSizeInBytes' instead." #-}

-- | A list of the part sizes of the multipart upload. Each object in the array contains a @RangeBytes@ and @sha256-tree-hash@ name/value pair.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsParts :: Lens.Lens' ListPartsResponse (Core.Maybe [Types.PartListElement])
lprrsParts = Lens.field @"parts"
{-# DEPRECATED lprrsParts "Use generic-lens or generic-optics with 'parts' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsVaultARN :: Lens.Lens' ListPartsResponse (Core.Maybe Types.String)
lprrsVaultARN = Lens.field @"vaultARN"
{-# DEPRECATED lprrsVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPartsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
