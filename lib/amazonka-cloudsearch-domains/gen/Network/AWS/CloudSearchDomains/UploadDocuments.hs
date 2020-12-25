{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.UploadDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a batch of documents to a search domain for indexing. A document batch is a collection of add and delete operations that represent the documents you want to add, update, or delete from your domain. Batches can be described in either JSON or XML. Each item that you want Amazon CloudSearch to return as a search result (such as a product) is represented as a document. Every document has a unique ID and one or more fields that contain the data that you want to search and return in results. Individual documents cannot contain more than 1 MB of data. The entire batch cannot exceed 5 MB. To get the best possible upload performance, group add and delete operations in batches that are close the 5 MB limit. Submitting a large volume of single-document batches can overload a domain's document service.
--
-- The endpoint for submitting @UploadDocuments@ requests is domain-specific. To get the document endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console.
-- For more information about formatting your data for Amazon CloudSearch, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html Preparing Your Data> in the /Amazon CloudSearch Developer Guide/ . For more information about uploading data for indexing, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html Uploading Data> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearchDomains.UploadDocuments
  ( -- * Creating a request
    UploadDocuments (..),
    mkUploadDocuments,

    -- ** Request lenses
    udDocuments,
    udContentType,

    -- * Destructuring the response
    UploadDocumentsResponse (..),
    mkUploadDocumentsResponse,

    -- ** Response lenses
    udrrsAdds,
    udrrsDeletes,
    udrrsStatus,
    udrrsWarnings,
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @UploadDocuments@ request.
--
-- /See:/ 'mkUploadDocuments' smart constructor.
data UploadDocuments = UploadDocuments'
  { -- | A batch of documents formatted in JSON or HTML.
    documents :: Core.HashedBody,
    -- | The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:
    --
    --
    --     * application/json
    --
    --     * application/xml
    contentType :: Types.ContentType
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'UploadDocuments' value with any optional fields omitted.
mkUploadDocuments ::
  -- | 'documents'
  Core.HashedBody ->
  -- | 'contentType'
  Types.ContentType ->
  UploadDocuments
mkUploadDocuments documents contentType =
  UploadDocuments' {documents, contentType}

-- | A batch of documents formatted in JSON or HTML.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocuments :: Lens.Lens' UploadDocuments Core.HashedBody
udDocuments = Lens.field @"documents"
{-# DEPRECATED udDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:
--
--
--     * application/json
--
--     * application/xml
--
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContentType :: Lens.Lens' UploadDocuments Types.ContentType
udContentType = Lens.field @"contentType"
{-# DEPRECATED udContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Core.AWSRequest UploadDocuments where
  type Rs UploadDocuments = UploadDocumentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-01-01/documents/batch",
        Core._rqQuery = Core.pure ("format=sdk", ""),
        Core._rqHeaders = Core.toHeaders "Content-Type" contentType,
        Core._rqBody = Core.toBody documents
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UploadDocumentsResponse'
            Core.<$> (x Core..:? "adds")
            Core.<*> (x Core..:? "deletes")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "warnings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to an @UploadDocuments@ request.
--
-- /See:/ 'mkUploadDocumentsResponse' smart constructor.
data UploadDocumentsResponse = UploadDocumentsResponse'
  { -- | The number of documents that were added to the search domain.
    adds :: Core.Maybe Core.Integer,
    -- | The number of documents that were deleted from the search domain.
    deletes :: Core.Maybe Core.Integer,
    -- | The status of an @UploadDocumentsRequest@ .
    status :: Core.Maybe Types.String,
    -- | Any warnings returned by the document service about the documents being uploaded.
    warnings :: Core.Maybe [Types.DocumentServiceWarning],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadDocumentsResponse' value with any optional fields omitted.
mkUploadDocumentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UploadDocumentsResponse
mkUploadDocumentsResponse responseStatus =
  UploadDocumentsResponse'
    { adds = Core.Nothing,
      deletes = Core.Nothing,
      status = Core.Nothing,
      warnings = Core.Nothing,
      responseStatus
    }

-- | The number of documents that were added to the search domain.
--
-- /Note:/ Consider using 'adds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsAdds :: Lens.Lens' UploadDocumentsResponse (Core.Maybe Core.Integer)
udrrsAdds = Lens.field @"adds"
{-# DEPRECATED udrrsAdds "Use generic-lens or generic-optics with 'adds' instead." #-}

-- | The number of documents that were deleted from the search domain.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsDeletes :: Lens.Lens' UploadDocumentsResponse (Core.Maybe Core.Integer)
udrrsDeletes = Lens.field @"deletes"
{-# DEPRECATED udrrsDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The status of an @UploadDocumentsRequest@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsStatus :: Lens.Lens' UploadDocumentsResponse (Core.Maybe Types.String)
udrrsStatus = Lens.field @"status"
{-# DEPRECATED udrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Any warnings returned by the document service about the documents being uploaded.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsWarnings :: Lens.Lens' UploadDocumentsResponse (Core.Maybe [Types.DocumentServiceWarning])
udrrsWarnings = Lens.field @"warnings"
{-# DEPRECATED udrrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UploadDocumentsResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
