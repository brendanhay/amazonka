{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    udContentType,
    udDocuments,

    -- * Destructuring the response
    UploadDocumentsResponse (..),
    mkUploadDocumentsResponse,

    -- ** Response lenses
    udrsStatus,
    udrsAdds,
    udrsWarnings,
    udrsDeletes,
    udrsResponseStatus,
  )
where

import Network.AWS.CloudSearchDomains.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @UploadDocuments@ request.
--
-- /See:/ 'mkUploadDocuments' smart constructor.
data UploadDocuments = UploadDocuments'
  { contentType :: ContentType,
    documents :: Lude.HashedBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'UploadDocuments' with the minimum fields required to make a request.
--
-- * 'contentType' - The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:
--
--
--     * application/json
--
--     * application/xml
--
-- * 'documents' - A batch of documents formatted in JSON or HTML.
mkUploadDocuments ::
  -- | 'contentType'
  ContentType ->
  -- | 'documents'
  Lude.HashedBody ->
  UploadDocuments
mkUploadDocuments pContentType_ pDocuments_ =
  UploadDocuments'
    { contentType = pContentType_,
      documents = pDocuments_
    }

-- | The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:
--
--
--     * application/json
--
--     * application/xml
--
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContentType :: Lens.Lens' UploadDocuments ContentType
udContentType = Lens.lens (contentType :: UploadDocuments -> ContentType) (\s a -> s {contentType = a} :: UploadDocuments)
{-# DEPRECATED udContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | A batch of documents formatted in JSON or HTML.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocuments :: Lens.Lens' UploadDocuments Lude.HashedBody
udDocuments = Lens.lens (documents :: UploadDocuments -> Lude.HashedBody) (\s a -> s {documents = a} :: UploadDocuments)
{-# DEPRECATED udDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

instance Lude.AWSRequest UploadDocuments where
  type Rs UploadDocuments = UploadDocumentsResponse
  request = Req.postBody cloudSearchDomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UploadDocumentsResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "adds")
            Lude.<*> (x Lude..?> "warnings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "deletes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody UploadDocuments where
  toBody = Lude.toBody Lude.. documents

instance Lude.ToHeaders UploadDocuments where
  toHeaders UploadDocuments' {..} =
    Lude.mconcat ["Content-Type" Lude.=# contentType]

instance Lude.ToPath UploadDocuments where
  toPath = Lude.const "/2013-01-01/documents/batch"

instance Lude.ToQuery UploadDocuments where
  toQuery = Lude.const (Lude.mconcat ["format=sdk"])

-- | Contains the response to an @UploadDocuments@ request.
--
-- /See:/ 'mkUploadDocumentsResponse' smart constructor.
data UploadDocumentsResponse = UploadDocumentsResponse'
  { status ::
      Lude.Maybe Lude.Text,
    adds :: Lude.Maybe Lude.Integer,
    warnings ::
      Lude.Maybe [DocumentServiceWarning],
    deletes :: Lude.Maybe Lude.Integer,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadDocumentsResponse' with the minimum fields required to make a request.
--
-- * 'adds' - The number of documents that were added to the search domain.
-- * 'deletes' - The number of documents that were deleted from the search domain.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of an @UploadDocumentsRequest@ .
-- * 'warnings' - Any warnings returned by the document service about the documents being uploaded.
mkUploadDocumentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UploadDocumentsResponse
mkUploadDocumentsResponse pResponseStatus_ =
  UploadDocumentsResponse'
    { status = Lude.Nothing,
      adds = Lude.Nothing,
      warnings = Lude.Nothing,
      deletes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of an @UploadDocumentsRequest@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsStatus :: Lens.Lens' UploadDocumentsResponse (Lude.Maybe Lude.Text)
udrsStatus = Lens.lens (status :: UploadDocumentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: UploadDocumentsResponse)
{-# DEPRECATED udrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of documents that were added to the search domain.
--
-- /Note:/ Consider using 'adds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsAdds :: Lens.Lens' UploadDocumentsResponse (Lude.Maybe Lude.Integer)
udrsAdds = Lens.lens (adds :: UploadDocumentsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {adds = a} :: UploadDocumentsResponse)
{-# DEPRECATED udrsAdds "Use generic-lens or generic-optics with 'adds' instead." #-}

-- | Any warnings returned by the document service about the documents being uploaded.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsWarnings :: Lens.Lens' UploadDocumentsResponse (Lude.Maybe [DocumentServiceWarning])
udrsWarnings = Lens.lens (warnings :: UploadDocumentsResponse -> Lude.Maybe [DocumentServiceWarning]) (\s a -> s {warnings = a} :: UploadDocumentsResponse)
{-# DEPRECATED udrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The number of documents that were deleted from the search domain.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsDeletes :: Lens.Lens' UploadDocumentsResponse (Lude.Maybe Lude.Integer)
udrsDeletes = Lens.lens (deletes :: UploadDocumentsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {deletes = a} :: UploadDocumentsResponse)
{-# DEPRECATED udrsDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UploadDocumentsResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UploadDocumentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UploadDocumentsResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
