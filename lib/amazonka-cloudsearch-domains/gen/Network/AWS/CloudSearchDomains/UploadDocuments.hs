{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.UploadDocuments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a batch of documents to a search domain for indexing. A document batch is a collection of add and delete operations that represent the documents you want to add, update, or delete from your domain. Batches can be described in either JSON or XML. Each item that you want Amazon CloudSearch to return as a search result (such as a product) is represented as a document. Every document has a unique ID and one or more fields that contain the data that you want to search and return in results. Individual documents cannot contain more than 1 MB of data. The entire batch cannot exceed 5 MB. To get the best possible upload performance, group add and delete operations in batches that are close the 5 MB limit. Submitting a large volume of single-document batches can overload a domain's document service.
--
--
-- The endpoint for submitting @UploadDocuments@ requests is domain-specific. To get the document endpoint for your domain, use the Amazon CloudSearch configuration service @DescribeDomains@ action. A domain's endpoints are also displayed on the domain dashboard in the Amazon CloudSearch console.
--
-- For more information about formatting your data for Amazon CloudSearch, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html Preparing Your Data> in the /Amazon CloudSearch Developer Guide/ . For more information about uploading data for indexing, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html Uploading Data> in the /Amazon CloudSearch Developer Guide/ .
--
module Network.AWS.CloudSearchDomains.UploadDocuments
    (
    -- * Creating a Request
      uploadDocuments
    , UploadDocuments
    -- * Request Lenses
    , udContentType
    , udDocuments

    -- * Destructuring the Response
    , uploadDocumentsResponse
    , UploadDocumentsResponse
    -- * Response Lenses
    , udrsStatus
    , udrsAdds
    , udrsWarnings
    , udrsDeletes
    , udrsResponseStatus
    ) where

import Network.AWS.CloudSearchDomains.Types
import Network.AWS.CloudSearchDomains.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @UploadDocuments@ request.
--
--
--
-- /See:/ 'uploadDocuments' smart constructor.
data UploadDocuments = UploadDocuments'
  { _udContentType :: !ContentType
  , _udDocuments   :: !HashedBody
  } deriving (Show, Generic)


-- | Creates a value of 'UploadDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udContentType' - The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:     * application/json    * application/xml
--
-- * 'udDocuments' - A batch of documents formatted in JSON or HTML.
uploadDocuments
    :: ContentType -- ^ 'udContentType'
    -> HashedBody -- ^ 'udDocuments'
    -> UploadDocuments
uploadDocuments pContentType_ pDocuments_ =
  UploadDocuments' {_udContentType = pContentType_, _udDocuments = pDocuments_}


-- | The format of the batch you are uploading. Amazon CloudSearch supports two document batch formats:     * application/json    * application/xml
udContentType :: Lens' UploadDocuments ContentType
udContentType = lens _udContentType (\ s a -> s{_udContentType = a})

-- | A batch of documents formatted in JSON or HTML.
udDocuments :: Lens' UploadDocuments HashedBody
udDocuments = lens _udDocuments (\ s a -> s{_udDocuments = a})

instance AWSRequest UploadDocuments where
        type Rs UploadDocuments = UploadDocumentsResponse
        request = postBody cloudSearchDomains
        response
          = receiveJSON
              (\ s h x ->
                 UploadDocumentsResponse' <$>
                   (x .?> "status") <*> (x .?> "adds") <*>
                     (x .?> "warnings" .!@ mempty)
                     <*> (x .?> "deletes")
                     <*> (pure (fromEnum s)))

instance ToBody UploadDocuments where
        toBody = toBody . _udDocuments

instance ToHeaders UploadDocuments where
        toHeaders UploadDocuments'{..}
          = mconcat ["Content-Type" =# _udContentType]

instance ToPath UploadDocuments where
        toPath = const "/2013-01-01/documents/batch"

instance ToQuery UploadDocuments where
        toQuery = const (mconcat ["format=sdk"])

-- | Contains the response to an @UploadDocuments@ request.
--
--
--
-- /See:/ 'uploadDocumentsResponse' smart constructor.
data UploadDocumentsResponse = UploadDocumentsResponse'
  { _udrsStatus         :: !(Maybe Text)
  , _udrsAdds           :: !(Maybe Integer)
  , _udrsWarnings       :: !(Maybe [DocumentServiceWarning])
  , _udrsDeletes        :: !(Maybe Integer)
  , _udrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadDocumentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsStatus' - The status of an @UploadDocumentsRequest@ .
--
-- * 'udrsAdds' - The number of documents that were added to the search domain.
--
-- * 'udrsWarnings' - Any warnings returned by the document service about the documents being uploaded.
--
-- * 'udrsDeletes' - The number of documents that were deleted from the search domain.
--
-- * 'udrsResponseStatus' - -- | The response status code.
uploadDocumentsResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UploadDocumentsResponse
uploadDocumentsResponse pResponseStatus_ =
  UploadDocumentsResponse'
    { _udrsStatus = Nothing
    , _udrsAdds = Nothing
    , _udrsWarnings = Nothing
    , _udrsDeletes = Nothing
    , _udrsResponseStatus = pResponseStatus_
    }


-- | The status of an @UploadDocumentsRequest@ .
udrsStatus :: Lens' UploadDocumentsResponse (Maybe Text)
udrsStatus = lens _udrsStatus (\ s a -> s{_udrsStatus = a})

-- | The number of documents that were added to the search domain.
udrsAdds :: Lens' UploadDocumentsResponse (Maybe Integer)
udrsAdds = lens _udrsAdds (\ s a -> s{_udrsAdds = a})

-- | Any warnings returned by the document service about the documents being uploaded.
udrsWarnings :: Lens' UploadDocumentsResponse [DocumentServiceWarning]
udrsWarnings = lens _udrsWarnings (\ s a -> s{_udrsWarnings = a}) . _Default . _Coerce

-- | The number of documents that were deleted from the search domain.
udrsDeletes :: Lens' UploadDocumentsResponse (Maybe Integer)
udrsDeletes = lens _udrsDeletes (\ s a -> s{_udrsDeletes = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UploadDocumentsResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UploadDocumentsResponse where
