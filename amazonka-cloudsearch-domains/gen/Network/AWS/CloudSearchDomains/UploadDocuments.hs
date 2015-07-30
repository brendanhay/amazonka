{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.UploadDocuments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Posts a batch of documents to a search domain for indexing. A document
-- batch is a collection of add and delete operations that represent the
-- documents you want to add, update, or delete from your domain. Batches
-- can be described in either JSON or XML. Each item that you want Amazon
-- CloudSearch to return as a search result (such as a product) is
-- represented as a document. Every document has a unique ID and one or
-- more fields that contain the data that you want to search and return in
-- results. Individual documents cannot contain more than 1 MB of data. The
-- entire batch cannot exceed 5 MB. To get the best possible upload
-- performance, group add and delete operations in batches that are close
-- the 5 MB limit. Submitting a large volume of single-document batches can
-- overload a domain\'s document service.
--
-- The endpoint for submitting @UploadDocuments@ requests is
-- domain-specific. To get the document endpoint for your domain, use the
-- Amazon CloudSearch configuration service @DescribeDomains@ action. A
-- domain\'s endpoints are also displayed on the domain dashboard in the
-- Amazon CloudSearch console.
--
-- For more information about formatting your data for Amazon CloudSearch,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html Preparing Your Data>
-- in the /Amazon CloudSearch Developer Guide/. For more information about
-- uploading data for indexing, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html Uploading Data>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UploadDocuments.html>
module Network.AWS.CloudSearchDomains.UploadDocuments
    (
    -- * Request
      UploadDocuments
    -- ** Request constructor
    , uploadDocuments
    -- ** Request lenses
    , udContentType
    , udDocuments

    -- * Response
    , UploadDocumentsResponse
    -- ** Response constructor
    , uploadDocumentsResponse
    -- ** Response lenses
    , udrsAdds
    , udrsWarnings
    , udrsDeletes
    , udrsStatus
    ) where

import           Network.AWS.CloudSearchDomains.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @UploadDocuments@ request.
--
-- /See:/ 'uploadDocuments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udContentType'
--
-- * 'udDocuments'
data UploadDocuments = UploadDocuments'
    { _udContentType :: !ContentType
    , _udDocuments   :: !RqBody
    } deriving (Show,Generic)

-- | 'UploadDocuments' smart constructor.
uploadDocuments :: ContentType -> RqBody -> UploadDocuments
uploadDocuments pContentType_ pDocuments_ =
    UploadDocuments'
    { _udContentType = pContentType_
    , _udDocuments = pDocuments_
    }

-- | The format of the batch you are uploading. Amazon CloudSearch supports
-- two document batch formats:
--
-- -   application\/json
-- -   application\/xml
udContentType :: Lens' UploadDocuments ContentType
udContentType = lens _udContentType (\ s a -> s{_udContentType = a});

-- | A batch of documents formatted in JSON or HTML.
udDocuments :: Lens' UploadDocuments RqBody
udDocuments = lens _udDocuments (\ s a -> s{_udDocuments = a});

instance AWSRequest UploadDocuments where
        type Sv UploadDocuments = CloudSearchDomains
        type Rs UploadDocuments = UploadDocumentsResponse
        request = postBody
        response
          = receiveJSON
              (\ s h x ->
                 UploadDocumentsResponse' <$>
                   (x .?> "adds") <*> (x .?> "warnings" .!@ mempty) <*>
                     (x .?> "deletes")
                     <*> (pure (fromEnum s)))

instance ToBody UploadDocuments where
        toBody = _udDocuments

instance ToHeaders UploadDocuments where
        toHeaders UploadDocuments'{..}
          = mconcat
              ["Content-Type" =# _udContentType,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath UploadDocuments where
        toPath = const ["2013-01-01", "documents", "batch"]

instance ToQuery UploadDocuments where
        toQuery = const (mconcat ["format=sdk"])

-- | Contains the response to an @UploadDocuments@ request.
--
-- /See:/ 'uploadDocumentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udrsAdds'
--
-- * 'udrsWarnings'
--
-- * 'udrsDeletes'
--
-- * 'udrsStatus'
data UploadDocumentsResponse = UploadDocumentsResponse'
    { _udrsAdds     :: !(Maybe Integer)
    , _udrsWarnings :: !(Maybe [DocumentServiceWarning])
    , _udrsDeletes  :: !(Maybe Integer)
    , _udrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadDocumentsResponse' smart constructor.
uploadDocumentsResponse :: Int -> UploadDocumentsResponse
uploadDocumentsResponse pStatus_ =
    UploadDocumentsResponse'
    { _udrsAdds = Nothing
    , _udrsWarnings = Nothing
    , _udrsDeletes = Nothing
    , _udrsStatus = pStatus_
    }

-- | The number of documents that were added to the search domain.
udrsAdds :: Lens' UploadDocumentsResponse (Maybe Integer)
udrsAdds = lens _udrsAdds (\ s a -> s{_udrsAdds = a});

-- | Any warnings returned by the document service about the documents being
-- uploaded.
udrsWarnings :: Lens' UploadDocumentsResponse [DocumentServiceWarning]
udrsWarnings = lens _udrsWarnings (\ s a -> s{_udrsWarnings = a}) . _Default . _Coerce;

-- | The number of documents that were deleted from the search domain.
udrsDeletes :: Lens' UploadDocumentsResponse (Maybe Integer)
udrsDeletes = lens _udrsDeletes (\ s a -> s{_udrsDeletes = a});

-- | FIXME: Undocumented member.
udrsStatus :: Lens' UploadDocumentsResponse Int
udrsStatus = lens _udrsStatus (\ s a -> s{_udrsStatus = a});
