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
-- Module      : Network.AWS.WorkDocs.DescribeDocumentVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the document versions for the specified document.
--
--
-- By default, only active versions are returned.
--
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeDocumentVersions
    (
    -- * Creating a Request
      describeDocumentVersions
    , DescribeDocumentVersions
    -- * Request Lenses
    , ddvInclude
    , ddvAuthenticationToken
    , ddvMarker
    , ddvLimit
    , ddvFields
    , ddvDocumentId

    -- * Destructuring the Response
    , describeDocumentVersionsResponse
    , DescribeDocumentVersionsResponse
    -- * Response Lenses
    , ddvrsDocumentVersions
    , ddvrsMarker
    , ddvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeDocumentVersions' smart constructor.
data DescribeDocumentVersions = DescribeDocumentVersions'
  { _ddvInclude             :: !(Maybe Text)
  , _ddvAuthenticationToken :: !(Maybe (Sensitive Text))
  , _ddvMarker              :: !(Maybe Text)
  , _ddvLimit               :: !(Maybe Nat)
  , _ddvFields              :: !(Maybe Text)
  , _ddvDocumentId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocumentVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvInclude' - A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
--
-- * 'ddvAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'ddvMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'ddvLimit' - The maximum number of versions to return with this call.
--
-- * 'ddvFields' - Specify "SOURCE" to include initialized versions and a URL for the source document.
--
-- * 'ddvDocumentId' - The ID of the document.
describeDocumentVersions
    :: Text -- ^ 'ddvDocumentId'
    -> DescribeDocumentVersions
describeDocumentVersions pDocumentId_ =
  DescribeDocumentVersions'
    { _ddvInclude = Nothing
    , _ddvAuthenticationToken = Nothing
    , _ddvMarker = Nothing
    , _ddvLimit = Nothing
    , _ddvFields = Nothing
    , _ddvDocumentId = pDocumentId_
    }


-- | A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
ddvInclude :: Lens' DescribeDocumentVersions (Maybe Text)
ddvInclude = lens _ddvInclude (\ s a -> s{_ddvInclude = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
ddvAuthenticationToken :: Lens' DescribeDocumentVersions (Maybe Text)
ddvAuthenticationToken = lens _ddvAuthenticationToken (\ s a -> s{_ddvAuthenticationToken = a}) . mapping _Sensitive

-- | The marker for the next set of results. (You received this marker from a previous call.)
ddvMarker :: Lens' DescribeDocumentVersions (Maybe Text)
ddvMarker = lens _ddvMarker (\ s a -> s{_ddvMarker = a})

-- | The maximum number of versions to return with this call.
ddvLimit :: Lens' DescribeDocumentVersions (Maybe Natural)
ddvLimit = lens _ddvLimit (\ s a -> s{_ddvLimit = a}) . mapping _Nat

-- | Specify "SOURCE" to include initialized versions and a URL for the source document.
ddvFields :: Lens' DescribeDocumentVersions (Maybe Text)
ddvFields = lens _ddvFields (\ s a -> s{_ddvFields = a})

-- | The ID of the document.
ddvDocumentId :: Lens' DescribeDocumentVersions Text
ddvDocumentId = lens _ddvDocumentId (\ s a -> s{_ddvDocumentId = a})

instance AWSPager DescribeDocumentVersions where
        page rq rs
          | stop (rs ^. ddvrsMarker) = Nothing
          | stop (rs ^. ddvrsDocumentVersions) = Nothing
          | otherwise =
            Just $ rq & ddvMarker .~ rs ^. ddvrsMarker

instance AWSRequest DescribeDocumentVersions where
        type Rs DescribeDocumentVersions =
             DescribeDocumentVersionsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDocumentVersionsResponse' <$>
                   (x .?> "DocumentVersions" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDocumentVersions where

instance NFData DescribeDocumentVersions where

instance ToHeaders DescribeDocumentVersions where
        toHeaders DescribeDocumentVersions'{..}
          = mconcat
              ["Authentication" =# _ddvAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeDocumentVersions where
        toPath DescribeDocumentVersions'{..}
          = mconcat
              ["/api/v1/documents/", toBS _ddvDocumentId,
               "/versions"]

instance ToQuery DescribeDocumentVersions where
        toQuery DescribeDocumentVersions'{..}
          = mconcat
              ["include" =: _ddvInclude, "marker" =: _ddvMarker,
               "limit" =: _ddvLimit, "fields" =: _ddvFields]

-- | /See:/ 'describeDocumentVersionsResponse' smart constructor.
data DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse'
  { _ddvrsDocumentVersions :: !(Maybe [DocumentVersionMetadata])
  , _ddvrsMarker           :: !(Maybe Text)
  , _ddvrsResponseStatus   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDocumentVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvrsDocumentVersions' - The document versions.
--
-- * 'ddvrsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'ddvrsResponseStatus' - -- | The response status code.
describeDocumentVersionsResponse
    :: Int -- ^ 'ddvrsResponseStatus'
    -> DescribeDocumentVersionsResponse
describeDocumentVersionsResponse pResponseStatus_ =
  DescribeDocumentVersionsResponse'
    { _ddvrsDocumentVersions = Nothing
    , _ddvrsMarker = Nothing
    , _ddvrsResponseStatus = pResponseStatus_
    }


-- | The document versions.
ddvrsDocumentVersions :: Lens' DescribeDocumentVersionsResponse [DocumentVersionMetadata]
ddvrsDocumentVersions = lens _ddvrsDocumentVersions (\ s a -> s{_ddvrsDocumentVersions = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
ddvrsMarker :: Lens' DescribeDocumentVersionsResponse (Maybe Text)
ddvrsMarker = lens _ddvrsMarker (\ s a -> s{_ddvrsMarker = a})

-- | -- | The response status code.
ddvrsResponseStatus :: Lens' DescribeDocumentVersionsResponse Int
ddvrsResponseStatus = lens _ddvrsResponseStatus (\ s a -> s{_ddvrsResponseStatus = a})

instance NFData DescribeDocumentVersionsResponse
         where
