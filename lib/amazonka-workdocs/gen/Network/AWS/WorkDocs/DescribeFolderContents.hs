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
-- Module      : Network.AWS.WorkDocs.DescribeFolderContents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the contents of the specified folder, including its documents and subfolders.
--
--
-- By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.
--
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeFolderContents
    (
    -- * Creating a Request
      describeFolderContents
    , DescribeFolderContents
    -- * Request Lenses
    , dfcsInclude
    , dfcsAuthenticationToken
    , dfcsSort
    , dfcsMarker
    , dfcsLimit
    , dfcsType
    , dfcsOrder
    , dfcsFolderId

    -- * Destructuring the Response
    , describeFolderContentsResponse
    , DescribeFolderContentsResponse
    -- * Response Lenses
    , dfcrsFolders
    , dfcrsDocuments
    , dfcrsMarker
    , dfcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeFolderContents' smart constructor.
data DescribeFolderContents = DescribeFolderContents'
  { _dfcsInclude             :: !(Maybe Text)
  , _dfcsAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dfcsSort                :: !(Maybe ResourceSortType)
  , _dfcsMarker              :: !(Maybe Text)
  , _dfcsLimit               :: !(Maybe Nat)
  , _dfcsType                :: !(Maybe FolderContentType)
  , _dfcsOrder               :: !(Maybe OrderType)
  , _dfcsFolderId            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFolderContents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcsInclude' - The contents to include. Specify "INITIALIZED" to include initialized documents.
--
-- * 'dfcsAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dfcsSort' - The sorting criteria.
--
-- * 'dfcsMarker' - The marker for the next set of results. This marker was received from a previous call.
--
-- * 'dfcsLimit' - The maximum number of items to return with this call.
--
-- * 'dfcsType' - The type of items.
--
-- * 'dfcsOrder' - The order for the contents of the folder.
--
-- * 'dfcsFolderId' - The ID of the folder.
describeFolderContents
    :: Text -- ^ 'dfcsFolderId'
    -> DescribeFolderContents
describeFolderContents pFolderId_ =
  DescribeFolderContents'
    { _dfcsInclude = Nothing
    , _dfcsAuthenticationToken = Nothing
    , _dfcsSort = Nothing
    , _dfcsMarker = Nothing
    , _dfcsLimit = Nothing
    , _dfcsType = Nothing
    , _dfcsOrder = Nothing
    , _dfcsFolderId = pFolderId_
    }


-- | The contents to include. Specify "INITIALIZED" to include initialized documents.
dfcsInclude :: Lens' DescribeFolderContents (Maybe Text)
dfcsInclude = lens _dfcsInclude (\ s a -> s{_dfcsInclude = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dfcsAuthenticationToken :: Lens' DescribeFolderContents (Maybe Text)
dfcsAuthenticationToken = lens _dfcsAuthenticationToken (\ s a -> s{_dfcsAuthenticationToken = a}) . mapping _Sensitive

-- | The sorting criteria.
dfcsSort :: Lens' DescribeFolderContents (Maybe ResourceSortType)
dfcsSort = lens _dfcsSort (\ s a -> s{_dfcsSort = a})

-- | The marker for the next set of results. This marker was received from a previous call.
dfcsMarker :: Lens' DescribeFolderContents (Maybe Text)
dfcsMarker = lens _dfcsMarker (\ s a -> s{_dfcsMarker = a})

-- | The maximum number of items to return with this call.
dfcsLimit :: Lens' DescribeFolderContents (Maybe Natural)
dfcsLimit = lens _dfcsLimit (\ s a -> s{_dfcsLimit = a}) . mapping _Nat

-- | The type of items.
dfcsType :: Lens' DescribeFolderContents (Maybe FolderContentType)
dfcsType = lens _dfcsType (\ s a -> s{_dfcsType = a})

-- | The order for the contents of the folder.
dfcsOrder :: Lens' DescribeFolderContents (Maybe OrderType)
dfcsOrder = lens _dfcsOrder (\ s a -> s{_dfcsOrder = a})

-- | The ID of the folder.
dfcsFolderId :: Lens' DescribeFolderContents Text
dfcsFolderId = lens _dfcsFolderId (\ s a -> s{_dfcsFolderId = a})

instance AWSPager DescribeFolderContents where
        page rq rs
          | stop (rs ^. dfcrsMarker) = Nothing
          | stop (rs ^. dfcrsFolders) = Nothing
          | stop (rs ^. dfcrsDocuments) = Nothing
          | otherwise =
            Just $ rq & dfcsMarker .~ rs ^. dfcrsMarker

instance AWSRequest DescribeFolderContents where
        type Rs DescribeFolderContents =
             DescribeFolderContentsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFolderContentsResponse' <$>
                   (x .?> "Folders" .!@ mempty) <*>
                     (x .?> "Documents" .!@ mempty)
                     <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFolderContents where

instance NFData DescribeFolderContents where

instance ToHeaders DescribeFolderContents where
        toHeaders DescribeFolderContents'{..}
          = mconcat
              ["Authentication" =# _dfcsAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeFolderContents where
        toPath DescribeFolderContents'{..}
          = mconcat
              ["/api/v1/folders/", toBS _dfcsFolderId, "/contents"]

instance ToQuery DescribeFolderContents where
        toQuery DescribeFolderContents'{..}
          = mconcat
              ["include" =: _dfcsInclude, "sort" =: _dfcsSort,
               "marker" =: _dfcsMarker, "limit" =: _dfcsLimit,
               "type" =: _dfcsType, "order" =: _dfcsOrder]

-- | /See:/ 'describeFolderContentsResponse' smart constructor.
data DescribeFolderContentsResponse = DescribeFolderContentsResponse'
  { _dfcrsFolders        :: !(Maybe [FolderMetadata])
  , _dfcrsDocuments      :: !(Maybe [DocumentMetadata])
  , _dfcrsMarker         :: !(Maybe Text)
  , _dfcrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFolderContentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcrsFolders' - The subfolders in the specified folder.
--
-- * 'dfcrsDocuments' - The documents in the specified folder.
--
-- * 'dfcrsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dfcrsResponseStatus' - -- | The response status code.
describeFolderContentsResponse
    :: Int -- ^ 'dfcrsResponseStatus'
    -> DescribeFolderContentsResponse
describeFolderContentsResponse pResponseStatus_ =
  DescribeFolderContentsResponse'
    { _dfcrsFolders = Nothing
    , _dfcrsDocuments = Nothing
    , _dfcrsMarker = Nothing
    , _dfcrsResponseStatus = pResponseStatus_
    }


-- | The subfolders in the specified folder.
dfcrsFolders :: Lens' DescribeFolderContentsResponse [FolderMetadata]
dfcrsFolders = lens _dfcrsFolders (\ s a -> s{_dfcrsFolders = a}) . _Default . _Coerce

-- | The documents in the specified folder.
dfcrsDocuments :: Lens' DescribeFolderContentsResponse [DocumentMetadata]
dfcrsDocuments = lens _dfcrsDocuments (\ s a -> s{_dfcrsDocuments = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dfcrsMarker :: Lens' DescribeFolderContentsResponse (Maybe Text)
dfcrsMarker = lens _dfcrsMarker (\ s a -> s{_dfcrsMarker = a})

-- | -- | The response status code.
dfcrsResponseStatus :: Lens' DescribeFolderContentsResponse Int
dfcrsResponseStatus = lens _dfcrsResponseStatus (\ s a -> s{_dfcrsResponseStatus = a})

instance NFData DescribeFolderContentsResponse where
