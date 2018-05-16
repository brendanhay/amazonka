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
-- Module      : Network.AWS.WorkDocs.DescribeRootFolders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current user's special folders; the @RootFolder@ and the @RecycleBin@ . @RootFolder@ is the root of user's files and folders and @RecycleBin@ is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.
--
--
module Network.AWS.WorkDocs.DescribeRootFolders
    (
    -- * Creating a Request
      describeRootFolders
    , DescribeRootFolders
    -- * Request Lenses
    , drfMarker
    , drfLimit
    , drfAuthenticationToken

    -- * Destructuring the Response
    , describeRootFoldersResponse
    , DescribeRootFoldersResponse
    -- * Response Lenses
    , drfrsFolders
    , drfrsMarker
    , drfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeRootFolders' smart constructor.
data DescribeRootFolders = DescribeRootFolders'
  { _drfMarker              :: !(Maybe Text)
  , _drfLimit               :: !(Maybe Nat)
  , _drfAuthenticationToken :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRootFolders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'drfLimit' - The maximum number of items to return.
--
-- * 'drfAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
describeRootFolders
    :: Text -- ^ 'drfAuthenticationToken'
    -> DescribeRootFolders
describeRootFolders pAuthenticationToken_ =
  DescribeRootFolders'
    { _drfMarker = Nothing
    , _drfLimit = Nothing
    , _drfAuthenticationToken = _Sensitive # pAuthenticationToken_
    }


-- | The marker for the next set of results. (You received this marker from a previous call.)
drfMarker :: Lens' DescribeRootFolders (Maybe Text)
drfMarker = lens _drfMarker (\ s a -> s{_drfMarker = a})

-- | The maximum number of items to return.
drfLimit :: Lens' DescribeRootFolders (Maybe Natural)
drfLimit = lens _drfLimit (\ s a -> s{_drfLimit = a}) . mapping _Nat

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
drfAuthenticationToken :: Lens' DescribeRootFolders Text
drfAuthenticationToken = lens _drfAuthenticationToken (\ s a -> s{_drfAuthenticationToken = a}) . _Sensitive

instance AWSRequest DescribeRootFolders where
        type Rs DescribeRootFolders =
             DescribeRootFoldersResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRootFoldersResponse' <$>
                   (x .?> "Folders" .!@ mempty) <*> (x .?> "Marker") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRootFolders where

instance NFData DescribeRootFolders where

instance ToHeaders DescribeRootFolders where
        toHeaders DescribeRootFolders'{..}
          = mconcat
              ["Authentication" =# _drfAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeRootFolders where
        toPath = const "/api/v1/me/root"

instance ToQuery DescribeRootFolders where
        toQuery DescribeRootFolders'{..}
          = mconcat
              ["marker" =: _drfMarker, "limit" =: _drfLimit]

-- | /See:/ 'describeRootFoldersResponse' smart constructor.
data DescribeRootFoldersResponse = DescribeRootFoldersResponse'
  { _drfrsFolders        :: !(Maybe [FolderMetadata])
  , _drfrsMarker         :: !(Maybe Text)
  , _drfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRootFoldersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfrsFolders' - The user's special folders.
--
-- * 'drfrsMarker' - The marker for the next set of results.
--
-- * 'drfrsResponseStatus' - -- | The response status code.
describeRootFoldersResponse
    :: Int -- ^ 'drfrsResponseStatus'
    -> DescribeRootFoldersResponse
describeRootFoldersResponse pResponseStatus_ =
  DescribeRootFoldersResponse'
    { _drfrsFolders = Nothing
    , _drfrsMarker = Nothing
    , _drfrsResponseStatus = pResponseStatus_
    }


-- | The user's special folders.
drfrsFolders :: Lens' DescribeRootFoldersResponse [FolderMetadata]
drfrsFolders = lens _drfrsFolders (\ s a -> s{_drfrsFolders = a}) . _Default . _Coerce

-- | The marker for the next set of results.
drfrsMarker :: Lens' DescribeRootFoldersResponse (Maybe Text)
drfrsMarker = lens _drfrsMarker (\ s a -> s{_drfrsMarker = a})

-- | -- | The response status code.
drfrsResponseStatus :: Lens' DescribeRootFoldersResponse Int
drfrsResponseStatus = lens _drfrsResponseStatus (\ s a -> s{_drfrsResponseStatus = a})

instance NFData DescribeRootFoldersResponse where
