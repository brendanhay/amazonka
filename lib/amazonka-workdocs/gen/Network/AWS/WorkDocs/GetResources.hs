{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of resources, including folders and documents. The only @CollectionType@ supported is @SHARED_WITH_ME@ .
module Network.AWS.WorkDocs.GetResources
  ( -- * Creating a Request
    getResources,
    GetResources,

    -- * Request Lenses
    grAuthenticationToken,
    grUserId,
    grMarker,
    grLimit,
    grCollectionType,

    -- * Destructuring the Response
    getResourcesResponse,
    GetResourcesResponse,

    -- * Response Lenses
    grrsFolders,
    grrsDocuments,
    grrsMarker,
    grrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'getResources' smart constructor.
data GetResources = GetResources'
  { _grAuthenticationToken ::
      !(Maybe (Sensitive Text)),
    _grUserId :: !(Maybe Text),
    _grMarker :: !(Maybe Text),
    _grLimit :: !(Maybe Nat),
    _grCollectionType :: !(Maybe ResourceCollectionType)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grAuthenticationToken' - The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- * 'grUserId' - The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
--
-- * 'grMarker' - The marker for the next set of results. This marker was received from a previous call.
--
-- * 'grLimit' - The maximum number of resources to return.
--
-- * 'grCollectionType' - The collection type.
getResources ::
  GetResources
getResources =
  GetResources'
    { _grAuthenticationToken = Nothing,
      _grUserId = Nothing,
      _grMarker = Nothing,
      _grLimit = Nothing,
      _grCollectionType = Nothing
    }

-- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
grAuthenticationToken :: Lens' GetResources (Maybe Text)
grAuthenticationToken = lens _grAuthenticationToken (\s a -> s {_grAuthenticationToken = a}) . mapping _Sensitive

-- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
grUserId :: Lens' GetResources (Maybe Text)
grUserId = lens _grUserId (\s a -> s {_grUserId = a})

-- | The marker for the next set of results. This marker was received from a previous call.
grMarker :: Lens' GetResources (Maybe Text)
grMarker = lens _grMarker (\s a -> s {_grMarker = a})

-- | The maximum number of resources to return.
grLimit :: Lens' GetResources (Maybe Natural)
grLimit = lens _grLimit (\s a -> s {_grLimit = a}) . mapping _Nat

-- | The collection type.
grCollectionType :: Lens' GetResources (Maybe ResourceCollectionType)
grCollectionType = lens _grCollectionType (\s a -> s {_grCollectionType = a})

instance AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request = get workDocs
  response =
    receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            <$> (x .?> "Folders" .!@ mempty)
            <*> (x .?> "Documents" .!@ mempty)
            <*> (x .?> "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable GetResources

instance NFData GetResources

instance ToHeaders GetResources where
  toHeaders GetResources' {..} =
    mconcat
      [ "Authentication" =# _grAuthenticationToken,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToPath GetResources where
  toPath = const "/api/v1/resources"

instance ToQuery GetResources where
  toQuery GetResources' {..} =
    mconcat
      [ "userId" =: _grUserId,
        "marker" =: _grMarker,
        "limit" =: _grLimit,
        "collectionType" =: _grCollectionType
      ]

-- | /See:/ 'getResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { _grrsFolders ::
      !(Maybe [FolderMetadata]),
    _grrsDocuments :: !(Maybe [DocumentMetadata]),
    _grrsMarker :: !(Maybe Text),
    _grrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsFolders' - The folders in the specified folder.
--
-- * 'grrsDocuments' - The documents in the specified collection.
--
-- * 'grrsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getResourcesResponse ::
  -- | 'grrsResponseStatus'
  Int ->
  GetResourcesResponse
getResourcesResponse pResponseStatus_ =
  GetResourcesResponse'
    { _grrsFolders = Nothing,
      _grrsDocuments = Nothing,
      _grrsMarker = Nothing,
      _grrsResponseStatus = pResponseStatus_
    }

-- | The folders in the specified folder.
grrsFolders :: Lens' GetResourcesResponse [FolderMetadata]
grrsFolders = lens _grrsFolders (\s a -> s {_grrsFolders = a}) . _Default . _Coerce

-- | The documents in the specified collection.
grrsDocuments :: Lens' GetResourcesResponse [DocumentMetadata]
grrsDocuments = lens _grrsDocuments (\s a -> s {_grrsDocuments = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
grrsMarker :: Lens' GetResourcesResponse (Maybe Text)
grrsMarker = lens _grrsMarker (\s a -> s {_grrsMarker = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetResourcesResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\s a -> s {_grrsResponseStatus = a})

instance NFData GetResourcesResponse
