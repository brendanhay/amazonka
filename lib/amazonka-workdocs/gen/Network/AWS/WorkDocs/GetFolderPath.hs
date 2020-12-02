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
-- Module      : Network.AWS.WorkDocs.GetFolderPath
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the specified folder.
--
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.
--
module Network.AWS.WorkDocs.GetFolderPath
    (
    -- * Creating a Request
      getFolderPath
    , GetFolderPath
    -- * Request Lenses
    , gfpAuthenticationToken
    , gfpMarker
    , gfpLimit
    , gfpFields
    , gfpFolderId

    -- * Destructuring the Response
    , getFolderPathResponse
    , GetFolderPathResponse
    -- * Response Lenses
    , gfprsPath
    , gfprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getFolderPath' smart constructor.
data GetFolderPath = GetFolderPath'
  { _gfpAuthenticationToken :: !(Maybe (Sensitive Text))
  , _gfpMarker              :: !(Maybe Text)
  , _gfpLimit               :: !(Maybe Nat)
  , _gfpFields              :: !(Maybe Text)
  , _gfpFolderId            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFolderPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfpAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'gfpMarker' - This value is not supported.
--
-- * 'gfpLimit' - The maximum number of levels in the hierarchy to return.
--
-- * 'gfpFields' - A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
--
-- * 'gfpFolderId' - The ID of the folder.
getFolderPath
    :: Text -- ^ 'gfpFolderId'
    -> GetFolderPath
getFolderPath pFolderId_ =
  GetFolderPath'
    { _gfpAuthenticationToken = Nothing
    , _gfpMarker = Nothing
    , _gfpLimit = Nothing
    , _gfpFields = Nothing
    , _gfpFolderId = pFolderId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
gfpAuthenticationToken :: Lens' GetFolderPath (Maybe Text)
gfpAuthenticationToken = lens _gfpAuthenticationToken (\ s a -> s{_gfpAuthenticationToken = a}) . mapping _Sensitive

-- | This value is not supported.
gfpMarker :: Lens' GetFolderPath (Maybe Text)
gfpMarker = lens _gfpMarker (\ s a -> s{_gfpMarker = a})

-- | The maximum number of levels in the hierarchy to return.
gfpLimit :: Lens' GetFolderPath (Maybe Natural)
gfpLimit = lens _gfpLimit (\ s a -> s{_gfpLimit = a}) . mapping _Nat

-- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
gfpFields :: Lens' GetFolderPath (Maybe Text)
gfpFields = lens _gfpFields (\ s a -> s{_gfpFields = a})

-- | The ID of the folder.
gfpFolderId :: Lens' GetFolderPath Text
gfpFolderId = lens _gfpFolderId (\ s a -> s{_gfpFolderId = a})

instance AWSRequest GetFolderPath where
        type Rs GetFolderPath = GetFolderPathResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetFolderPathResponse' <$>
                   (x .?> "Path") <*> (pure (fromEnum s)))

instance Hashable GetFolderPath where

instance NFData GetFolderPath where

instance ToHeaders GetFolderPath where
        toHeaders GetFolderPath'{..}
          = mconcat
              ["Authentication" =# _gfpAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath GetFolderPath where
        toPath GetFolderPath'{..}
          = mconcat
              ["/api/v1/folders/", toBS _gfpFolderId, "/path"]

instance ToQuery GetFolderPath where
        toQuery GetFolderPath'{..}
          = mconcat
              ["marker" =: _gfpMarker, "limit" =: _gfpLimit,
               "fields" =: _gfpFields]

-- | /See:/ 'getFolderPathResponse' smart constructor.
data GetFolderPathResponse = GetFolderPathResponse'
  { _gfprsPath           :: !(Maybe ResourcePath)
  , _gfprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFolderPathResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfprsPath' - The path information.
--
-- * 'gfprsResponseStatus' - -- | The response status code.
getFolderPathResponse
    :: Int -- ^ 'gfprsResponseStatus'
    -> GetFolderPathResponse
getFolderPathResponse pResponseStatus_ =
  GetFolderPathResponse'
    {_gfprsPath = Nothing, _gfprsResponseStatus = pResponseStatus_}


-- | The path information.
gfprsPath :: Lens' GetFolderPathResponse (Maybe ResourcePath)
gfprsPath = lens _gfprsPath (\ s a -> s{_gfprsPath = a})

-- | -- | The response status code.
gfprsResponseStatus :: Lens' GetFolderPathResponse Int
gfprsResponseStatus = lens _gfprsResponseStatus (\ s a -> s{_gfprsResponseStatus = a})

instance NFData GetFolderPathResponse where
