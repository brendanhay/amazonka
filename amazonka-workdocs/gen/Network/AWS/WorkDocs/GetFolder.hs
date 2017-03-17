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
-- Module      : Network.AWS.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
--
--
module Network.AWS.WorkDocs.GetFolder
    (
    -- * Creating a Request
      getFolder
    , GetFolder
    -- * Request Lenses
    , gfFolderId

    -- * Destructuring the Response
    , getFolderResponse
    , GetFolderResponse
    -- * Response Lenses
    , gfrsMetadata
    , gfrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'getFolder' smart constructor.
newtype GetFolder = GetFolder'
    { _gfFolderId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfFolderId' - The ID of the folder.
getFolder
    :: Text -- ^ 'gfFolderId'
    -> GetFolder
getFolder pFolderId_ =
    GetFolder'
    { _gfFolderId = pFolderId_
    }

-- | The ID of the folder.
gfFolderId :: Lens' GetFolder Text
gfFolderId = lens _gfFolderId (\ s a -> s{_gfFolderId = a});

instance AWSRequest GetFolder where
        type Rs GetFolder = GetFolderResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 GetFolderResponse' <$>
                   (x .?> "Metadata") <*> (pure (fromEnum s)))

instance Hashable GetFolder

instance NFData GetFolder

instance ToHeaders GetFolder where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetFolder where
        toPath GetFolder'{..}
          = mconcat ["/api/v1/folders/", toBS _gfFolderId]

instance ToQuery GetFolder where
        toQuery = const mempty

-- | /See:/ 'getFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
    { _gfrsMetadata       :: !(Maybe FolderMetadata)
    , _gfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetFolderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsMetadata' - The metadata of the folder.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
getFolderResponse
    :: Int -- ^ 'gfrsResponseStatus'
    -> GetFolderResponse
getFolderResponse pResponseStatus_ =
    GetFolderResponse'
    { _gfrsMetadata = Nothing
    , _gfrsResponseStatus = pResponseStatus_
    }

-- | The metadata of the folder.
gfrsMetadata :: Lens' GetFolderResponse (Maybe FolderMetadata)
gfrsMetadata = lens _gfrsMetadata (\ s a -> s{_gfrsMetadata = a});

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFolderResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\ s a -> s{_gfrsResponseStatus = a});

instance NFData GetFolderResponse
