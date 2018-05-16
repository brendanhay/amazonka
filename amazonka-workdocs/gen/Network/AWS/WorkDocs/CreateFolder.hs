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
-- Module      : Network.AWS.WorkDocs.CreateFolder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a folder with the specified name and parent folder.
--
--
module Network.AWS.WorkDocs.CreateFolder
    (
    -- * Creating a Request
      createFolder
    , CreateFolder
    -- * Request Lenses
    , cfAuthenticationToken
    , cfName
    , cfParentFolderId

    -- * Destructuring the Response
    , createFolderResponse
    , CreateFolderResponse
    -- * Response Lenses
    , cfrsMetadata
    , cfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createFolder' smart constructor.
data CreateFolder = CreateFolder'
  { _cfAuthenticationToken :: !(Maybe (Sensitive Text))
  , _cfName                :: !(Maybe Text)
  , _cfParentFolderId      :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'cfName' - The name of the new folder.
--
-- * 'cfParentFolderId' - The ID of the parent folder.
createFolder
    :: Text -- ^ 'cfParentFolderId'
    -> CreateFolder
createFolder pParentFolderId_ =
  CreateFolder'
    { _cfAuthenticationToken = Nothing
    , _cfName = Nothing
    , _cfParentFolderId = pParentFolderId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
cfAuthenticationToken :: Lens' CreateFolder (Maybe Text)
cfAuthenticationToken = lens _cfAuthenticationToken (\ s a -> s{_cfAuthenticationToken = a}) . mapping _Sensitive

-- | The name of the new folder.
cfName :: Lens' CreateFolder (Maybe Text)
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | The ID of the parent folder.
cfParentFolderId :: Lens' CreateFolder Text
cfParentFolderId = lens _cfParentFolderId (\ s a -> s{_cfParentFolderId = a})

instance AWSRequest CreateFolder where
        type Rs CreateFolder = CreateFolderResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 CreateFolderResponse' <$>
                   (x .?> "Metadata") <*> (pure (fromEnum s)))

instance Hashable CreateFolder where

instance NFData CreateFolder where

instance ToHeaders CreateFolder where
        toHeaders CreateFolder'{..}
          = mconcat
              ["Authentication" =# _cfAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateFolder where
        toJSON CreateFolder'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _cfName,
                  Just ("ParentFolderId" .= _cfParentFolderId)])

instance ToPath CreateFolder where
        toPath = const "/api/v1/folders"

instance ToQuery CreateFolder where
        toQuery = const mempty

-- | /See:/ 'createFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { _cfrsMetadata       :: !(Maybe FolderMetadata)
  , _cfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFolderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsMetadata' - The metadata of the folder.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFolderResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFolderResponse
createFolderResponse pResponseStatus_ =
  CreateFolderResponse'
    {_cfrsMetadata = Nothing, _cfrsResponseStatus = pResponseStatus_}


-- | The metadata of the folder.
cfrsMetadata :: Lens' CreateFolderResponse (Maybe FolderMetadata)
cfrsMetadata = lens _cfrsMetadata (\ s a -> s{_cfrsMetadata = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFolderResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFolderResponse where
