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
-- Module      : Network.AWS.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
--
--
module Network.AWS.WorkDocs.DeleteFolder
    (
    -- * Creating a Request
      deleteFolder
    , DeleteFolder
    -- * Request Lenses
    , dfAuthenticationToken
    , dfFolderId

    -- * Destructuring the Response
    , deleteFolderResponse
    , DeleteFolderResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { _dfAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dfFolderId            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dfFolderId' - The ID of the folder.
deleteFolder
    :: Text -- ^ 'dfFolderId'
    -> DeleteFolder
deleteFolder pFolderId_ =
  DeleteFolder' {_dfAuthenticationToken = Nothing, _dfFolderId = pFolderId_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dfAuthenticationToken :: Lens' DeleteFolder (Maybe Text)
dfAuthenticationToken = lens _dfAuthenticationToken (\ s a -> s{_dfAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the folder.
dfFolderId :: Lens' DeleteFolder Text
dfFolderId = lens _dfFolderId (\ s a -> s{_dfFolderId = a})

instance AWSRequest DeleteFolder where
        type Rs DeleteFolder = DeleteFolderResponse
        request = delete workDocs
        response = receiveNull DeleteFolderResponse'

instance Hashable DeleteFolder where

instance NFData DeleteFolder where

instance ToHeaders DeleteFolder where
        toHeaders DeleteFolder'{..}
          = mconcat
              ["Authentication" =# _dfAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteFolder where
        toPath DeleteFolder'{..}
          = mconcat ["/api/v1/folders/", toBS _dfFolderId]

instance ToQuery DeleteFolder where
        toQuery = const mempty

-- | /See:/ 'deleteFolderResponse' smart constructor.
data DeleteFolderResponse =
  DeleteFolderResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFolderResponse' with the minimum fields required to make a request.
--
deleteFolderResponse
    :: DeleteFolderResponse
deleteFolderResponse = DeleteFolderResponse'


instance NFData DeleteFolderResponse where
