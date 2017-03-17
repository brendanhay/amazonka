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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , dfFolderId

    -- * Destructuring the Response
    , deleteFolderResponse
    , DeleteFolderResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteFolder' smart constructor.
newtype DeleteFolder = DeleteFolder'
    { _dfFolderId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfFolderId' - The ID of the folder.
deleteFolder
    :: Text -- ^ 'dfFolderId'
    -> DeleteFolder
deleteFolder pFolderId_ =
    DeleteFolder'
    { _dfFolderId = pFolderId_
    }

-- | The ID of the folder.
dfFolderId :: Lens' DeleteFolder Text
dfFolderId = lens _dfFolderId (\ s a -> s{_dfFolderId = a});

instance AWSRequest DeleteFolder where
        type Rs DeleteFolder = DeleteFolderResponse
        request = delete workDocs
        response = receiveNull DeleteFolderResponse'

instance Hashable DeleteFolder

instance NFData DeleteFolder

instance ToHeaders DeleteFolder where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteFolder where
        toPath DeleteFolder'{..}
          = mconcat ["/api/v1/folders/", toBS _dfFolderId]

instance ToQuery DeleteFolder where
        toQuery = const mempty

-- | /See:/ 'deleteFolderResponse' smart constructor.
data DeleteFolderResponse =
    DeleteFolderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFolderResponse' with the minimum fields required to make a request.
--
deleteFolderResponse
    :: DeleteFolderResponse
deleteFolderResponse = DeleteFolderResponse'

instance NFData DeleteFolderResponse
