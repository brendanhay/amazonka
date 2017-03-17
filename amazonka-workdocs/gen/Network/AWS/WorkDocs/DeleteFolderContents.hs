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
-- Module      : Network.AWS.WorkDocs.DeleteFolderContents
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the contents of the specified folder.
--
--
module Network.AWS.WorkDocs.DeleteFolderContents
    (
    -- * Creating a Request
      deleteFolderContents
    , DeleteFolderContents
    -- * Request Lenses
    , dfcFolderId

    -- * Destructuring the Response
    , deleteFolderContentsResponse
    , DeleteFolderContentsResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteFolderContents' smart constructor.
newtype DeleteFolderContents = DeleteFolderContents'
    { _dfcFolderId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFolderContents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcFolderId' - The ID of the folder.
deleteFolderContents
    :: Text -- ^ 'dfcFolderId'
    -> DeleteFolderContents
deleteFolderContents pFolderId_ =
    DeleteFolderContents'
    { _dfcFolderId = pFolderId_
    }

-- | The ID of the folder.
dfcFolderId :: Lens' DeleteFolderContents Text
dfcFolderId = lens _dfcFolderId (\ s a -> s{_dfcFolderId = a});

instance AWSRequest DeleteFolderContents where
        type Rs DeleteFolderContents =
             DeleteFolderContentsResponse
        request = delete workDocs
        response = receiveNull DeleteFolderContentsResponse'

instance Hashable DeleteFolderContents

instance NFData DeleteFolderContents

instance ToHeaders DeleteFolderContents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteFolderContents where
        toPath DeleteFolderContents'{..}
          = mconcat
              ["/api/v1/folders/", toBS _dfcFolderId, "/contents"]

instance ToQuery DeleteFolderContents where
        toQuery = const mempty

-- | /See:/ 'deleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse =
    DeleteFolderContentsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFolderContentsResponse' with the minimum fields required to make a request.
--
deleteFolderContentsResponse
    :: DeleteFolderContentsResponse
deleteFolderContentsResponse = DeleteFolderContentsResponse'

instance NFData DeleteFolderContentsResponse
