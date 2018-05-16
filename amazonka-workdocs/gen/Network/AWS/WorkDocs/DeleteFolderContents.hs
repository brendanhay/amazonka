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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , dfcAuthenticationToken
    , dfcFolderId

    -- * Destructuring the Response
    , deleteFolderContentsResponse
    , DeleteFolderContentsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteFolderContents' smart constructor.
data DeleteFolderContents = DeleteFolderContents'
  { _dfcAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dfcFolderId            :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFolderContents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dfcFolderId' - The ID of the folder.
deleteFolderContents
    :: Text -- ^ 'dfcFolderId'
    -> DeleteFolderContents
deleteFolderContents pFolderId_ =
  DeleteFolderContents'
    {_dfcAuthenticationToken = Nothing, _dfcFolderId = pFolderId_}


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dfcAuthenticationToken :: Lens' DeleteFolderContents (Maybe Text)
dfcAuthenticationToken = lens _dfcAuthenticationToken (\ s a -> s{_dfcAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the folder.
dfcFolderId :: Lens' DeleteFolderContents Text
dfcFolderId = lens _dfcFolderId (\ s a -> s{_dfcFolderId = a})

instance AWSRequest DeleteFolderContents where
        type Rs DeleteFolderContents =
             DeleteFolderContentsResponse
        request = delete workDocs
        response = receiveNull DeleteFolderContentsResponse'

instance Hashable DeleteFolderContents where

instance NFData DeleteFolderContents where

instance ToHeaders DeleteFolderContents where
        toHeaders DeleteFolderContents'{..}
          = mconcat
              ["Authentication" =# _dfcAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteFolderContents where
        toPath DeleteFolderContents'{..}
          = mconcat
              ["/api/v1/folders/", toBS _dfcFolderId, "/contents"]

instance ToQuery DeleteFolderContents where
        toQuery = const mempty

-- | /See:/ 'deleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse =
  DeleteFolderContentsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFolderContentsResponse' with the minimum fields required to make a request.
--
deleteFolderContentsResponse
    :: DeleteFolderContentsResponse
deleteFolderContentsResponse = DeleteFolderContentsResponse'


instance NFData DeleteFolderContentsResponse where
