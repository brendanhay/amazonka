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
-- Module      : Network.AWS.WorkDocs.UpdateFolder
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.
--
--
module Network.AWS.WorkDocs.UpdateFolder
    (
    -- * Creating a Request
      updateFolder
    , UpdateFolder
    -- * Request Lenses
    , ufParentFolderId
    , ufName
    , ufResourceState
    , ufFolderId

    -- * Destructuring the Response
    , updateFolderResponse
    , UpdateFolderResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkDocs.Types
import           Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'updateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
    { _ufParentFolderId :: !(Maybe Text)
    , _ufName           :: !(Maybe Text)
    , _ufResourceState  :: !(Maybe ResourceStateType)
    , _ufFolderId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFolder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufParentFolderId' - The ID of the parent folder.
--
-- * 'ufName' - The name of the folder.
--
-- * 'ufResourceState' - The resource state of the folder. Note that only ACTIVE and RECYCLED are accepted values from the API.
--
-- * 'ufFolderId' - The ID of the folder.
updateFolder
    :: Text -- ^ 'ufFolderId'
    -> UpdateFolder
updateFolder pFolderId_ =
    UpdateFolder'
    { _ufParentFolderId = Nothing
    , _ufName = Nothing
    , _ufResourceState = Nothing
    , _ufFolderId = pFolderId_
    }

-- | The ID of the parent folder.
ufParentFolderId :: Lens' UpdateFolder (Maybe Text)
ufParentFolderId = lens _ufParentFolderId (\ s a -> s{_ufParentFolderId = a});

-- | The name of the folder.
ufName :: Lens' UpdateFolder (Maybe Text)
ufName = lens _ufName (\ s a -> s{_ufName = a});

-- | The resource state of the folder. Note that only ACTIVE and RECYCLED are accepted values from the API.
ufResourceState :: Lens' UpdateFolder (Maybe ResourceStateType)
ufResourceState = lens _ufResourceState (\ s a -> s{_ufResourceState = a});

-- | The ID of the folder.
ufFolderId :: Lens' UpdateFolder Text
ufFolderId = lens _ufFolderId (\ s a -> s{_ufFolderId = a});

instance AWSRequest UpdateFolder where
        type Rs UpdateFolder = UpdateFolderResponse
        request = patchJSON workDocs
        response = receiveNull UpdateFolderResponse'

instance Hashable UpdateFolder

instance NFData UpdateFolder

instance ToHeaders UpdateFolder where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFolder where
        toJSON UpdateFolder'{..}
          = object
              (catMaybes
                 [("ParentFolderId" .=) <$> _ufParentFolderId,
                  ("Name" .=) <$> _ufName,
                  ("ResourceState" .=) <$> _ufResourceState])

instance ToPath UpdateFolder where
        toPath UpdateFolder'{..}
          = mconcat ["/api/v1/folders/", toBS _ufFolderId]

instance ToQuery UpdateFolder where
        toQuery = const mempty

-- | /See:/ 'updateFolderResponse' smart constructor.
data UpdateFolderResponse =
    UpdateFolderResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFolderResponse' with the minimum fields required to make a request.
--
updateFolderResponse
    :: UpdateFolderResponse
updateFolderResponse = UpdateFolderResponse'

instance NFData UpdateFolderResponse
