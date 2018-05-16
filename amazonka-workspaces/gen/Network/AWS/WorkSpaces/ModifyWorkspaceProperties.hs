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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified WorkSpace properties.
--
--
module Network.AWS.WorkSpaces.ModifyWorkspaceProperties
    (
    -- * Creating a Request
      modifyWorkspaceProperties
    , ModifyWorkspaceProperties
    -- * Request Lenses
    , mwpWorkspaceId
    , mwpWorkspaceProperties

    -- * Destructuring the Response
    , modifyWorkspacePropertiesResponse
    , ModifyWorkspacePropertiesResponse
    -- * Response Lenses
    , mwprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'modifyWorkspaceProperties' smart constructor.
data ModifyWorkspaceProperties = ModifyWorkspaceProperties'
  { _mwpWorkspaceId         :: !Text
  , _mwpWorkspaceProperties :: !WorkspaceProperties
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyWorkspaceProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwpWorkspaceId' - The ID of the WorkSpace.
--
-- * 'mwpWorkspaceProperties' - The properties of the WorkSpace.
modifyWorkspaceProperties
    :: Text -- ^ 'mwpWorkspaceId'
    -> WorkspaceProperties -- ^ 'mwpWorkspaceProperties'
    -> ModifyWorkspaceProperties
modifyWorkspaceProperties pWorkspaceId_ pWorkspaceProperties_ =
  ModifyWorkspaceProperties'
    { _mwpWorkspaceId = pWorkspaceId_
    , _mwpWorkspaceProperties = pWorkspaceProperties_
    }


-- | The ID of the WorkSpace.
mwpWorkspaceId :: Lens' ModifyWorkspaceProperties Text
mwpWorkspaceId = lens _mwpWorkspaceId (\ s a -> s{_mwpWorkspaceId = a})

-- | The properties of the WorkSpace.
mwpWorkspaceProperties :: Lens' ModifyWorkspaceProperties WorkspaceProperties
mwpWorkspaceProperties = lens _mwpWorkspaceProperties (\ s a -> s{_mwpWorkspaceProperties = a})

instance AWSRequest ModifyWorkspaceProperties where
        type Rs ModifyWorkspaceProperties =
             ModifyWorkspacePropertiesResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 ModifyWorkspacePropertiesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ModifyWorkspaceProperties where

instance NFData ModifyWorkspaceProperties where

instance ToHeaders ModifyWorkspaceProperties where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.ModifyWorkspaceProperties" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyWorkspaceProperties where
        toJSON ModifyWorkspaceProperties'{..}
          = object
              (catMaybes
                 [Just ("WorkspaceId" .= _mwpWorkspaceId),
                  Just
                    ("WorkspaceProperties" .= _mwpWorkspaceProperties)])

instance ToPath ModifyWorkspaceProperties where
        toPath = const "/"

instance ToQuery ModifyWorkspaceProperties where
        toQuery = const mempty

-- | /See:/ 'modifyWorkspacePropertiesResponse' smart constructor.
newtype ModifyWorkspacePropertiesResponse = ModifyWorkspacePropertiesResponse'
  { _mwprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyWorkspacePropertiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwprsResponseStatus' - -- | The response status code.
modifyWorkspacePropertiesResponse
    :: Int -- ^ 'mwprsResponseStatus'
    -> ModifyWorkspacePropertiesResponse
modifyWorkspacePropertiesResponse pResponseStatus_ =
  ModifyWorkspacePropertiesResponse' {_mwprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
mwprsResponseStatus :: Lens' ModifyWorkspacePropertiesResponse Int
mwprsResponseStatus = lens _mwprsResponseStatus (\ s a -> s{_mwprsResponseStatus = a})

instance NFData ModifyWorkspacePropertiesResponse
         where
