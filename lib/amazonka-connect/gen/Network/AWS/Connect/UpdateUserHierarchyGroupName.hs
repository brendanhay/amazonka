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
-- Module      : Network.AWS.Connect.UpdateUserHierarchyGroupName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of the user hierarchy group.
module Network.AWS.Connect.UpdateUserHierarchyGroupName
  ( -- * Creating a Request
    updateUserHierarchyGroupName,
    UpdateUserHierarchyGroupName,

    -- * Request Lenses
    uuhgnName,
    uuhgnHierarchyGroupId,
    uuhgnInstanceId,

    -- * Destructuring the Response
    updateUserHierarchyGroupNameResponse,
    UpdateUserHierarchyGroupNameResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserHierarchyGroupName' smart constructor.
data UpdateUserHierarchyGroupName = UpdateUserHierarchyGroupName'
  { _uuhgnName ::
      !Text,
    _uuhgnHierarchyGroupId :: !Text,
    _uuhgnInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserHierarchyGroupName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuhgnName' - The name of the hierarchy group. Must not be more than 100 characters.
--
-- * 'uuhgnHierarchyGroupId' - The identifier of the hierarchy group.
--
-- * 'uuhgnInstanceId' - The identifier of the Amazon Connect instance.
updateUserHierarchyGroupName ::
  -- | 'uuhgnName'
  Text ->
  -- | 'uuhgnHierarchyGroupId'
  Text ->
  -- | 'uuhgnInstanceId'
  Text ->
  UpdateUserHierarchyGroupName
updateUserHierarchyGroupName pName_ pHierarchyGroupId_ pInstanceId_ =
  UpdateUserHierarchyGroupName'
    { _uuhgnName = pName_,
      _uuhgnHierarchyGroupId = pHierarchyGroupId_,
      _uuhgnInstanceId = pInstanceId_
    }

-- | The name of the hierarchy group. Must not be more than 100 characters.
uuhgnName :: Lens' UpdateUserHierarchyGroupName Text
uuhgnName = lens _uuhgnName (\s a -> s {_uuhgnName = a})

-- | The identifier of the hierarchy group.
uuhgnHierarchyGroupId :: Lens' UpdateUserHierarchyGroupName Text
uuhgnHierarchyGroupId = lens _uuhgnHierarchyGroupId (\s a -> s {_uuhgnHierarchyGroupId = a})

-- | The identifier of the Amazon Connect instance.
uuhgnInstanceId :: Lens' UpdateUserHierarchyGroupName Text
uuhgnInstanceId = lens _uuhgnInstanceId (\s a -> s {_uuhgnInstanceId = a})

instance AWSRequest UpdateUserHierarchyGroupName where
  type
    Rs UpdateUserHierarchyGroupName =
      UpdateUserHierarchyGroupNameResponse
  request = postJSON connect
  response = receiveNull UpdateUserHierarchyGroupNameResponse'

instance Hashable UpdateUserHierarchyGroupName

instance NFData UpdateUserHierarchyGroupName

instance ToHeaders UpdateUserHierarchyGroupName where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserHierarchyGroupName where
  toJSON UpdateUserHierarchyGroupName' {..} =
    object (catMaybes [Just ("Name" .= _uuhgnName)])

instance ToPath UpdateUserHierarchyGroupName where
  toPath UpdateUserHierarchyGroupName' {..} =
    mconcat
      [ "/user-hierarchy-groups/",
        toBS _uuhgnInstanceId,
        "/",
        toBS _uuhgnHierarchyGroupId,
        "/name"
      ]

instance ToQuery UpdateUserHierarchyGroupName where
  toQuery = const mempty

-- | /See:/ 'updateUserHierarchyGroupNameResponse' smart constructor.
data UpdateUserHierarchyGroupNameResponse = UpdateUserHierarchyGroupNameResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserHierarchyGroupNameResponse' with the minimum fields required to make a request.
updateUserHierarchyGroupNameResponse ::
  UpdateUserHierarchyGroupNameResponse
updateUserHierarchyGroupNameResponse =
  UpdateUserHierarchyGroupNameResponse'

instance NFData UpdateUserHierarchyGroupNameResponse
