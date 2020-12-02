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
-- Module      : Network.AWS.Connect.UpdateUserHierarchyStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the user hierarchy structure: add, remove, and rename user hierarchy levels.
module Network.AWS.Connect.UpdateUserHierarchyStructure
  ( -- * Creating a Request
    updateUserHierarchyStructure,
    UpdateUserHierarchyStructure,

    -- * Request Lenses
    uuhsHierarchyStructure,
    uuhsInstanceId,

    -- * Destructuring the Response
    updateUserHierarchyStructureResponse,
    UpdateUserHierarchyStructureResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserHierarchyStructure' smart constructor.
data UpdateUserHierarchyStructure = UpdateUserHierarchyStructure'
  { _uuhsHierarchyStructure ::
      !HierarchyStructureUpdate,
    _uuhsInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserHierarchyStructure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuhsHierarchyStructure' - The hierarchy levels to update.
--
-- * 'uuhsInstanceId' - The identifier of the Amazon Connect instance.
updateUserHierarchyStructure ::
  -- | 'uuhsHierarchyStructure'
  HierarchyStructureUpdate ->
  -- | 'uuhsInstanceId'
  Text ->
  UpdateUserHierarchyStructure
updateUserHierarchyStructure pHierarchyStructure_ pInstanceId_ =
  UpdateUserHierarchyStructure'
    { _uuhsHierarchyStructure =
        pHierarchyStructure_,
      _uuhsInstanceId = pInstanceId_
    }

-- | The hierarchy levels to update.
uuhsHierarchyStructure :: Lens' UpdateUserHierarchyStructure HierarchyStructureUpdate
uuhsHierarchyStructure = lens _uuhsHierarchyStructure (\s a -> s {_uuhsHierarchyStructure = a})

-- | The identifier of the Amazon Connect instance.
uuhsInstanceId :: Lens' UpdateUserHierarchyStructure Text
uuhsInstanceId = lens _uuhsInstanceId (\s a -> s {_uuhsInstanceId = a})

instance AWSRequest UpdateUserHierarchyStructure where
  type
    Rs UpdateUserHierarchyStructure =
      UpdateUserHierarchyStructureResponse
  request = postJSON connect
  response = receiveNull UpdateUserHierarchyStructureResponse'

instance Hashable UpdateUserHierarchyStructure

instance NFData UpdateUserHierarchyStructure

instance ToHeaders UpdateUserHierarchyStructure where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserHierarchyStructure where
  toJSON UpdateUserHierarchyStructure' {..} =
    object
      ( catMaybes
          [Just ("HierarchyStructure" .= _uuhsHierarchyStructure)]
      )

instance ToPath UpdateUserHierarchyStructure where
  toPath UpdateUserHierarchyStructure' {..} =
    mconcat ["/user-hierarchy-structure/", toBS _uuhsInstanceId]

instance ToQuery UpdateUserHierarchyStructure where
  toQuery = const mempty

-- | /See:/ 'updateUserHierarchyStructureResponse' smart constructor.
data UpdateUserHierarchyStructureResponse = UpdateUserHierarchyStructureResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserHierarchyStructureResponse' with the minimum fields required to make a request.
updateUserHierarchyStructureResponse ::
  UpdateUserHierarchyStructureResponse
updateUserHierarchyStructureResponse =
  UpdateUserHierarchyStructureResponse'

instance NFData UpdateUserHierarchyStructureResponse
