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
-- Module      : Network.AWS.FMS.DeleteProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager protocols list.
module Network.AWS.FMS.DeleteProtocolsList
  ( -- * Creating a Request
    deleteProtocolsList,
    DeleteProtocolsList,

    -- * Request Lenses
    dplListId,

    -- * Destructuring the Response
    deleteProtocolsListResponse,
    DeleteProtocolsListResponse,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProtocolsList' smart constructor.
newtype DeleteProtocolsList = DeleteProtocolsList'
  { _dplListId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProtocolsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dplListId' - The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
deleteProtocolsList ::
  -- | 'dplListId'
  Text ->
  DeleteProtocolsList
deleteProtocolsList pListId_ =
  DeleteProtocolsList' {_dplListId = pListId_}

-- | The ID of the protocols list that you want to delete. You can retrieve this ID from @PutProtocolsList@ , @ListProtocolsLists@ , and @GetProtocolsLost@ .
dplListId :: Lens' DeleteProtocolsList Text
dplListId = lens _dplListId (\s a -> s {_dplListId = a})

instance AWSRequest DeleteProtocolsList where
  type Rs DeleteProtocolsList = DeleteProtocolsListResponse
  request = postJSON fms
  response = receiveNull DeleteProtocolsListResponse'

instance Hashable DeleteProtocolsList

instance NFData DeleteProtocolsList

instance ToHeaders DeleteProtocolsList where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSFMS_20180101.DeleteProtocolsList" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteProtocolsList where
  toJSON DeleteProtocolsList' {..} =
    object (catMaybes [Just ("ListId" .= _dplListId)])

instance ToPath DeleteProtocolsList where
  toPath = const "/"

instance ToQuery DeleteProtocolsList where
  toQuery = const mempty

-- | /See:/ 'deleteProtocolsListResponse' smart constructor.
data DeleteProtocolsListResponse = DeleteProtocolsListResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProtocolsListResponse' with the minimum fields required to make a request.
deleteProtocolsListResponse ::
  DeleteProtocolsListResponse
deleteProtocolsListResponse = DeleteProtocolsListResponse'

instance NFData DeleteProtocolsListResponse
