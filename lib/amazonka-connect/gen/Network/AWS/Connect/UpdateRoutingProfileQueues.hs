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
-- Module      : Network.AWS.Connect.UpdateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties associated with a set of queues for a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileQueues
  ( -- * Creating a Request
    updateRoutingProfileQueues,
    UpdateRoutingProfileQueues,

    -- * Request Lenses
    urpqInstanceId,
    urpqRoutingProfileId,
    urpqQueueConfigs,

    -- * Destructuring the Response
    updateRoutingProfileQueuesResponse,
    UpdateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoutingProfileQueues' smart constructor.
data UpdateRoutingProfileQueues = UpdateRoutingProfileQueues'
  { _urpqInstanceId ::
      !Text,
    _urpqRoutingProfileId :: !Text,
    _urpqQueueConfigs ::
      !(List1 RoutingProfileQueueConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpqInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'urpqRoutingProfileId' - The identifier of the routing profile.
--
-- * 'urpqQueueConfigs' - The queues to be updated for this routing profile.
updateRoutingProfileQueues ::
  -- | 'urpqInstanceId'
  Text ->
  -- | 'urpqRoutingProfileId'
  Text ->
  -- | 'urpqQueueConfigs'
  NonEmpty RoutingProfileQueueConfig ->
  UpdateRoutingProfileQueues
updateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    UpdateRoutingProfileQueues'
      { _urpqInstanceId = pInstanceId_,
        _urpqRoutingProfileId = pRoutingProfileId_,
        _urpqQueueConfigs = _List1 # pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance.
urpqInstanceId :: Lens' UpdateRoutingProfileQueues Text
urpqInstanceId = lens _urpqInstanceId (\s a -> s {_urpqInstanceId = a})

-- | The identifier of the routing profile.
urpqRoutingProfileId :: Lens' UpdateRoutingProfileQueues Text
urpqRoutingProfileId = lens _urpqRoutingProfileId (\s a -> s {_urpqRoutingProfileId = a})

-- | The queues to be updated for this routing profile.
urpqQueueConfigs :: Lens' UpdateRoutingProfileQueues (NonEmpty RoutingProfileQueueConfig)
urpqQueueConfigs = lens _urpqQueueConfigs (\s a -> s {_urpqQueueConfigs = a}) . _List1

instance AWSRequest UpdateRoutingProfileQueues where
  type
    Rs UpdateRoutingProfileQueues =
      UpdateRoutingProfileQueuesResponse
  request = postJSON connect
  response = receiveNull UpdateRoutingProfileQueuesResponse'

instance Hashable UpdateRoutingProfileQueues

instance NFData UpdateRoutingProfileQueues

instance ToHeaders UpdateRoutingProfileQueues where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateRoutingProfileQueues where
  toJSON UpdateRoutingProfileQueues' {..} =
    object (catMaybes [Just ("QueueConfigs" .= _urpqQueueConfigs)])

instance ToPath UpdateRoutingProfileQueues where
  toPath UpdateRoutingProfileQueues' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _urpqInstanceId,
        "/",
        toBS _urpqRoutingProfileId,
        "/queues"
      ]

instance ToQuery UpdateRoutingProfileQueues where
  toQuery = const mempty

-- | /See:/ 'updateRoutingProfileQueuesResponse' smart constructor.
data UpdateRoutingProfileQueuesResponse = UpdateRoutingProfileQueuesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
updateRoutingProfileQueuesResponse ::
  UpdateRoutingProfileQueuesResponse
updateRoutingProfileQueuesResponse =
  UpdateRoutingProfileQueuesResponse'

instance NFData UpdateRoutingProfileQueuesResponse
