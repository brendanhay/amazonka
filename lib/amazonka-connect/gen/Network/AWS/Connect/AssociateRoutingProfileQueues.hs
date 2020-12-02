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
-- Module      : Network.AWS.Connect.AssociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of queues with a routing profile.
module Network.AWS.Connect.AssociateRoutingProfileQueues
  ( -- * Creating a Request
    associateRoutingProfileQueues,
    AssociateRoutingProfileQueues,

    -- * Request Lenses
    arpqInstanceId,
    arpqRoutingProfileId,
    arpqQueueConfigs,

    -- * Destructuring the Response
    associateRoutingProfileQueuesResponse,
    AssociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateRoutingProfileQueues' smart constructor.
data AssociateRoutingProfileQueues = AssociateRoutingProfileQueues'
  { _arpqInstanceId ::
      !Text,
    _arpqRoutingProfileId :: !Text,
    _arpqQueueConfigs ::
      !( List1
           RoutingProfileQueueConfig
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arpqInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'arpqRoutingProfileId' - The identifier of the routing profile.
--
-- * 'arpqQueueConfigs' - The queues to associate with this routing profile.
associateRoutingProfileQueues ::
  -- | 'arpqInstanceId'
  Text ->
  -- | 'arpqRoutingProfileId'
  Text ->
  -- | 'arpqQueueConfigs'
  NonEmpty RoutingProfileQueueConfig ->
  AssociateRoutingProfileQueues
associateRoutingProfileQueues
  pInstanceId_
  pRoutingProfileId_
  pQueueConfigs_ =
    AssociateRoutingProfileQueues'
      { _arpqInstanceId = pInstanceId_,
        _arpqRoutingProfileId = pRoutingProfileId_,
        _arpqQueueConfigs = _List1 # pQueueConfigs_
      }

-- | The identifier of the Amazon Connect instance.
arpqInstanceId :: Lens' AssociateRoutingProfileQueues Text
arpqInstanceId = lens _arpqInstanceId (\s a -> s {_arpqInstanceId = a})

-- | The identifier of the routing profile.
arpqRoutingProfileId :: Lens' AssociateRoutingProfileQueues Text
arpqRoutingProfileId = lens _arpqRoutingProfileId (\s a -> s {_arpqRoutingProfileId = a})

-- | The queues to associate with this routing profile.
arpqQueueConfigs :: Lens' AssociateRoutingProfileQueues (NonEmpty RoutingProfileQueueConfig)
arpqQueueConfigs = lens _arpqQueueConfigs (\s a -> s {_arpqQueueConfigs = a}) . _List1

instance AWSRequest AssociateRoutingProfileQueues where
  type
    Rs AssociateRoutingProfileQueues =
      AssociateRoutingProfileQueuesResponse
  request = postJSON connect
  response = receiveNull AssociateRoutingProfileQueuesResponse'

instance Hashable AssociateRoutingProfileQueues

instance NFData AssociateRoutingProfileQueues

instance ToHeaders AssociateRoutingProfileQueues where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateRoutingProfileQueues where
  toJSON AssociateRoutingProfileQueues' {..} =
    object (catMaybes [Just ("QueueConfigs" .= _arpqQueueConfigs)])

instance ToPath AssociateRoutingProfileQueues where
  toPath AssociateRoutingProfileQueues' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _arpqInstanceId,
        "/",
        toBS _arpqRoutingProfileId,
        "/associate-queues"
      ]

instance ToQuery AssociateRoutingProfileQueues where
  toQuery = const mempty

-- | /See:/ 'associateRoutingProfileQueuesResponse' smart constructor.
data AssociateRoutingProfileQueuesResponse = AssociateRoutingProfileQueuesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
associateRoutingProfileQueuesResponse ::
  AssociateRoutingProfileQueuesResponse
associateRoutingProfileQueuesResponse =
  AssociateRoutingProfileQueuesResponse'

instance NFData AssociateRoutingProfileQueuesResponse
