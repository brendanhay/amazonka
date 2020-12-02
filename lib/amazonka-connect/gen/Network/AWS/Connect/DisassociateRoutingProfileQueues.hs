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
-- Module      : Network.AWS.Connect.DisassociateRoutingProfileQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a set of queues from a routing profile.
module Network.AWS.Connect.DisassociateRoutingProfileQueues
  ( -- * Creating a Request
    disassociateRoutingProfileQueues,
    DisassociateRoutingProfileQueues,

    -- * Request Lenses
    drpqInstanceId,
    drpqRoutingProfileId,
    drpqQueueReferences,

    -- * Destructuring the Response
    disassociateRoutingProfileQueuesResponse,
    DisassociateRoutingProfileQueuesResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateRoutingProfileQueues' smart constructor.
data DisassociateRoutingProfileQueues = DisassociateRoutingProfileQueues'
  { _drpqInstanceId ::
      !Text,
    _drpqRoutingProfileId ::
      !Text,
    _drpqQueueReferences ::
      ![RoutingProfileQueueReference]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateRoutingProfileQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpqInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'drpqRoutingProfileId' - The identifier of the routing profile.
--
-- * 'drpqQueueReferences' - The queues to disassociate from this routing profile.
disassociateRoutingProfileQueues ::
  -- | 'drpqInstanceId'
  Text ->
  -- | 'drpqRoutingProfileId'
  Text ->
  DisassociateRoutingProfileQueues
disassociateRoutingProfileQueues pInstanceId_ pRoutingProfileId_ =
  DisassociateRoutingProfileQueues'
    { _drpqInstanceId = pInstanceId_,
      _drpqRoutingProfileId = pRoutingProfileId_,
      _drpqQueueReferences = mempty
    }

-- | The identifier of the Amazon Connect instance.
drpqInstanceId :: Lens' DisassociateRoutingProfileQueues Text
drpqInstanceId = lens _drpqInstanceId (\s a -> s {_drpqInstanceId = a})

-- | The identifier of the routing profile.
drpqRoutingProfileId :: Lens' DisassociateRoutingProfileQueues Text
drpqRoutingProfileId = lens _drpqRoutingProfileId (\s a -> s {_drpqRoutingProfileId = a})

-- | The queues to disassociate from this routing profile.
drpqQueueReferences :: Lens' DisassociateRoutingProfileQueues [RoutingProfileQueueReference]
drpqQueueReferences = lens _drpqQueueReferences (\s a -> s {_drpqQueueReferences = a}) . _Coerce

instance AWSRequest DisassociateRoutingProfileQueues where
  type
    Rs DisassociateRoutingProfileQueues =
      DisassociateRoutingProfileQueuesResponse
  request = postJSON connect
  response = receiveNull DisassociateRoutingProfileQueuesResponse'

instance Hashable DisassociateRoutingProfileQueues

instance NFData DisassociateRoutingProfileQueues

instance ToHeaders DisassociateRoutingProfileQueues where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DisassociateRoutingProfileQueues where
  toJSON DisassociateRoutingProfileQueues' {..} =
    object
      (catMaybes [Just ("QueueReferences" .= _drpqQueueReferences)])

instance ToPath DisassociateRoutingProfileQueues where
  toPath DisassociateRoutingProfileQueues' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _drpqInstanceId,
        "/",
        toBS _drpqRoutingProfileId,
        "/disassociate-queues"
      ]

instance ToQuery DisassociateRoutingProfileQueues where
  toQuery = const mempty

-- | /See:/ 'disassociateRoutingProfileQueuesResponse' smart constructor.
data DisassociateRoutingProfileQueuesResponse = DisassociateRoutingProfileQueuesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateRoutingProfileQueuesResponse' with the minimum fields required to make a request.
disassociateRoutingProfileQueuesResponse ::
  DisassociateRoutingProfileQueuesResponse
disassociateRoutingProfileQueuesResponse =
  DisassociateRoutingProfileQueuesResponse'

instance NFData DisassociateRoutingProfileQueuesResponse
