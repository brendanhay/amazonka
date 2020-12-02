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
-- Module      : Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default outbound queue of a routing profile.
module Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
  ( -- * Creating a Request
    updateRoutingProfileDefaultOutboundQueue,
    UpdateRoutingProfileDefaultOutboundQueue,

    -- * Request Lenses
    urpdoqInstanceId,
    urpdoqRoutingProfileId,
    urpdoqDefaultOutboundQueueId,

    -- * Destructuring the Response
    updateRoutingProfileDefaultOutboundQueueResponse,
    UpdateRoutingProfileDefaultOutboundQueueResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoutingProfileDefaultOutboundQueue' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueue = UpdateRoutingProfileDefaultOutboundQueue'
  { _urpdoqInstanceId ::
      !Text,
    _urpdoqRoutingProfileId ::
      !Text,
    _urpdoqDefaultOutboundQueueId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateRoutingProfileDefaultOutboundQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpdoqInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'urpdoqRoutingProfileId' - The identifier of the routing profile.
--
-- * 'urpdoqDefaultOutboundQueueId' - The identifier for the default outbound queue.
updateRoutingProfileDefaultOutboundQueue ::
  -- | 'urpdoqInstanceId'
  Text ->
  -- | 'urpdoqRoutingProfileId'
  Text ->
  -- | 'urpdoqDefaultOutboundQueueId'
  Text ->
  UpdateRoutingProfileDefaultOutboundQueue
updateRoutingProfileDefaultOutboundQueue
  pInstanceId_
  pRoutingProfileId_
  pDefaultOutboundQueueId_ =
    UpdateRoutingProfileDefaultOutboundQueue'
      { _urpdoqInstanceId =
          pInstanceId_,
        _urpdoqRoutingProfileId = pRoutingProfileId_,
        _urpdoqDefaultOutboundQueueId =
          pDefaultOutboundQueueId_
      }

-- | The identifier of the Amazon Connect instance.
urpdoqInstanceId :: Lens' UpdateRoutingProfileDefaultOutboundQueue Text
urpdoqInstanceId = lens _urpdoqInstanceId (\s a -> s {_urpdoqInstanceId = a})

-- | The identifier of the routing profile.
urpdoqRoutingProfileId :: Lens' UpdateRoutingProfileDefaultOutboundQueue Text
urpdoqRoutingProfileId = lens _urpdoqRoutingProfileId (\s a -> s {_urpdoqRoutingProfileId = a})

-- | The identifier for the default outbound queue.
urpdoqDefaultOutboundQueueId :: Lens' UpdateRoutingProfileDefaultOutboundQueue Text
urpdoqDefaultOutboundQueueId = lens _urpdoqDefaultOutboundQueueId (\s a -> s {_urpdoqDefaultOutboundQueueId = a})

instance AWSRequest UpdateRoutingProfileDefaultOutboundQueue where
  type
    Rs UpdateRoutingProfileDefaultOutboundQueue =
      UpdateRoutingProfileDefaultOutboundQueueResponse
  request = postJSON connect
  response =
    receiveNull UpdateRoutingProfileDefaultOutboundQueueResponse'

instance Hashable UpdateRoutingProfileDefaultOutboundQueue

instance NFData UpdateRoutingProfileDefaultOutboundQueue

instance ToHeaders UpdateRoutingProfileDefaultOutboundQueue where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateRoutingProfileDefaultOutboundQueue where
  toJSON UpdateRoutingProfileDefaultOutboundQueue' {..} =
    object
      ( catMaybes
          [Just ("DefaultOutboundQueueId" .= _urpdoqDefaultOutboundQueueId)]
      )

instance ToPath UpdateRoutingProfileDefaultOutboundQueue where
  toPath UpdateRoutingProfileDefaultOutboundQueue' {..} =
    mconcat
      [ "/routing-profiles/",
        toBS _urpdoqInstanceId,
        "/",
        toBS _urpdoqRoutingProfileId,
        "/default-outbound-queue"
      ]

instance ToQuery UpdateRoutingProfileDefaultOutboundQueue where
  toQuery = const mempty

-- | /See:/ 'updateRoutingProfileDefaultOutboundQueueResponse' smart constructor.
data UpdateRoutingProfileDefaultOutboundQueueResponse = UpdateRoutingProfileDefaultOutboundQueueResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateRoutingProfileDefaultOutboundQueueResponse' with the minimum fields required to make a request.
updateRoutingProfileDefaultOutboundQueueResponse ::
  UpdateRoutingProfileDefaultOutboundQueueResponse
updateRoutingProfileDefaultOutboundQueueResponse =
  UpdateRoutingProfileDefaultOutboundQueueResponse'

instance NFData UpdateRoutingProfileDefaultOutboundQueueResponse
