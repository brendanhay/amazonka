{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueReference where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the channel and queue identifier for a routing profile.
--
--
--
-- /See:/ 'routingProfileQueueReference' smart constructor.
data RoutingProfileQueueReference = RoutingProfileQueueReference'
  { _rpqrQueueId ::
      !Text,
    _rpqrChannel :: !Channel
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingProfileQueueReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpqrQueueId' - The identifier of the queue.
--
-- * 'rpqrChannel' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
routingProfileQueueReference ::
  -- | 'rpqrQueueId'
  Text ->
  -- | 'rpqrChannel'
  Channel ->
  RoutingProfileQueueReference
routingProfileQueueReference pQueueId_ pChannel_ =
  RoutingProfileQueueReference'
    { _rpqrQueueId = pQueueId_,
      _rpqrChannel = pChannel_
    }

-- | The identifier of the queue.
rpqrQueueId :: Lens' RoutingProfileQueueReference Text
rpqrQueueId = lens _rpqrQueueId (\s a -> s {_rpqrQueueId = a})

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
rpqrChannel :: Lens' RoutingProfileQueueReference Channel
rpqrChannel = lens _rpqrChannel (\s a -> s {_rpqrChannel = a})

instance Hashable RoutingProfileQueueReference

instance NFData RoutingProfileQueueReference

instance ToJSON RoutingProfileQueueReference where
  toJSON RoutingProfileQueueReference' {..} =
    object
      ( catMaybes
          [ Just ("QueueId" .= _rpqrQueueId),
            Just ("Channel" .= _rpqrChannel)
          ]
      )
