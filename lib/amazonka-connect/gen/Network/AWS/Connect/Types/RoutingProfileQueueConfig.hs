{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueConfig where

import Network.AWS.Connect.Types.RoutingProfileQueueReference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the queue and channel for which priority and delay can be set.
--
--
--
-- /See:/ 'routingProfileQueueConfig' smart constructor.
data RoutingProfileQueueConfig = RoutingProfileQueueConfig'
  { _rpqcQueueReference ::
      !RoutingProfileQueueReference,
    _rpqcPriority :: !Nat,
    _rpqcDelay :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingProfileQueueConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpqcQueueReference' - Contains information about a queue resource.
--
-- * 'rpqcPriority' - The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- * 'rpqcDelay' - The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
routingProfileQueueConfig ::
  -- | 'rpqcQueueReference'
  RoutingProfileQueueReference ->
  -- | 'rpqcPriority'
  Natural ->
  -- | 'rpqcDelay'
  Natural ->
  RoutingProfileQueueConfig
routingProfileQueueConfig pQueueReference_ pPriority_ pDelay_ =
  RoutingProfileQueueConfig'
    { _rpqcQueueReference =
        pQueueReference_,
      _rpqcPriority = _Nat # pPriority_,
      _rpqcDelay = _Nat # pDelay_
    }

-- | Contains information about a queue resource.
rpqcQueueReference :: Lens' RoutingProfileQueueConfig RoutingProfileQueueReference
rpqcQueueReference = lens _rpqcQueueReference (\s a -> s {_rpqcQueueReference = a})

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
rpqcPriority :: Lens' RoutingProfileQueueConfig Natural
rpqcPriority = lens _rpqcPriority (\s a -> s {_rpqcPriority = a}) . _Nat

-- | The delay, in seconds, a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
rpqcDelay :: Lens' RoutingProfileQueueConfig Natural
rpqcDelay = lens _rpqcDelay (\s a -> s {_rpqcDelay = a}) . _Nat

instance Hashable RoutingProfileQueueConfig

instance NFData RoutingProfileQueueConfig

instance ToJSON RoutingProfileQueueConfig where
  toJSON RoutingProfileQueueConfig' {..} =
    object
      ( catMaybes
          [ Just ("QueueReference" .= _rpqcQueueReference),
            Just ("Priority" .= _rpqcPriority),
            Just ("Delay" .= _rpqcDelay)
          ]
      )
