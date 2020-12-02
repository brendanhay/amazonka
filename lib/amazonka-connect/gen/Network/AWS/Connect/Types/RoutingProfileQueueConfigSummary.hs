{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfileQueueConfigSummary where

import Network.AWS.Connect.Types.Channel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a routing profile queue.
--
--
--
-- /See:/ 'routingProfileQueueConfigSummary' smart constructor.
data RoutingProfileQueueConfigSummary = RoutingProfileQueueConfigSummary'
  { _rpqcsQueueId ::
      !Text,
    _rpqcsQueueARN :: !Text,
    _rpqcsQueueName :: !Text,
    _rpqcsPriority :: !Nat,
    _rpqcsDelay :: !Nat,
    _rpqcsChannel :: !Channel
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingProfileQueueConfigSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpqcsQueueId' - The identifier of the queue.
--
-- * 'rpqcsQueueARN' - The Amazon Resource Name (ARN) of the queue.
--
-- * 'rpqcsQueueName' - The name of the queue.
--
-- * 'rpqcsPriority' - The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
--
-- * 'rpqcsDelay' - The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
--
-- * 'rpqcsChannel' - The channels this queue supports.
routingProfileQueueConfigSummary ::
  -- | 'rpqcsQueueId'
  Text ->
  -- | 'rpqcsQueueARN'
  Text ->
  -- | 'rpqcsQueueName'
  Text ->
  -- | 'rpqcsPriority'
  Natural ->
  -- | 'rpqcsDelay'
  Natural ->
  -- | 'rpqcsChannel'
  Channel ->
  RoutingProfileQueueConfigSummary
routingProfileQueueConfigSummary
  pQueueId_
  pQueueARN_
  pQueueName_
  pPriority_
  pDelay_
  pChannel_ =
    RoutingProfileQueueConfigSummary'
      { _rpqcsQueueId = pQueueId_,
        _rpqcsQueueARN = pQueueARN_,
        _rpqcsQueueName = pQueueName_,
        _rpqcsPriority = _Nat # pPriority_,
        _rpqcsDelay = _Nat # pDelay_,
        _rpqcsChannel = pChannel_
      }

-- | The identifier of the queue.
rpqcsQueueId :: Lens' RoutingProfileQueueConfigSummary Text
rpqcsQueueId = lens _rpqcsQueueId (\s a -> s {_rpqcsQueueId = a})

-- | The Amazon Resource Name (ARN) of the queue.
rpqcsQueueARN :: Lens' RoutingProfileQueueConfigSummary Text
rpqcsQueueARN = lens _rpqcsQueueARN (\s a -> s {_rpqcsQueueARN = a})

-- | The name of the queue.
rpqcsQueueName :: Lens' RoutingProfileQueueConfigSummary Text
rpqcsQueueName = lens _rpqcsQueueName (\s a -> s {_rpqcsQueueName = a})

-- | The order in which contacts are to be handled for the queue. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> .
rpqcsPriority :: Lens' RoutingProfileQueueConfigSummary Natural
rpqcsPriority = lens _rpqcsPriority (\s a -> s {_rpqcsPriority = a}) . _Nat

-- | The delay, in seconds, that a contact should be in the queue before they are routed to an available agent. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing-profiles-priority.html Queues: priority and delay> in the /Amazon Connect Administrator Guide/ .
rpqcsDelay :: Lens' RoutingProfileQueueConfigSummary Natural
rpqcsDelay = lens _rpqcsDelay (\s a -> s {_rpqcsDelay = a}) . _Nat

-- | The channels this queue supports.
rpqcsChannel :: Lens' RoutingProfileQueueConfigSummary Channel
rpqcsChannel = lens _rpqcsChannel (\s a -> s {_rpqcsChannel = a})

instance FromJSON RoutingProfileQueueConfigSummary where
  parseJSON =
    withObject
      "RoutingProfileQueueConfigSummary"
      ( \x ->
          RoutingProfileQueueConfigSummary'
            <$> (x .: "QueueId")
            <*> (x .: "QueueArn")
            <*> (x .: "QueueName")
            <*> (x .: "Priority")
            <*> (x .: "Delay")
            <*> (x .: "Channel")
      )

instance Hashable RoutingProfileQueueConfigSummary

instance NFData RoutingProfileQueueConfigSummary
