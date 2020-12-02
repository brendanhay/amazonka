{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueueDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueueDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination that is configured for a queue.
--
--
--     * 'CreateGameSessionQueue'
--
--     * 'DescribeGameSessionQueues'
--
--     * 'UpdateGameSessionQueue'
--
--     * 'DeleteGameSessionQueue'
--
--
--
--
-- /See:/ 'gameSessionQueueDestination' smart constructor.
newtype GameSessionQueueDestination = GameSessionQueueDestination'
  { _gsqdDestinationARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameSessionQueueDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsqdDestinationARN' - The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions.
gameSessionQueueDestination ::
  GameSessionQueueDestination
gameSessionQueueDestination =
  GameSessionQueueDestination' {_gsqdDestinationARN = Nothing}

-- | The Amazon Resource Name (ARN) that is assigned to fleet or fleet alias. ARNs, which include a fleet ID or alias ID and a Region name, provide a unique identifier across all Regions.
gsqdDestinationARN :: Lens' GameSessionQueueDestination (Maybe Text)
gsqdDestinationARN = lens _gsqdDestinationARN (\s a -> s {_gsqdDestinationARN = a})

instance FromJSON GameSessionQueueDestination where
  parseJSON =
    withObject
      "GameSessionQueueDestination"
      (\x -> GameSessionQueueDestination' <$> (x .:? "DestinationArn"))

instance Hashable GameSessionQueueDestination

instance NFData GameSessionQueueDestination

instance ToJSON GameSessionQueueDestination where
  toJSON GameSessionQueueDestination' {..} =
    object
      (catMaybes [("DestinationArn" .=) <$> _gsqdDestinationARN])
