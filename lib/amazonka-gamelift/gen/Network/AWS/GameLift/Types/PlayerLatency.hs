{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerLatency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatency where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Regional latency information for a player, used when requesting a new game session with 'StartGameSessionPlacement' . This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified Region. The relative difference between a player's latency values for multiple Regions are used to determine which fleets are best suited to place a new game session for the player.
--
--
--
-- /See:/ 'playerLatency' smart constructor.
data PlayerLatency = PlayerLatency'
  { _plLatencyInMilliseconds ::
      !(Maybe Double),
    _plRegionIdentifier :: !(Maybe Text),
    _plPlayerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlayerLatency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plLatencyInMilliseconds' - Amount of time that represents the time lag experienced by the player when connected to the specified Region.
--
-- * 'plRegionIdentifier' - Name of the Region that is associated with the latency value.
--
-- * 'plPlayerId' - A unique identifier for a player associated with the latency data.
playerLatency ::
  PlayerLatency
playerLatency =
  PlayerLatency'
    { _plLatencyInMilliseconds = Nothing,
      _plRegionIdentifier = Nothing,
      _plPlayerId = Nothing
    }

-- | Amount of time that represents the time lag experienced by the player when connected to the specified Region.
plLatencyInMilliseconds :: Lens' PlayerLatency (Maybe Double)
plLatencyInMilliseconds = lens _plLatencyInMilliseconds (\s a -> s {_plLatencyInMilliseconds = a})

-- | Name of the Region that is associated with the latency value.
plRegionIdentifier :: Lens' PlayerLatency (Maybe Text)
plRegionIdentifier = lens _plRegionIdentifier (\s a -> s {_plRegionIdentifier = a})

-- | A unique identifier for a player associated with the latency data.
plPlayerId :: Lens' PlayerLatency (Maybe Text)
plPlayerId = lens _plPlayerId (\s a -> s {_plPlayerId = a})

instance FromJSON PlayerLatency where
  parseJSON =
    withObject
      "PlayerLatency"
      ( \x ->
          PlayerLatency'
            <$> (x .:? "LatencyInMilliseconds")
            <*> (x .:? "RegionIdentifier")
            <*> (x .:? "PlayerId")
      )

instance Hashable PlayerLatency

instance NFData PlayerLatency

instance ToJSON PlayerLatency where
  toJSON PlayerLatency' {..} =
    object
      ( catMaybes
          [ ("LatencyInMilliseconds" .=) <$> _plLatencyInMilliseconds,
            ("RegionIdentifier" .=) <$> _plRegionIdentifier,
            ("PlayerId" .=) <$> _plPlayerId
          ]
      )
