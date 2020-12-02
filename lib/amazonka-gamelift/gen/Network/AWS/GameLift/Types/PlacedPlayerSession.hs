{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlacedPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlacedPlayerSession where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a player session that was created as part of a 'StartGameSessionPlacement' request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call 'DescribePlayerSessions' with the player session ID.
--
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--     * 'DescribeGameSessionPlacement'
--
--     * 'StopGameSessionPlacement'
--
--
--
--
--
--
-- /See:/ 'placedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { _ppsPlayerSessionId ::
      !(Maybe Text),
    _ppsPlayerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacedPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppsPlayerSessionId' - A unique identifier for a player session.
--
-- * 'ppsPlayerId' - A unique identifier for a player that is associated with this player session.
placedPlayerSession ::
  PlacedPlayerSession
placedPlayerSession =
  PlacedPlayerSession'
    { _ppsPlayerSessionId = Nothing,
      _ppsPlayerId = Nothing
    }

-- | A unique identifier for a player session.
ppsPlayerSessionId :: Lens' PlacedPlayerSession (Maybe Text)
ppsPlayerSessionId = lens _ppsPlayerSessionId (\s a -> s {_ppsPlayerSessionId = a})

-- | A unique identifier for a player that is associated with this player session.
ppsPlayerId :: Lens' PlacedPlayerSession (Maybe Text)
ppsPlayerId = lens _ppsPlayerId (\s a -> s {_ppsPlayerId = a})

instance FromJSON PlacedPlayerSession where
  parseJSON =
    withObject
      "PlacedPlayerSession"
      ( \x ->
          PlacedPlayerSession'
            <$> (x .:? "PlayerSessionId") <*> (x .:? "PlayerId")
      )

instance Hashable PlacedPlayerSession

instance NFData PlacedPlayerSession
