{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchedPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchedPlayerSession where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request.
--
--
-- When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.
--
--
-- /See:/ 'matchedPlayerSession' smart constructor.
data MatchedPlayerSession = MatchedPlayerSession'
  { _mpsPlayerSessionId ::
      !(Maybe Text),
    _mpsPlayerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MatchedPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpsPlayerSessionId' - A unique identifier for a player session
--
-- * 'mpsPlayerId' - A unique identifier for a player
matchedPlayerSession ::
  MatchedPlayerSession
matchedPlayerSession =
  MatchedPlayerSession'
    { _mpsPlayerSessionId = Nothing,
      _mpsPlayerId = Nothing
    }

-- | A unique identifier for a player session
mpsPlayerSessionId :: Lens' MatchedPlayerSession (Maybe Text)
mpsPlayerSessionId = lens _mpsPlayerSessionId (\s a -> s {_mpsPlayerSessionId = a})

-- | A unique identifier for a player
mpsPlayerId :: Lens' MatchedPlayerSession (Maybe Text)
mpsPlayerId = lens _mpsPlayerId (\s a -> s {_mpsPlayerId = a})

instance FromJSON MatchedPlayerSession where
  parseJSON =
    withObject
      "MatchedPlayerSession"
      ( \x ->
          MatchedPlayerSession'
            <$> (x .:? "PlayerSessionId") <*> (x .:? "PlayerId")
      )

instance Hashable MatchedPlayerSession

instance NFData MatchedPlayerSession
