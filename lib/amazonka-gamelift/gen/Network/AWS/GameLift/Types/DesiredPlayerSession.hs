{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.DesiredPlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.DesiredPlayerSession where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Player information for use when creating player sessions using a game session placement request with 'StartGameSessionPlacement' .
--
--
--
-- /See:/ 'desiredPlayerSession' smart constructor.
data DesiredPlayerSession = DesiredPlayerSession'
  { _dpsPlayerData ::
      !(Maybe Text),
    _dpsPlayerId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DesiredPlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsPlayerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- * 'dpsPlayerId' - A unique identifier for a player to associate with the player session.
desiredPlayerSession ::
  DesiredPlayerSession
desiredPlayerSession =
  DesiredPlayerSession'
    { _dpsPlayerData = Nothing,
      _dpsPlayerId = Nothing
    }

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
dpsPlayerData :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerData = lens _dpsPlayerData (\s a -> s {_dpsPlayerData = a})

-- | A unique identifier for a player to associate with the player session.
dpsPlayerId :: Lens' DesiredPlayerSession (Maybe Text)
dpsPlayerId = lens _dpsPlayerId (\s a -> s {_dpsPlayerId = a})

instance Hashable DesiredPlayerSession

instance NFData DesiredPlayerSession

instance ToJSON DesiredPlayerSession where
  toJSON DesiredPlayerSession' {..} =
    object
      ( catMaybes
          [ ("PlayerData" .=) <$> _dpsPlayerData,
            ("PlayerId" .=) <$> _dpsPlayerId
          ]
      )
