{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionDetail where

import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.ProtectionPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A game session's properties plus the protection policy currently in force.
--
--
--
-- /See:/ 'gameSessionDetail' smart constructor.
data GameSessionDetail = GameSessionDetail'
  { _gsdGameSession ::
      !(Maybe GameSession),
    _gsdProtectionPolicy :: !(Maybe ProtectionPolicy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameSessionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsdGameSession' - Object that describes a game session.
--
-- * 'gsdProtectionPolicy' - Current status of protection for the game session.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gameSessionDetail ::
  GameSessionDetail
gameSessionDetail =
  GameSessionDetail'
    { _gsdGameSession = Nothing,
      _gsdProtectionPolicy = Nothing
    }

-- | Object that describes a game session.
gsdGameSession :: Lens' GameSessionDetail (Maybe GameSession)
gsdGameSession = lens _gsdGameSession (\s a -> s {_gsdGameSession = a})

-- | Current status of protection for the game session.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
gsdProtectionPolicy :: Lens' GameSessionDetail (Maybe ProtectionPolicy)
gsdProtectionPolicy = lens _gsdProtectionPolicy (\s a -> s {_gsdProtectionPolicy = a})

instance FromJSON GameSessionDetail where
  parseJSON =
    withObject
      "GameSessionDetail"
      ( \x ->
          GameSessionDetail'
            <$> (x .:? "GameSession") <*> (x .:? "ProtectionPolicy")
      )

instance Hashable GameSessionDetail

instance NFData GameSessionDetail
