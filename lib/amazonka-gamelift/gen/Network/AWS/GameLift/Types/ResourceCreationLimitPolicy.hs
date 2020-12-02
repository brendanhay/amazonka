{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ResourceCreationLimitPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".
--
--
-- The policy is evaluated when a player tries to create a new game session. For example: Assume you have a policy of 10 new game sessions and a time period of 60 minutes. On receiving a @CreateGameSession@ request, Amazon GameLift checks that the player (identified by @CreatorId@ ) has created fewer than 10 game sessions in the past 60 minutes.
--
--
-- /See:/ 'resourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { _rclpNewGameSessionsPerCreator ::
      !(Maybe Nat),
    _rclpPolicyPeriodInMinutes ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceCreationLimitPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rclpNewGameSessionsPerCreator' - The maximum number of game sessions that an individual can create during the policy period.
--
-- * 'rclpPolicyPeriodInMinutes' - The time span used in evaluating the resource creation limit policy.
resourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
resourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { _rclpNewGameSessionsPerCreator =
        Nothing,
      _rclpPolicyPeriodInMinutes = Nothing
    }

-- | The maximum number of game sessions that an individual can create during the policy period.
rclpNewGameSessionsPerCreator :: Lens' ResourceCreationLimitPolicy (Maybe Natural)
rclpNewGameSessionsPerCreator = lens _rclpNewGameSessionsPerCreator (\s a -> s {_rclpNewGameSessionsPerCreator = a}) . mapping _Nat

-- | The time span used in evaluating the resource creation limit policy.
rclpPolicyPeriodInMinutes :: Lens' ResourceCreationLimitPolicy (Maybe Natural)
rclpPolicyPeriodInMinutes = lens _rclpPolicyPeriodInMinutes (\s a -> s {_rclpPolicyPeriodInMinutes = a}) . mapping _Nat

instance FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    withObject
      "ResourceCreationLimitPolicy"
      ( \x ->
          ResourceCreationLimitPolicy'
            <$> (x .:? "NewGameSessionsPerCreator")
            <*> (x .:? "PolicyPeriodInMinutes")
      )

instance Hashable ResourceCreationLimitPolicy

instance NFData ResourceCreationLimitPolicy

instance ToJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy' {..} =
    object
      ( catMaybes
          [ ("NewGameSessionsPerCreator" .=)
              <$> _rclpNewGameSessionsPerCreator,
            ("PolicyPeriodInMinutes" .=) <$> _rclpPolicyPeriodInMinutes
          ]
      )
