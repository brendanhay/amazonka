{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerLatencyPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatencyPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Queue setting that determines the highest latency allowed for individual players when placing a game session. When a latency policy is in force, a game session cannot be placed with any fleet in a Region where a player reports latency higher than the cap. Latency policies are only enforced when the placement request contains player latency information.
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
-- /See:/ 'playerLatencyPolicy' smart constructor.
data PlayerLatencyPolicy = PlayerLatencyPolicy'
  { _plpPolicyDurationSeconds ::
      !(Maybe Nat),
    _plpMaximumIndividualPlayerLatencyMilliseconds ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlayerLatencyPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plpPolicyDurationSeconds' - The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
--
-- * 'plpMaximumIndividualPlayerLatencyMilliseconds' - The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
playerLatencyPolicy ::
  PlayerLatencyPolicy
playerLatencyPolicy =
  PlayerLatencyPolicy'
    { _plpPolicyDurationSeconds = Nothing,
      _plpMaximumIndividualPlayerLatencyMilliseconds = Nothing
    }

-- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
plpPolicyDurationSeconds :: Lens' PlayerLatencyPolicy (Maybe Natural)
plpPolicyDurationSeconds = lens _plpPolicyDurationSeconds (\s a -> s {_plpPolicyDurationSeconds = a}) . mapping _Nat

-- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
plpMaximumIndividualPlayerLatencyMilliseconds :: Lens' PlayerLatencyPolicy (Maybe Natural)
plpMaximumIndividualPlayerLatencyMilliseconds = lens _plpMaximumIndividualPlayerLatencyMilliseconds (\s a -> s {_plpMaximumIndividualPlayerLatencyMilliseconds = a}) . mapping _Nat

instance FromJSON PlayerLatencyPolicy where
  parseJSON =
    withObject
      "PlayerLatencyPolicy"
      ( \x ->
          PlayerLatencyPolicy'
            <$> (x .:? "PolicyDurationSeconds")
            <*> (x .:? "MaximumIndividualPlayerLatencyMilliseconds")
      )

instance Hashable PlayerLatencyPolicy

instance NFData PlayerLatencyPolicy

instance ToJSON PlayerLatencyPolicy where
  toJSON PlayerLatencyPolicy' {..} =
    object
      ( catMaybes
          [ ("PolicyDurationSeconds" .=) <$> _plpPolicyDurationSeconds,
            ("MaximumIndividualPlayerLatencyMilliseconds" .=)
              <$> _plpMaximumIndividualPlayerLatencyMilliseconds
          ]
      )
