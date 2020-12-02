{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingRuleSet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Set of rule statements, used with FlexMatch, that determine how to build your player matches. Each rule set describes a type of group to be created and defines the parameters for acceptable player matches. Rule sets are used in 'MatchmakingConfiguration' objects.
--
--
-- A rule set may define the following elements for a match. For detailed information and examples showing how to construct a rule set, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a FlexMatch Rule Set> .
--
--     * Teams -- Required. A rule set must define one or multiple teams for the match and set minimum and maximum team sizes. For example, a rule set might describe a 4x4 match that requires all eight slots to be filled.
--
--     * Player attributes -- Optional. These attributes specify a set of player characteristics to evaluate when looking for a match. Matchmaking requests that use a rule set with player attributes must provide the corresponding attribute values. For example, an attribute might specify a player's skill or level.
--
--     * Rules -- Optional. Rules define how to evaluate potential players for a match based on player attributes. A rule might specify minimum requirements for individual players, teams, or entire matches. For example, a rule might require each player to meet a certain skill level, each team to have at least one player in a certain role, or the match to have a minimum average skill level. or may describe an entire group--such as all teams must be evenly matched or have at least one player in a certain role.
--
--     * Expansions -- Optional. Expansions allow you to relax the rules after a period of time when no acceptable matches are found. This feature lets you balance getting players into games in a reasonable amount of time instead of making them wait indefinitely for the best possible match. For example, you might use an expansion to increase the maximum skill variance between players after 30 seconds.
--
--
--
--
-- /See:/ 'matchmakingRuleSet' smart constructor.
data MatchmakingRuleSet = MatchmakingRuleSet'
  { _mrsCreationTime ::
      !(Maybe POSIX),
    _mrsRuleSetName :: !(Maybe Text),
    _mrsRuleSetARN :: !(Maybe Text),
    _mrsRuleSetBody :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MatchmakingRuleSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrsCreationTime' - The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mrsRuleSetName' - A unique identifier for a matchmaking rule set
--
-- * 'mrsRuleSetARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking rule set resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
--
-- * 'mrsRuleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
matchmakingRuleSet ::
  -- | 'mrsRuleSetBody'
  Text ->
  MatchmakingRuleSet
matchmakingRuleSet pRuleSetBody_ =
  MatchmakingRuleSet'
    { _mrsCreationTime = Nothing,
      _mrsRuleSetName = Nothing,
      _mrsRuleSetARN = Nothing,
      _mrsRuleSetBody = pRuleSetBody_
    }

-- | The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mrsCreationTime :: Lens' MatchmakingRuleSet (Maybe UTCTime)
mrsCreationTime = lens _mrsCreationTime (\s a -> s {_mrsCreationTime = a}) . mapping _Time

-- | A unique identifier for a matchmaking rule set
mrsRuleSetName :: Lens' MatchmakingRuleSet (Maybe Text)
mrsRuleSetName = lens _mrsRuleSetName (\s a -> s {_mrsRuleSetName = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking rule set resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
mrsRuleSetARN :: Lens' MatchmakingRuleSet (Maybe Text)
mrsRuleSetARN = lens _mrsRuleSetARN (\s a -> s {_mrsRuleSetARN = a})

-- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
mrsRuleSetBody :: Lens' MatchmakingRuleSet Text
mrsRuleSetBody = lens _mrsRuleSetBody (\s a -> s {_mrsRuleSetBody = a})

instance FromJSON MatchmakingRuleSet where
  parseJSON =
    withObject
      "MatchmakingRuleSet"
      ( \x ->
          MatchmakingRuleSet'
            <$> (x .:? "CreationTime")
            <*> (x .:? "RuleSetName")
            <*> (x .:? "RuleSetArn")
            <*> (x .: "RuleSetBody")
      )

instance Hashable MatchmakingRuleSet

instance NFData MatchmakingRuleSet
