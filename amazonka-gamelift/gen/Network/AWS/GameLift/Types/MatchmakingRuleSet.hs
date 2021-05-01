{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingRuleSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingRuleSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Set of rule statements, used with FlexMatch, that determine how to build
-- your player matches. Each rule set describes a type of group to be
-- created and defines the parameters for acceptable player matches. Rule
-- sets are used in MatchmakingConfiguration objects.
--
-- A rule set may define the following elements for a match. For detailed
-- information and examples showing how to construct a rule set, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a FlexMatch Rule Set>.
--
-- -   Teams -- Required. A rule set must define one or multiple teams for
--     the match and set minimum and maximum team sizes. For example, a
--     rule set might describe a 4x4 match that requires all eight slots to
--     be filled.
--
-- -   Player attributes -- Optional. These attributes specify a set of
--     player characteristics to evaluate when looking for a match.
--     Matchmaking requests that use a rule set with player attributes must
--     provide the corresponding attribute values. For example, an
--     attribute might specify a player\'s skill or level.
--
-- -   Rules -- Optional. Rules define how to evaluate potential players
--     for a match based on player attributes. A rule might specify minimum
--     requirements for individual players, teams, or entire matches. For
--     example, a rule might require each player to meet a certain skill
--     level, each team to have at least one player in a certain role, or
--     the match to have a minimum average skill level. or may describe an
--     entire group--such as all teams must be evenly matched or have at
--     least one player in a certain role.
--
-- -   Expansions -- Optional. Expansions allow you to relax the rules
--     after a period of time when no acceptable matches are found. This
--     feature lets you balance getting players into games in a reasonable
--     amount of time instead of making them wait indefinitely for the best
--     possible match. For example, you might use an expansion to increase
--     the maximum skill variance between players after 30 seconds.
--
-- /See:/ 'newMatchmakingRuleSet' smart constructor.
data MatchmakingRuleSet = MatchmakingRuleSet'
  { -- | The time stamp indicating when this data object was created. The format
    -- is a number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift matchmaking rule set resource and
    -- uniquely identifies it. ARNs are unique across all Regions. In a
    -- GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
    ruleSetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a matchmaking rule set
    ruleSetName :: Prelude.Maybe Prelude.Text,
    -- | A collection of matchmaking rules, formatted as a JSON string. Comments
    -- are not allowed in JSON, but most elements support a description field.
    ruleSetBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MatchmakingRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'matchmakingRuleSet_creationTime' - The time stamp indicating when this data object was created. The format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'ruleSetArn', 'matchmakingRuleSet_ruleSetArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift matchmaking rule set resource and
-- uniquely identifies it. ARNs are unique across all Regions. In a
-- GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
--
-- 'ruleSetName', 'matchmakingRuleSet_ruleSetName' - A unique identifier for a matchmaking rule set
--
-- 'ruleSetBody', 'matchmakingRuleSet_ruleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments
-- are not allowed in JSON, but most elements support a description field.
newMatchmakingRuleSet ::
  -- | 'ruleSetBody'
  Prelude.Text ->
  MatchmakingRuleSet
newMatchmakingRuleSet pRuleSetBody_ =
  MatchmakingRuleSet'
    { creationTime = Prelude.Nothing,
      ruleSetArn = Prelude.Nothing,
      ruleSetName = Prelude.Nothing,
      ruleSetBody = pRuleSetBody_
    }

-- | The time stamp indicating when this data object was created. The format
-- is a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
matchmakingRuleSet_creationTime :: Lens.Lens' MatchmakingRuleSet (Prelude.Maybe Prelude.UTCTime)
matchmakingRuleSet_creationTime = Lens.lens (\MatchmakingRuleSet' {creationTime} -> creationTime) (\s@MatchmakingRuleSet' {} a -> s {creationTime = a} :: MatchmakingRuleSet) Prelude.. Lens.mapping Prelude._Time

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift matchmaking rule set resource and
-- uniquely identifies it. ARNs are unique across all Regions. In a
-- GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
matchmakingRuleSet_ruleSetArn :: Lens.Lens' MatchmakingRuleSet (Prelude.Maybe Prelude.Text)
matchmakingRuleSet_ruleSetArn = Lens.lens (\MatchmakingRuleSet' {ruleSetArn} -> ruleSetArn) (\s@MatchmakingRuleSet' {} a -> s {ruleSetArn = a} :: MatchmakingRuleSet)

-- | A unique identifier for a matchmaking rule set
matchmakingRuleSet_ruleSetName :: Lens.Lens' MatchmakingRuleSet (Prelude.Maybe Prelude.Text)
matchmakingRuleSet_ruleSetName = Lens.lens (\MatchmakingRuleSet' {ruleSetName} -> ruleSetName) (\s@MatchmakingRuleSet' {} a -> s {ruleSetName = a} :: MatchmakingRuleSet)

-- | A collection of matchmaking rules, formatted as a JSON string. Comments
-- are not allowed in JSON, but most elements support a description field.
matchmakingRuleSet_ruleSetBody :: Lens.Lens' MatchmakingRuleSet Prelude.Text
matchmakingRuleSet_ruleSetBody = Lens.lens (\MatchmakingRuleSet' {ruleSetBody} -> ruleSetBody) (\s@MatchmakingRuleSet' {} a -> s {ruleSetBody = a} :: MatchmakingRuleSet)

instance Prelude.FromJSON MatchmakingRuleSet where
  parseJSON =
    Prelude.withObject
      "MatchmakingRuleSet"
      ( \x ->
          MatchmakingRuleSet'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "RuleSetArn")
            Prelude.<*> (x Prelude..:? "RuleSetName")
            Prelude.<*> (x Prelude..: "RuleSetBody")
      )

instance Prelude.Hashable MatchmakingRuleSet

instance Prelude.NFData MatchmakingRuleSet
