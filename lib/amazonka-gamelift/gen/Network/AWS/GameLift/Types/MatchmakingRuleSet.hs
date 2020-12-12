{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingRuleSet
  ( MatchmakingRuleSet (..),

    -- * Smart constructor
    mkMatchmakingRuleSet,

    -- * Lenses
    mrsCreationTime,
    mrsRuleSetName,
    mrsRuleSetARN,
    mrsRuleSetBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Set of rule statements, used with FlexMatch, that determine how to build your player matches. Each rule set describes a type of group to be created and defines the parameters for acceptable player matches. Rule sets are used in 'MatchmakingConfiguration' objects.
--
-- A rule set may define the following elements for a match. For detailed information and examples showing how to construct a rule set, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a FlexMatch Rule Set> .
--
--     * Teams -- Required. A rule set must define one or multiple teams for the match and set minimum and maximum team sizes. For example, a rule set might describe a 4x4 match that requires all eight slots to be filled.
--
--
--     * Player attributes -- Optional. These attributes specify a set of player characteristics to evaluate when looking for a match. Matchmaking requests that use a rule set with player attributes must provide the corresponding attribute values. For example, an attribute might specify a player's skill or level.
--
--
--     * Rules -- Optional. Rules define how to evaluate potential players for a match based on player attributes. A rule might specify minimum requirements for individual players, teams, or entire matches. For example, a rule might require each player to meet a certain skill level, each team to have at least one player in a certain role, or the match to have a minimum average skill level. or may describe an entire group--such as all teams must be evenly matched or have at least one player in a certain role.
--
--
--     * Expansions -- Optional. Expansions allow you to relax the rules after a period of time when no acceptable matches are found. This feature lets you balance getting players into games in a reasonable amount of time instead of making them wait indefinitely for the best possible match. For example, you might use an expansion to increase the maximum skill variance between players after 30 seconds.
--
--
--
-- /See:/ 'mkMatchmakingRuleSet' smart constructor.
data MatchmakingRuleSet = MatchmakingRuleSet'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    ruleSetName :: Lude.Maybe Lude.Text,
    ruleSetARN :: Lude.Maybe Lude.Text,
    ruleSetBody :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MatchmakingRuleSet' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'ruleSetARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking rule set resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
-- * 'ruleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
-- * 'ruleSetName' - A unique identifier for a matchmaking rule set
mkMatchmakingRuleSet ::
  -- | 'ruleSetBody'
  Lude.Text ->
  MatchmakingRuleSet
mkMatchmakingRuleSet pRuleSetBody_ =
  MatchmakingRuleSet'
    { creationTime = Lude.Nothing,
      ruleSetName = Lude.Nothing,
      ruleSetARN = Lude.Nothing,
      ruleSetBody = pRuleSetBody_
    }

-- | The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsCreationTime :: Lens.Lens' MatchmakingRuleSet (Lude.Maybe Lude.Timestamp)
mrsCreationTime = Lens.lens (creationTime :: MatchmakingRuleSet -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: MatchmakingRuleSet)
{-# DEPRECATED mrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A unique identifier for a matchmaking rule set
--
-- /Note:/ Consider using 'ruleSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsRuleSetName :: Lens.Lens' MatchmakingRuleSet (Lude.Maybe Lude.Text)
mrsRuleSetName = Lens.lens (ruleSetName :: MatchmakingRuleSet -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetName = a} :: MatchmakingRuleSet)
{-# DEPRECATED mrsRuleSetName "Use generic-lens or generic-optics with 'ruleSetName' instead." #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking rule set resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift rule set ARN, the resource ID matches the /RuleSetName/ value.
--
-- /Note:/ Consider using 'ruleSetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsRuleSetARN :: Lens.Lens' MatchmakingRuleSet (Lude.Maybe Lude.Text)
mrsRuleSetARN = Lens.lens (ruleSetARN :: MatchmakingRuleSet -> Lude.Maybe Lude.Text) (\s a -> s {ruleSetARN = a} :: MatchmakingRuleSet)
{-# DEPRECATED mrsRuleSetARN "Use generic-lens or generic-optics with 'ruleSetARN' instead." #-}

-- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
--
-- /Note:/ Consider using 'ruleSetBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsRuleSetBody :: Lens.Lens' MatchmakingRuleSet Lude.Text
mrsRuleSetBody = Lens.lens (ruleSetBody :: MatchmakingRuleSet -> Lude.Text) (\s a -> s {ruleSetBody = a} :: MatchmakingRuleSet)
{-# DEPRECATED mrsRuleSetBody "Use generic-lens or generic-optics with 'ruleSetBody' instead." #-}

instance Lude.FromJSON MatchmakingRuleSet where
  parseJSON =
    Lude.withObject
      "MatchmakingRuleSet"
      ( \x ->
          MatchmakingRuleSet'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "RuleSetName")
            Lude.<*> (x Lude..:? "RuleSetArn")
            Lude.<*> (x Lude..: "RuleSetBody")
      )
