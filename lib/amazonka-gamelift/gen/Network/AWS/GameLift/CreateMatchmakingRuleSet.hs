{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new rule set for FlexMatch matchmaking. A rule set describes the type of match to create, such as the number and size of teams. It also sets the parameters for acceptable player matches, such as minimum skill level or character type. A rule set is used by a 'MatchmakingConfiguration' .
--
-- To create a matchmaking rule set, provide unique rule set name and the rule set body in JSON format. Rule sets must be defined in the same Region as the matchmaking configuration they are used with.
-- Since matchmaking rule sets cannot be edited, it is a good idea to check the rule set syntax using 'ValidateMatchmakingRuleSet' before creating a new rule set.
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
--
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a Matchmaker>
--
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-intro.html Matchmaking with FlexMatch>
--
--
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--
--     * 'DescribeMatchmakingConfigurations'
--
--
--     * 'UpdateMatchmakingConfiguration'
--
--
--     * 'DeleteMatchmakingConfiguration'
--
--
--     * 'CreateMatchmakingRuleSet'
--
--
--     * 'DescribeMatchmakingRuleSets'
--
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.CreateMatchmakingRuleSet
  ( -- * Creating a request
    CreateMatchmakingRuleSet (..),
    mkCreateMatchmakingRuleSet,

    -- ** Request lenses
    cmrsName,
    cmrsRuleSetBody,
    cmrsTags,

    -- * Destructuring the response
    CreateMatchmakingRuleSetResponse (..),
    mkCreateMatchmakingRuleSetResponse,

    -- ** Response lenses
    cmrsrrsRuleSet,
    cmrsrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateMatchmakingRuleSet' smart constructor.
data CreateMatchmakingRuleSet = CreateMatchmakingRuleSet'
  { -- | A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
    name :: Types.Name,
    -- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
    ruleSetBody :: Types.RuleSetBody,
    -- | A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMatchmakingRuleSet' value with any optional fields omitted.
mkCreateMatchmakingRuleSet ::
  -- | 'name'
  Types.Name ->
  -- | 'ruleSetBody'
  Types.RuleSetBody ->
  CreateMatchmakingRuleSet
mkCreateMatchmakingRuleSet name ruleSetBody =
  CreateMatchmakingRuleSet' {name, ruleSetBody, tags = Core.Nothing}

-- | A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsName :: Lens.Lens' CreateMatchmakingRuleSet Types.Name
cmrsName = Lens.field @"name"
{-# DEPRECATED cmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
--
-- /Note:/ Consider using 'ruleSetBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsRuleSetBody :: Lens.Lens' CreateMatchmakingRuleSet Types.RuleSetBody
cmrsRuleSetBody = Lens.field @"ruleSetBody"
{-# DEPRECATED cmrsRuleSetBody "Use generic-lens or generic-optics with 'ruleSetBody' instead." #-}

-- | A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsTags :: Lens.Lens' CreateMatchmakingRuleSet (Core.Maybe [Types.Tag])
cmrsTags = Lens.field @"tags"
{-# DEPRECATED cmrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateMatchmakingRuleSet where
  toJSON CreateMatchmakingRuleSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RuleSetBody" Core..= ruleSetBody),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateMatchmakingRuleSet where
  type Rs CreateMatchmakingRuleSet = CreateMatchmakingRuleSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.CreateMatchmakingRuleSet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMatchmakingRuleSetResponse'
            Core.<$> (x Core..: "RuleSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateMatchmakingRuleSetResponse' smart constructor.
data CreateMatchmakingRuleSetResponse = CreateMatchmakingRuleSetResponse'
  { -- | The newly created matchmaking rule set.
    ruleSet :: Types.MatchmakingRuleSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateMatchmakingRuleSetResponse' value with any optional fields omitted.
mkCreateMatchmakingRuleSetResponse ::
  -- | 'ruleSet'
  Types.MatchmakingRuleSet ->
  -- | 'responseStatus'
  Core.Int ->
  CreateMatchmakingRuleSetResponse
mkCreateMatchmakingRuleSetResponse ruleSet responseStatus =
  CreateMatchmakingRuleSetResponse' {ruleSet, responseStatus}

-- | The newly created matchmaking rule set.
--
-- /Note:/ Consider using 'ruleSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsrrsRuleSet :: Lens.Lens' CreateMatchmakingRuleSetResponse Types.MatchmakingRuleSet
cmrsrrsRuleSet = Lens.field @"ruleSet"
{-# DEPRECATED cmrsrrsRuleSet "Use generic-lens or generic-optics with 'ruleSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsrrsResponseStatus :: Lens.Lens' CreateMatchmakingRuleSetResponse Core.Int
cmrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cmrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
