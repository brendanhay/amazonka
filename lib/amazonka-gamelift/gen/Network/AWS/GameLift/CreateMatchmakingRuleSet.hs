{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cmrsTags,
    cmrsName,
    cmrsRuleSetBody,

    -- * Destructuring the response
    CreateMatchmakingRuleSetResponse (..),
    mkCreateMatchmakingRuleSetResponse,

    -- ** Response lenses
    cmrsrsResponseStatus,
    cmrsrsRuleSet,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateMatchmakingRuleSet' smart constructor.
data CreateMatchmakingRuleSet = CreateMatchmakingRuleSet'
  { tags ::
      Lude.Maybe [Tag],
    name :: Lude.Text,
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

-- | Creates a value of 'CreateMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- * 'name' - A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
-- * 'ruleSetBody' - A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
-- * 'tags' - A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
mkCreateMatchmakingRuleSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'ruleSetBody'
  Lude.Text ->
  CreateMatchmakingRuleSet
mkCreateMatchmakingRuleSet pName_ pRuleSetBody_ =
  CreateMatchmakingRuleSet'
    { tags = Lude.Nothing,
      name = pName_,
      ruleSetBody = pRuleSetBody_
    }

-- | A list of labels to assign to the new matchmaking rule set resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsTags :: Lens.Lens' CreateMatchmakingRuleSet (Lude.Maybe [Tag])
cmrsTags = Lens.lens (tags :: CreateMatchmakingRuleSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMatchmakingRuleSet)
{-# DEPRECATED cmrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique identifier for a matchmaking rule set. A matchmaking configuration identifies the rule set it uses by this name value. Note that the rule set name is different from the optional @name@ field in the rule set body.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsName :: Lens.Lens' CreateMatchmakingRuleSet Lude.Text
cmrsName = Lens.lens (name :: CreateMatchmakingRuleSet -> Lude.Text) (\s a -> s {name = a} :: CreateMatchmakingRuleSet)
{-# DEPRECATED cmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of matchmaking rules, formatted as a JSON string. Comments are not allowed in JSON, but most elements support a description field.
--
-- /Note:/ Consider using 'ruleSetBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsRuleSetBody :: Lens.Lens' CreateMatchmakingRuleSet Lude.Text
cmrsRuleSetBody = Lens.lens (ruleSetBody :: CreateMatchmakingRuleSet -> Lude.Text) (\s a -> s {ruleSetBody = a} :: CreateMatchmakingRuleSet)
{-# DEPRECATED cmrsRuleSetBody "Use generic-lens or generic-optics with 'ruleSetBody' instead." #-}

instance Lude.AWSRequest CreateMatchmakingRuleSet where
  type Rs CreateMatchmakingRuleSet = CreateMatchmakingRuleSetResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMatchmakingRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "RuleSet")
      )

instance Lude.ToHeaders CreateMatchmakingRuleSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateMatchmakingRuleSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateMatchmakingRuleSet where
  toJSON CreateMatchmakingRuleSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("RuleSetBody" Lude..= ruleSetBody)
          ]
      )

instance Lude.ToPath CreateMatchmakingRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateMatchmakingRuleSet where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateMatchmakingRuleSetResponse' smart constructor.
data CreateMatchmakingRuleSetResponse = CreateMatchmakingRuleSetResponse'
  { responseStatus ::
      Lude.Int,
    ruleSet ::
      MatchmakingRuleSet
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'ruleSet' - The newly created matchmaking rule set.
mkCreateMatchmakingRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'ruleSet'
  MatchmakingRuleSet ->
  CreateMatchmakingRuleSetResponse
mkCreateMatchmakingRuleSetResponse pResponseStatus_ pRuleSet_ =
  CreateMatchmakingRuleSetResponse'
    { responseStatus =
        pResponseStatus_,
      ruleSet = pRuleSet_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsrsResponseStatus :: Lens.Lens' CreateMatchmakingRuleSetResponse Lude.Int
cmrsrsResponseStatus = Lens.lens (responseStatus :: CreateMatchmakingRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMatchmakingRuleSetResponse)
{-# DEPRECATED cmrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The newly created matchmaking rule set.
--
-- /Note:/ Consider using 'ruleSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsrsRuleSet :: Lens.Lens' CreateMatchmakingRuleSetResponse MatchmakingRuleSet
cmrsrsRuleSet = Lens.lens (ruleSet :: CreateMatchmakingRuleSetResponse -> MatchmakingRuleSet) (\s a -> s {ruleSet = a} :: CreateMatchmakingRuleSetResponse)
{-# DEPRECATED cmrsrsRuleSet "Use generic-lens or generic-optics with 'ruleSet' instead." #-}
