{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ValidateMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the syntax of a matchmaking rule or rule set. This operation checks that the rule set is using syntactically correct JSON and that it conforms to allowed property expressions. To validate syntax, provide a rule set JSON string.
--
-- __Learn more__
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set>
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
module Network.AWS.GameLift.ValidateMatchmakingRuleSet
  ( -- * Creating a request
    ValidateMatchmakingRuleSet (..),
    mkValidateMatchmakingRuleSet,

    -- ** Request lenses
    vmrsRuleSetBody,

    -- * Destructuring the response
    ValidateMatchmakingRuleSetResponse (..),
    mkValidateMatchmakingRuleSetResponse,

    -- ** Response lenses
    vmrsrsValid,
    vmrsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkValidateMatchmakingRuleSet' smart constructor.
newtype ValidateMatchmakingRuleSet = ValidateMatchmakingRuleSet'
  { -- | A collection of matchmaking rules to validate, formatted as a JSON string.
    ruleSetBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- * 'ruleSetBody' - A collection of matchmaking rules to validate, formatted as a JSON string.
mkValidateMatchmakingRuleSet ::
  -- | 'ruleSetBody'
  Lude.Text ->
  ValidateMatchmakingRuleSet
mkValidateMatchmakingRuleSet pRuleSetBody_ =
  ValidateMatchmakingRuleSet' {ruleSetBody = pRuleSetBody_}

-- | A collection of matchmaking rules to validate, formatted as a JSON string.
--
-- /Note:/ Consider using 'ruleSetBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsRuleSetBody :: Lens.Lens' ValidateMatchmakingRuleSet Lude.Text
vmrsRuleSetBody = Lens.lens (ruleSetBody :: ValidateMatchmakingRuleSet -> Lude.Text) (\s a -> s {ruleSetBody = a} :: ValidateMatchmakingRuleSet)
{-# DEPRECATED vmrsRuleSetBody "Use generic-lens or generic-optics with 'ruleSetBody' instead." #-}

instance Lude.AWSRequest ValidateMatchmakingRuleSet where
  type
    Rs ValidateMatchmakingRuleSet =
      ValidateMatchmakingRuleSetResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          ValidateMatchmakingRuleSetResponse'
            Lude.<$> (x Lude..?> "Valid") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ValidateMatchmakingRuleSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.ValidateMatchmakingRuleSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ValidateMatchmakingRuleSet where
  toJSON ValidateMatchmakingRuleSet' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RuleSetBody" Lude..= ruleSetBody)])

instance Lude.ToPath ValidateMatchmakingRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery ValidateMatchmakingRuleSet where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkValidateMatchmakingRuleSetResponse' smart constructor.
data ValidateMatchmakingRuleSetResponse = ValidateMatchmakingRuleSetResponse'
  { -- | A response indicating whether the rule set is valid.
    valid :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValidateMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'valid' - A response indicating whether the rule set is valid.
-- * 'responseStatus' - The response status code.
mkValidateMatchmakingRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ValidateMatchmakingRuleSetResponse
mkValidateMatchmakingRuleSetResponse pResponseStatus_ =
  ValidateMatchmakingRuleSetResponse'
    { valid = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A response indicating whether the rule set is valid.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsrsValid :: Lens.Lens' ValidateMatchmakingRuleSetResponse (Lude.Maybe Lude.Bool)
vmrsrsValid = Lens.lens (valid :: ValidateMatchmakingRuleSetResponse -> Lude.Maybe Lude.Bool) (\s a -> s {valid = a} :: ValidateMatchmakingRuleSetResponse)
{-# DEPRECATED vmrsrsValid "Use generic-lens or generic-optics with 'valid' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsrsResponseStatus :: Lens.Lens' ValidateMatchmakingRuleSetResponse Lude.Int
vmrsrsResponseStatus = Lens.lens (responseStatus :: ValidateMatchmakingRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ValidateMatchmakingRuleSetResponse)
{-# DEPRECATED vmrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
