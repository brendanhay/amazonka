{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing matchmaking rule set. To delete the rule set, provide the rule set name. Rule sets cannot be deleted if they are currently being used by a matchmaking configuration.
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
module Network.AWS.GameLift.DeleteMatchmakingRuleSet
  ( -- * Creating a request
    DeleteMatchmakingRuleSet (..),
    mkDeleteMatchmakingRuleSet,

    -- ** Request lenses
    dmrsName,

    -- * Destructuring the response
    DeleteMatchmakingRuleSetResponse (..),
    mkDeleteMatchmakingRuleSetResponse,

    -- ** Response lenses
    dmrsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteMatchmakingRuleSet' smart constructor.
newtype DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSet'
  { name ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMatchmakingRuleSet' with the minimum fields required to make a request.
--
-- * 'name' - A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
mkDeleteMatchmakingRuleSet ::
  -- | 'name'
  Lude.Text ->
  DeleteMatchmakingRuleSet
mkDeleteMatchmakingRuleSet pName_ =
  DeleteMatchmakingRuleSet' {name = pName_}

-- | A unique identifier for a matchmaking rule set to be deleted. (Note: The rule set name is different from the optional "name" field in the rule set body.) You can use either the rule set name or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsName :: Lens.Lens' DeleteMatchmakingRuleSet Lude.Text
dmrsName = Lens.lens (name :: DeleteMatchmakingRuleSet -> Lude.Text) (\s a -> s {name = a} :: DeleteMatchmakingRuleSet)
{-# DEPRECATED dmrsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteMatchmakingRuleSet where
  type Rs DeleteMatchmakingRuleSet = DeleteMatchmakingRuleSetResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMatchmakingRuleSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMatchmakingRuleSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteMatchmakingRuleSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMatchmakingRuleSet where
  toJSON DeleteMatchmakingRuleSet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteMatchmakingRuleSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMatchmakingRuleSet where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDeleteMatchmakingRuleSetResponse' smart constructor.
newtype DeleteMatchmakingRuleSetResponse = DeleteMatchmakingRuleSetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMatchmakingRuleSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMatchmakingRuleSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMatchmakingRuleSetResponse
mkDeleteMatchmakingRuleSetResponse pResponseStatus_ =
  DeleteMatchmakingRuleSetResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrsrsResponseStatus :: Lens.Lens' DeleteMatchmakingRuleSetResponse Lude.Int
dmrsrsResponseStatus = Lens.lens (responseStatus :: DeleteMatchmakingRuleSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMatchmakingRuleSetResponse)
{-# DEPRECATED dmrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
