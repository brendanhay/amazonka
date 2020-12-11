{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetRulePriorities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the priorities of the specified rules.
--
-- You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.
module Network.AWS.ELBv2.SetRulePriorities
  ( -- * Creating a request
    SetRulePriorities (..),
    mkSetRulePriorities,

    -- ** Request lenses
    srpRulePriorities,

    -- * Destructuring the response
    SetRulePrioritiesResponse (..),
    mkSetRulePrioritiesResponse,

    -- ** Response lenses
    srprsRules,
    srprsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetRulePriorities' smart constructor.
newtype SetRulePriorities = SetRulePriorities'
  { rulePriorities ::
      [RulePriorityPair]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRulePriorities' with the minimum fields required to make a request.
--
-- * 'rulePriorities' - The rule priorities.
mkSetRulePriorities ::
  SetRulePriorities
mkSetRulePriorities =
  SetRulePriorities' {rulePriorities = Lude.mempty}

-- | The rule priorities.
--
-- /Note:/ Consider using 'rulePriorities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srpRulePriorities :: Lens.Lens' SetRulePriorities [RulePriorityPair]
srpRulePriorities = Lens.lens (rulePriorities :: SetRulePriorities -> [RulePriorityPair]) (\s a -> s {rulePriorities = a} :: SetRulePriorities)
{-# DEPRECATED srpRulePriorities "Use generic-lens or generic-optics with 'rulePriorities' instead." #-}

instance Lude.AWSRequest SetRulePriorities where
  type Rs SetRulePriorities = SetRulePrioritiesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "SetRulePrioritiesResult"
      ( \s h x ->
          SetRulePrioritiesResponse'
            Lude.<$> ( x Lude..@? "Rules" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetRulePriorities where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetRulePriorities where
  toPath = Lude.const "/"

instance Lude.ToQuery SetRulePriorities where
  toQuery SetRulePriorities' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetRulePriorities" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "RulePriorities" Lude.=: Lude.toQueryList "member" rulePriorities
      ]

-- | /See:/ 'mkSetRulePrioritiesResponse' smart constructor.
data SetRulePrioritiesResponse = SetRulePrioritiesResponse'
  { rules ::
      Lude.Maybe [Rule],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetRulePrioritiesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'rules' - Information about the rules.
mkSetRulePrioritiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetRulePrioritiesResponse
mkSetRulePrioritiesResponse pResponseStatus_ =
  SetRulePrioritiesResponse'
    { rules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsRules :: Lens.Lens' SetRulePrioritiesResponse (Lude.Maybe [Rule])
srprsRules = Lens.lens (rules :: SetRulePrioritiesResponse -> Lude.Maybe [Rule]) (\s a -> s {rules = a} :: SetRulePrioritiesResponse)
{-# DEPRECATED srprsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srprsResponseStatus :: Lens.Lens' SetRulePrioritiesResponse Lude.Int
srprsResponseStatus = Lens.lens (responseStatus :: SetRulePrioritiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetRulePrioritiesResponse)
{-# DEPRECATED srprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
