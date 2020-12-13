{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RuleGroup' that is specified by the @RuleGroupId@ that you included in the @GetRuleGroup@ request.
--
-- To view the rules in a rule group, use 'ListActivatedRulesInRuleGroup' .
module Network.AWS.WAF.GetRuleGroup
  ( -- * Creating a request
    GetRuleGroup (..),
    mkGetRuleGroup,

    -- ** Request lenses
    grgRuleGroupId,

    -- * Destructuring the response
    GetRuleGroupResponse (..),
    mkGetRuleGroupResponse,

    -- ** Response lenses
    grgrsRuleGroup,
    grgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetRuleGroup' smart constructor.
newtype GetRuleGroup = GetRuleGroup'
  { -- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
    ruleGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRuleGroup' with the minimum fields required to make a request.
--
-- * 'ruleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
mkGetRuleGroup ::
  -- | 'ruleGroupId'
  Lude.Text ->
  GetRuleGroup
mkGetRuleGroup pRuleGroupId_ =
  GetRuleGroup' {ruleGroupId = pRuleGroupId_}

-- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgRuleGroupId :: Lens.Lens' GetRuleGroup Lude.Text
grgRuleGroupId = Lens.lens (ruleGroupId :: GetRuleGroup -> Lude.Text) (\s a -> s {ruleGroupId = a} :: GetRuleGroup)
{-# DEPRECATED grgRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

instance Lude.AWSRequest GetRuleGroup where
  type Rs GetRuleGroup = GetRuleGroupResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRuleGroupResponse'
            Lude.<$> (x Lude..?> "RuleGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRuleGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetRuleGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRuleGroup where
  toJSON GetRuleGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RuleGroupId" Lude..= ruleGroupId)])

instance Lude.ToPath GetRuleGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRuleGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { -- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
    ruleGroup :: Lude.Maybe RuleGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRuleGroupResponse' with the minimum fields required to make a request.
--
-- * 'ruleGroup' - Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
-- * 'responseStatus' - The response status code.
mkGetRuleGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRuleGroupResponse
mkGetRuleGroupResponse pResponseStatus_ =
  GetRuleGroupResponse'
    { ruleGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
--
-- /Note:/ Consider using 'ruleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrsRuleGroup :: Lens.Lens' GetRuleGroupResponse (Lude.Maybe RuleGroup)
grgrsRuleGroup = Lens.lens (ruleGroup :: GetRuleGroupResponse -> Lude.Maybe RuleGroup) (\s a -> s {ruleGroup = a} :: GetRuleGroupResponse)
{-# DEPRECATED grgrsRuleGroup "Use generic-lens or generic-optics with 'ruleGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgrsResponseStatus :: Lens.Lens' GetRuleGroupResponse Lude.Int
grgrsResponseStatus = Lens.lens (responseStatus :: GetRuleGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRuleGroupResponse)
{-# DEPRECATED grgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
