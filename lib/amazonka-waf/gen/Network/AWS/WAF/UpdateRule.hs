{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a @Rule@ . Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a @Rule@ , a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose that you add the following to a @Rule@ :
--
--
--     * A @ByteMatchSet@ that matches the value @BadBot@ in the @User-Agent@ header
--
--
--     * An @IPSet@ that matches the IP address @192.0.2.44@
--
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to block requests that satisfy the @Rule@ . For a request to be blocked, the @User-Agent@ header in the request must contain the value @BadBot@ /and/ the request must originate from the IP address 192.0.2.44.
-- To create and configure a @Rule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the @Rule@ .
--
--
--     * Create the @Rule@ . See 'CreateRule' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--
--     * Submit an @UpdateRule@ request to add predicates to the @Rule@ .
--
--
--     * Create and update a @WebACL@ that contains the @Rule@ . See 'CreateWebACL' .
--
--
-- If you want to replace one @ByteMatchSet@ or @IPSet@ with another, you delete the existing one and add the new one.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.UpdateRule
  ( -- * Creating a request
    UpdateRule (..),
    mkUpdateRule,

    -- ** Request lenses
    urRuleId,
    urUpdates,
    urChangeToken,

    -- * Destructuring the response
    UpdateRuleResponse (..),
    mkUpdateRuleResponse,

    -- ** Response lenses
    urrsChangeToken,
    urrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
    ruleId :: Lude.Text,
    -- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:
    --
    --
    --     * 'RuleUpdate' : Contains @Action@ and @Predicate@
    --
    --
    --     * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    updates :: [RuleUpdate],
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRule' with the minimum fields required to make a request.
--
-- * 'ruleId' - The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
-- * 'updates' - An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:
--
--
--     * 'RuleUpdate' : Contains @Action@ and @Predicate@
--
--
--     * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
mkUpdateRule ::
  -- | 'ruleId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateRule
mkUpdateRule pRuleId_ pChangeToken_ =
  UpdateRule'
    { ruleId = pRuleId_,
      updates = Lude.mempty,
      changeToken = pChangeToken_
    }

-- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRuleId :: Lens.Lens' UpdateRule Lude.Text
urRuleId = Lens.lens (ruleId :: UpdateRule -> Lude.Text) (\s a -> s {ruleId = a} :: UpdateRule)
{-# DEPRECATED urRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:
--
--
--     * 'RuleUpdate' : Contains @Action@ and @Predicate@
--
--
--     * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urUpdates :: Lens.Lens' UpdateRule [RuleUpdate]
urUpdates = Lens.lens (updates :: UpdateRule -> [RuleUpdate]) (\s a -> s {updates = a} :: UpdateRule)
{-# DEPRECATED urUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urChangeToken :: Lens.Lens' UpdateRule Lude.Text
urChangeToken = Lens.lens (changeToken :: UpdateRule -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateRule)
{-# DEPRECATED urChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateRule where
  type Rs UpdateRule = UpdateRuleResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRuleResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.UpdateRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRule where
  toJSON UpdateRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RuleId" Lude..= ruleId),
            Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateRule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRuleResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRuleResponse
mkUpdateRuleResponse pResponseStatus_ =
  UpdateRuleResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsChangeToken :: Lens.Lens' UpdateRuleResponse (Lude.Maybe Lude.Text)
urrsChangeToken = Lens.lens (changeToken :: UpdateRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateRuleResponse)
{-# DEPRECATED urrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrsResponseStatus :: Lens.Lens' UpdateRuleResponse Lude.Int
urrsResponseStatus = Lens.lens (responseStatus :: UpdateRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRuleResponse)
{-# DEPRECATED urrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
