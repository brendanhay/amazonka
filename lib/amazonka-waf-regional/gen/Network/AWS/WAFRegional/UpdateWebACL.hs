{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @WebACL@ . Each @Rule@ identifies web requests that you want to allow, block, or count. When you update a @WebACL@ , you specify the following values:
--
--
--     * A default action for the @WebACL@ , either @ALLOW@ or @BLOCK@ . AWS WAF performs the default action if a request doesn't match the criteria in any of the @Rules@ in a @WebACL@ .
--
--
--     * The @Rules@ that you want to add or delete. If you want to replace one @Rule@ with another, you delete the existing @Rule@ and add the new one.
--
--
--     * For each @Rule@ , whether you want AWS WAF to allow requests, block requests, or count requests that match the conditions in the @Rule@ .
--
--
--     * The order in which you want AWS WAF to evaluate the @Rules@ in a @WebACL@ . If you add more than one @Rule@ to a @WebACL@ , AWS WAF evaluates each request against the @Rules@ in order based on the value of @Priority@ . (The @Rule@ that has the lowest value for @Priority@ is evaluated first.) When a web request matches all the predicates (such as @ByteMatchSets@ and @IPSets@ ) in a @Rule@ , AWS WAF immediately takes the corresponding action, allow or block, and doesn't evaluate the request against the remaining @Rules@ in the @WebACL@ , if any.
--
--
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--
--     * Create a @WebACL@ . See 'CreateWebACL' .
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--
--     * Submit an @UpdateWebACL@ request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
-- The @ActivatedRule@ can be a rule group. If you specify a rule group as your @ActivatedRule@ , you can exclude specific rules from that rule group.
-- If you already have a rule group associated with a web ACL and want to submit an @UpdateWebACL@ request to exclude certain rules from that rule group, you must first remove the rule group from the web ACL, the re-insert it again, specifying the excluded rules. For details, see 'ActivatedRule$ExcludedRules' .
--
--
-- Be aware that if you try to add a RATE_BASED rule to a web ACL without setting the rule type when first creating the rule, the 'UpdateWebACL' request will fail because the request tries to add a REGULAR rule (the default rule type) with the specified ID, which does not exist.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateWebACL
  ( -- * Creating a request
    UpdateWebACL (..),
    mkUpdateWebACL,

    -- ** Request lenses
    uwaUpdates,
    uwaDefaultAction,
    uwaWebACLId,
    uwaChangeToken,

    -- * Destructuring the response
    UpdateWebACLResponse (..),
    mkUpdateWebACLResponse,

    -- ** Response lenses
    uwarsChangeToken,
    uwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateWebACL' smart constructor.
data UpdateWebACL = UpdateWebACL'
  { updates ::
      Lude.Maybe [WebACLUpdate],
    defaultAction :: Lude.Maybe WafAction,
    webACLId :: Lude.Text,
    changeToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWebACL' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'defaultAction' - A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
-- * 'updates' - An array of updates to make to the 'WebACL' .
--
-- An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:
--
--     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@
--
--
--     * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
--
--     * 'WafAction' : Contains @Type@
--
--
-- * 'webACLId' - The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
mkUpdateWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  UpdateWebACL
mkUpdateWebACL pWebACLId_ pChangeToken_ =
  UpdateWebACL'
    { updates = Lude.Nothing,
      defaultAction = Lude.Nothing,
      webACLId = pWebACLId_,
      changeToken = pChangeToken_
    }

-- | An array of updates to make to the 'WebACL' .
--
-- An array of @WebACLUpdate@ objects that you want to insert into or delete from a 'WebACL' . For more information, see the applicable data types:
--
--     * 'WebACLUpdate' : Contains @Action@ and @ActivatedRule@
--
--
--     * 'ActivatedRule' : Contains @Action@ , @OverrideAction@ , @Priority@ , @RuleId@ , and @Type@ . @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case, you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
--
--     * 'WafAction' : Contains @Type@
--
--
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaUpdates :: Lens.Lens' UpdateWebACL (Lude.Maybe [WebACLUpdate])
uwaUpdates = Lens.lens (updates :: UpdateWebACL -> Lude.Maybe [WebACLUpdate]) (\s a -> s {updates = a} :: UpdateWebACL)
{-# DEPRECATED uwaUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF performs the default action if a request doesn't match the criteria in any of the rules in a web ACL.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaDefaultAction :: Lens.Lens' UpdateWebACL (Lude.Maybe WafAction)
uwaDefaultAction = Lens.lens (defaultAction :: UpdateWebACL -> Lude.Maybe WafAction) (\s a -> s {defaultAction = a} :: UpdateWebACL)
{-# DEPRECATED uwaDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | The @WebACLId@ of the 'WebACL' that you want to update. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaWebACLId :: Lens.Lens' UpdateWebACL Lude.Text
uwaWebACLId = Lens.lens (webACLId :: UpdateWebACL -> Lude.Text) (\s a -> s {webACLId = a} :: UpdateWebACL)
{-# DEPRECATED uwaWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwaChangeToken :: Lens.Lens' UpdateWebACL Lude.Text
uwaChangeToken = Lens.lens (changeToken :: UpdateWebACL -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateWebACL)
{-# DEPRECATED uwaChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest UpdateWebACL where
  type Rs UpdateWebACL = UpdateWebACLResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateWebACLResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.UpdateWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWebACL where
  toJSON UpdateWebACL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Updates" Lude..=) Lude.<$> updates,
            ("DefaultAction" Lude..=) Lude.<$> defaultAction,
            Lude.Just ("WebACLId" Lude..= webACLId),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath UpdateWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWebACLResponse' smart constructor.
data UpdateWebACLResponse = UpdateWebACLResponse'
  { changeToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateWebACLResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateWebACLResponse
mkUpdateWebACLResponse pResponseStatus_ =
  UpdateWebACLResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwarsChangeToken :: Lens.Lens' UpdateWebACLResponse (Lude.Maybe Lude.Text)
uwarsChangeToken = Lens.lens (changeToken :: UpdateWebACLResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateWebACLResponse)
{-# DEPRECATED uwarsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwarsResponseStatus :: Lens.Lens' UpdateWebACLResponse Lude.Int
uwarsResponseStatus = Lens.lens (responseStatus :: UpdateWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWebACLResponse)
{-# DEPRECATED uwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
