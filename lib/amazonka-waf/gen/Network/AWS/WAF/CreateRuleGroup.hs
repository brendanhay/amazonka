{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RuleGroup@ . A rule group is a collection of predefined rules that you add to a web ACL. You use 'UpdateRuleGroup' to add rules to the rule group.
--
-- Rule groups are subject to the following limits:
--
--     * Three rule groups per account. You can request an increase to this limit by contacting customer support.
--
--
--     * One rule group per web ACL.
--
--
--     * Ten rules per rule group.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateRuleGroup
  ( -- * Creating a request
    CreateRuleGroup (..),
    mkCreateRuleGroup,

    -- ** Request lenses
    crgMetricName,
    crgName,
    crgChangeToken,
    crgTags,

    -- * Destructuring the response
    CreateRuleGroupResponse (..),
    mkCreateRuleGroupResponse,

    -- ** Response lenses
    crgrsChangeToken,
    crgrsRuleGroup,
    crgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { -- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
    metricName :: Lude.Text,
    -- | A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text,
    -- |
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRuleGroup' with the minimum fields required to make a request.
--
-- * 'metricName' - A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
-- * 'name' - A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'tags' -
mkCreateRuleGroup ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateRuleGroup
mkCreateRuleGroup pMetricName_ pName_ pChangeToken_ =
  CreateRuleGroup'
    { metricName = pMetricName_,
      name = pName_,
      changeToken = pChangeToken_,
      tags = Lude.Nothing
    }

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgMetricName :: Lens.Lens' CreateRuleGroup Lude.Text
crgMetricName = Lens.lens (metricName :: CreateRuleGroup -> Lude.Text) (\s a -> s {metricName = a} :: CreateRuleGroup)
{-# DEPRECATED crgMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgName :: Lens.Lens' CreateRuleGroup Lude.Text
crgName = Lens.lens (name :: CreateRuleGroup -> Lude.Text) (\s a -> s {name = a} :: CreateRuleGroup)
{-# DEPRECATED crgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgChangeToken :: Lens.Lens' CreateRuleGroup Lude.Text
crgChangeToken = Lens.lens (changeToken :: CreateRuleGroup -> Lude.Text) (\s a -> s {changeToken = a} :: CreateRuleGroup)
{-# DEPRECATED crgChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateRuleGroup (Lude.Maybe (Lude.NonEmpty Tag))
crgTags = Lens.lens (tags :: CreateRuleGroup -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateRuleGroup)
{-# DEPRECATED crgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRuleGroup where
  type Rs CreateRuleGroup = CreateRuleGroupResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRuleGroupResponse'
            Lude.<$> (x Lude..?> "ChangeToken")
            Lude.<*> (x Lude..?> "RuleGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRuleGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateRuleGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRuleGroup where
  toJSON CreateRuleGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRuleGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRuleGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { -- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | An empty 'RuleGroup' .
    ruleGroup :: Lude.Maybe RuleGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRuleGroupResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'ruleGroup' - An empty 'RuleGroup' .
-- * 'responseStatus' - The response status code.
mkCreateRuleGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRuleGroupResponse
mkCreateRuleGroupResponse pResponseStatus_ =
  CreateRuleGroupResponse'
    { changeToken = Lude.Nothing,
      ruleGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsChangeToken :: Lens.Lens' CreateRuleGroupResponse (Lude.Maybe Lude.Text)
crgrsChangeToken = Lens.lens (changeToken :: CreateRuleGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateRuleGroupResponse)
{-# DEPRECATED crgrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | An empty 'RuleGroup' .
--
-- /Note:/ Consider using 'ruleGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsRuleGroup :: Lens.Lens' CreateRuleGroupResponse (Lude.Maybe RuleGroup)
crgrsRuleGroup = Lens.lens (ruleGroup :: CreateRuleGroupResponse -> Lude.Maybe RuleGroup) (\s a -> s {ruleGroup = a} :: CreateRuleGroupResponse)
{-# DEPRECATED crgrsRuleGroup "Use generic-lens or generic-optics with 'ruleGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsResponseStatus :: Lens.Lens' CreateRuleGroupResponse Lude.Int
crgrsResponseStatus = Lens.lens (responseStatus :: CreateRuleGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRuleGroupResponse)
{-# DEPRECATED crgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
