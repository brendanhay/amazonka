{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.CreateRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Rule@ , which contains the @IPSet@ objects, @ByteMatchSet@ objects, and other predicates that identify the requests that you want to block. If you add more than one predicate to a @Rule@ , a request must match all of the specifications to be allowed or blocked. For example, suppose that you add the following to a @Rule@ :
--
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@
--
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to blocks requests that satisfy the @Rule@ . For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ .
-- To create and configure a @Rule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the @Rule@ . For more information, see 'CreateByteMatchSet' , 'CreateIPSet' , and 'CreateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRule@ request.
--
--
--     * Submit a @CreateRule@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--
--     * Submit an @UpdateRule@ request to specify the predicates that you want to include in the @Rule@ .
--
--
--     * Create and update a @WebACL@ that contains the @Rule@ . For more information, see 'CreateWebACL' .
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.CreateRule
  ( -- * Creating a request
    CreateRule (..),
    mkCreateRule,

    -- ** Request lenses
    crMetricName,
    crName,
    crChangeToken,
    crTags,

    -- * Destructuring the response
    CreateRuleResponse (..),
    mkCreateRuleResponse,

    -- ** Response lenses
    crrsRule,
    crrsChangeToken,
    crrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkCreateRule' smart constructor.
data CreateRule = CreateRule'
  { -- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @Rule@ .
    metricName :: Lude.Text,
    -- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
    name :: Lude.Text,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text,
    -- |
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRule' with the minimum fields required to make a request.
--
-- * 'metricName' - A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @Rule@ .
-- * 'name' - A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'tags' -
mkCreateRule ::
  -- | 'metricName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateRule
mkCreateRule pMetricName_ pName_ pChangeToken_ =
  CreateRule'
    { metricName = pMetricName_,
      name = pName_,
      changeToken = pChangeToken_,
      tags = Lude.Nothing
    }

-- | A friendly name or description for the metrics for this @Rule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @Rule@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMetricName :: Lens.Lens' CreateRule Lude.Text
crMetricName = Lens.lens (metricName :: CreateRule -> Lude.Text) (\s a -> s {metricName = a} :: CreateRule)
{-# DEPRECATED crMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CreateRule Lude.Text
crName = Lens.lens (name :: CreateRule -> Lude.Text) (\s a -> s {name = a} :: CreateRule)
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crChangeToken :: Lens.Lens' CreateRule Lude.Text
crChangeToken = Lens.lens (changeToken :: CreateRule -> Lude.Text) (\s a -> s {changeToken = a} :: CreateRule)
{-# DEPRECATED crChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRule (Lude.Maybe (Lude.NonEmpty Tag))
crTags = Lens.lens (tags :: CreateRule -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateRule)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRule where
  type Rs CreateRule = CreateRuleResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRuleResponse'
            Lude.<$> (x Lude..?> "Rule")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.CreateRule" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRule where
  toJSON CreateRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRule where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | The 'Rule' returned in the @CreateRule@ response.
    rule :: Lude.Maybe Rule,
    -- | The @ChangeToken@ that you used to submit the @CreateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRuleResponse' with the minimum fields required to make a request.
--
-- * 'rule' - The 'Rule' returned in the @CreateRule@ response.
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkCreateRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRuleResponse
mkCreateRuleResponse pResponseStatus_ =
  CreateRuleResponse'
    { rule = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'Rule' returned in the @CreateRule@ response.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRule :: Lens.Lens' CreateRuleResponse (Lude.Maybe Rule)
crrsRule = Lens.lens (rule :: CreateRuleResponse -> Lude.Maybe Rule) (\s a -> s {rule = a} :: CreateRuleResponse)
{-# DEPRECATED crrsRule "Use generic-lens or generic-optics with 'rule' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsChangeToken :: Lens.Lens' CreateRuleResponse (Lude.Maybe Lude.Text)
crrsChangeToken = Lens.lens (changeToken :: CreateRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateRuleResponse)
{-# DEPRECATED crrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRuleResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRuleResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
