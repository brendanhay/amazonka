{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @WebACL@ , which contains the @Rules@ that identify the CloudFront web requests that you want to allow, block, or count. AWS WAF evaluates @Rules@ in order based on the value of @Priority@ for each @Rule@ .
--
-- You also specify a default action, either @ALLOW@ or @BLOCK@ . If a web request doesn't match any of the @Rules@ in a @WebACL@ , AWS WAF responds to the request with the default action.
-- To create and configure a @WebACL@ , perform the following steps:
--
--     * Create and update the @ByteMatchSet@ objects and other predicates that you want to include in @Rules@ . For more information, see 'CreateByteMatchSet' , 'UpdateByteMatchSet' , 'CreateIPSet' , 'UpdateIPSet' , 'CreateSqlInjectionMatchSet' , and 'UpdateSqlInjectionMatchSet' .
--
--
--     * Create and update the @Rules@ that you want to include in the @WebACL@ . For more information, see 'CreateRule' and 'UpdateRule' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateWebACL@ request.
--
--
--     * Submit a @CreateWebACL@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateWebACL' request.
--
--
--     * Submit an 'UpdateWebACL' request to specify the @Rules@ that you want to include in the @WebACL@ , to specify the default action, and to associate the @WebACL@ with a CloudFront distribution.
--
--
-- For more information about how to use the AWS WAF API, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateWebACL
  ( -- * Creating a request
    CreateWebACL (..),
    mkCreateWebACL,

    -- ** Request lenses
    cwaTags,
    cwaName,
    cwaMetricName,
    cwaDefaultAction,
    cwaChangeToken,

    -- * Destructuring the response
    CreateWebACLResponse (..),
    mkCreateWebACLResponse,

    -- ** Response lenses
    cwarsWebACL,
    cwarsChangeToken,
    cwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateWebACL' smart constructor.
data CreateWebACL = CreateWebACL'
  { tags ::
      Lude.Maybe (Lude.NonEmpty Tag),
    name :: Lude.Text,
    metricName :: Lude.Text,
    defaultAction :: WafAction,
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

-- | Creates a value of 'CreateWebACL' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'defaultAction' - The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
-- * 'metricName' - A friendly name or description for the metrics for this @WebACL@ .The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
-- * 'name' - A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
-- * 'tags' -
mkCreateWebACL ::
  -- | 'name'
  Lude.Text ->
  -- | 'metricName'
  Lude.Text ->
  -- | 'defaultAction'
  WafAction ->
  -- | 'changeToken'
  Lude.Text ->
  CreateWebACL
mkCreateWebACL pName_ pMetricName_ pDefaultAction_ pChangeToken_ =
  CreateWebACL'
    { tags = Lude.Nothing,
      name = pName_,
      metricName = pMetricName_,
      defaultAction = pDefaultAction_,
      changeToken = pChangeToken_
    }

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaTags :: Lens.Lens' CreateWebACL (Lude.Maybe (Lude.NonEmpty Tag))
cwaTags = Lens.lens (tags :: CreateWebACL -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateWebACL)
{-# DEPRECATED cwaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A friendly name or description of the 'WebACL' . You can't change @Name@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaName :: Lens.Lens' CreateWebACL Lude.Text
cwaName = Lens.lens (name :: CreateWebACL -> Lude.Text) (\s a -> s {name = a} :: CreateWebACL)
{-# DEPRECATED cwaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A friendly name or description for the metrics for this @WebACL@ .The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaMetricName :: Lens.Lens' CreateWebACL Lude.Text
cwaMetricName = Lens.lens (metricName :: CreateWebACL -> Lude.Text) (\s a -> s {metricName = a} :: CreateWebACL)
{-# DEPRECATED cwaMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The action that you want AWS WAF to take when a request doesn't match the criteria specified in any of the @Rule@ objects that are associated with the @WebACL@ .
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaDefaultAction :: Lens.Lens' CreateWebACL WafAction
cwaDefaultAction = Lens.lens (defaultAction :: CreateWebACL -> WafAction) (\s a -> s {defaultAction = a} :: CreateWebACL)
{-# DEPRECATED cwaDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwaChangeToken :: Lens.Lens' CreateWebACL Lude.Text
cwaChangeToken = Lens.lens (changeToken :: CreateWebACL -> Lude.Text) (\s a -> s {changeToken = a} :: CreateWebACL)
{-# DEPRECATED cwaChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateWebACL where
  type Rs CreateWebACL = CreateWebACLResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateWebACLResponse'
            Lude.<$> (x Lude..?> "WebACL")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateWebACL where
  toJSON CreateWebACL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MetricName" Lude..= metricName),
            Lude.Just ("DefaultAction" Lude..= defaultAction),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateWebACLResponse' smart constructor.
data CreateWebACLResponse = CreateWebACLResponse'
  { webACL ::
      Lude.Maybe WebACL,
    changeToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateWebACLResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
-- * 'webACL' - The 'WebACL' returned in the @CreateWebACL@ response.
mkCreateWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateWebACLResponse
mkCreateWebACLResponse pResponseStatus_ =
  CreateWebACLResponse'
    { webACL = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'WebACL' returned in the @CreateWebACL@ response.
--
-- /Note:/ Consider using 'webACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwarsWebACL :: Lens.Lens' CreateWebACLResponse (Lude.Maybe WebACL)
cwarsWebACL = Lens.lens (webACL :: CreateWebACLResponse -> Lude.Maybe WebACL) (\s a -> s {webACL = a} :: CreateWebACLResponse)
{-# DEPRECATED cwarsWebACL "Use generic-lens or generic-optics with 'webACL' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwarsChangeToken :: Lens.Lens' CreateWebACLResponse (Lude.Maybe Lude.Text)
cwarsChangeToken = Lens.lens (changeToken :: CreateWebACLResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateWebACLResponse)
{-# DEPRECATED cwarsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwarsResponseStatus :: Lens.Lens' CreateWebACLResponse Lude.Int
cwarsResponseStatus = Lens.lens (responseStatus :: CreateWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateWebACLResponse)
{-# DEPRECATED cwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
