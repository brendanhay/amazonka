{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetWebACLForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the web ACL for the specified resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.GetWebACLForResource
  ( -- * Creating a request
    GetWebACLForResource (..),
    mkGetWebACLForResource,

    -- ** Request lenses
    gwafrResourceARN,

    -- * Destructuring the response
    GetWebACLForResourceResponse (..),
    mkGetWebACLForResourceResponse,

    -- ** Response lenses
    gwafrrsWebACLSummary,
    gwafrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetWebACLForResource' smart constructor.
newtype GetWebACLForResource = GetWebACLForResource'
  { -- | The ARN (Amazon Resource Name) of the resource for which to get the web ACL, either an application load balancer or Amazon API Gateway stage.
    --
    -- The ARN should be in one of the following formats:
    --
    --     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
    --
    --
    --     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWebACLForResource' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource for which to get the web ACL, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
mkGetWebACLForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  GetWebACLForResource
mkGetWebACLForResource pResourceARN_ =
  GetWebACLForResource' {resourceARN = pResourceARN_}

-- | The ARN (Amazon Resource Name) of the resource for which to get the web ACL, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
--
--
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwafrResourceARN :: Lens.Lens' GetWebACLForResource Lude.Text
gwafrResourceARN = Lens.lens (resourceARN :: GetWebACLForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: GetWebACLForResource)
{-# DEPRECATED gwafrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetWebACLForResource where
  type Rs GetWebACLForResource = GetWebACLForResourceResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWebACLForResourceResponse'
            Lude.<$> (x Lude..?> "WebACLSummary")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWebACLForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.GetWebACLForResource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWebACLForResource where
  toJSON GetWebACLForResource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetWebACLForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWebACLForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWebACLForResourceResponse' smart constructor.
data GetWebACLForResourceResponse = GetWebACLForResourceResponse'
  { -- | Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
    webACLSummary :: Lude.Maybe WebACLSummary,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWebACLForResourceResponse' with the minimum fields required to make a request.
--
-- * 'webACLSummary' - Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
-- * 'responseStatus' - The response status code.
mkGetWebACLForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWebACLForResourceResponse
mkGetWebACLForResourceResponse pResponseStatus_ =
  GetWebACLForResourceResponse'
    { webACLSummary = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
--
-- /Note:/ Consider using 'webACLSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwafrrsWebACLSummary :: Lens.Lens' GetWebACLForResourceResponse (Lude.Maybe WebACLSummary)
gwafrrsWebACLSummary = Lens.lens (webACLSummary :: GetWebACLForResourceResponse -> Lude.Maybe WebACLSummary) (\s a -> s {webACLSummary = a} :: GetWebACLForResourceResponse)
{-# DEPRECATED gwafrrsWebACLSummary "Use generic-lens or generic-optics with 'webACLSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwafrrsResponseStatus :: Lens.Lens' GetWebACLForResourceResponse Lude.Int
gwafrrsResponseStatus = Lens.lens (responseStatus :: GetWebACLForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWebACLForResourceResponse)
{-# DEPRECATED gwafrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
