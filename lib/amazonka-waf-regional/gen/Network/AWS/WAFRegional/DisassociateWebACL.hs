{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DisassociateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a web ACL from the specified resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.DisassociateWebACL
  ( -- * Creating a request
    DisassociateWebACL (..),
    mkDisassociateWebACL,

    -- ** Request lenses
    dwaResourceARN,

    -- * Destructuring the response
    DisassociateWebACLResponse (..),
    mkDisassociateWebACLResponse,

    -- ** Response lenses
    dwaclrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkDisassociateWebACL' smart constructor.
newtype DisassociateWebACL = DisassociateWebACL'
  { resourceARN ::
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

-- | Creates a value of 'DisassociateWebACL' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
mkDisassociateWebACL ::
  -- | 'resourceARN'
  Lude.Text ->
  DisassociateWebACL
mkDisassociateWebACL pResourceARN_ =
  DisassociateWebACL' {resourceARN = pResourceARN_}

-- | The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed, either an application load balancer or Amazon API Gateway stage.
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
dwaResourceARN :: Lens.Lens' DisassociateWebACL Lude.Text
dwaResourceARN = Lens.lens (resourceARN :: DisassociateWebACL -> Lude.Text) (\s a -> s {resourceARN = a} :: DisassociateWebACL)
{-# DEPRECATED dwaResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DisassociateWebACL where
  type Rs DisassociateWebACL = DisassociateWebACLResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateWebACLResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.DisassociateWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateWebACL where
  toJSON DisassociateWebACL' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath DisassociateWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateWebACLResponse' smart constructor.
newtype DisassociateWebACLResponse = DisassociateWebACLResponse'
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

-- | Creates a value of 'DisassociateWebACLResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateWebACLResponse
mkDisassociateWebACLResponse pResponseStatus_ =
  DisassociateWebACLResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaclrsResponseStatus :: Lens.Lens' DisassociateWebACLResponse Lude.Int
dwaclrsResponseStatus = Lens.lens (responseStatus :: DisassociateWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateWebACLResponse)
{-# DEPRECATED dwaclrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
