{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.AssociateWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a web ACL with a resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.AssociateWebACL
  ( -- * Creating a request
    AssociateWebACL (..),
    mkAssociateWebACL,

    -- ** Request lenses
    awaWebACLId,
    awaResourceARN,

    -- * Destructuring the response
    AssociateWebACLResponse (..),
    mkAssociateWebACLResponse,

    -- ** Response lenses
    awarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkAssociateWebACL' smart constructor.
data AssociateWebACL = AssociateWebACL'
  { webACLId :: Lude.Text,
    resourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateWebACL' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource to be protected, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @
--
--
-- * 'webACLId' - A unique identifier (ID) for the web ACL.
mkAssociateWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'resourceARN'
  Lude.Text ->
  AssociateWebACL
mkAssociateWebACL pWebACLId_ pResourceARN_ =
  AssociateWebACL'
    { webACLId = pWebACLId_,
      resourceARN = pResourceARN_
    }

-- | A unique identifier (ID) for the web ACL.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awaWebACLId :: Lens.Lens' AssociateWebACL Lude.Text
awaWebACLId = Lens.lens (webACLId :: AssociateWebACL -> Lude.Text) (\s a -> s {webACLId = a} :: AssociateWebACL)
{-# DEPRECATED awaWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource to be protected, either an application load balancer or Amazon API Gateway stage.
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
awaResourceARN :: Lens.Lens' AssociateWebACL Lude.Text
awaResourceARN = Lens.lens (resourceARN :: AssociateWebACL -> Lude.Text) (\s a -> s {resourceARN = a} :: AssociateWebACL)
{-# DEPRECATED awaResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest AssociateWebACL where
  type Rs AssociateWebACL = AssociateWebACLResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateWebACLResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.AssociateWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateWebACL where
  toJSON AssociateWebACL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WebACLId" Lude..= webACLId),
            Lude.Just ("ResourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath AssociateWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateWebACLResponse' smart constructor.
newtype AssociateWebACLResponse = AssociateWebACLResponse'
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

-- | Creates a value of 'AssociateWebACLResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateWebACLResponse
mkAssociateWebACLResponse pResponseStatus_ =
  AssociateWebACLResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
awarsResponseStatus :: Lens.Lens' AssociateWebACLResponse Lude.Int
awarsResponseStatus = Lens.lens (responseStatus :: AssociateWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateWebACLResponse)
{-# DEPRECATED awarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
