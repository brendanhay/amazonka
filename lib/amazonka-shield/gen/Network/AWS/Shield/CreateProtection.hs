{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables AWS Shield Advanced for a specific AWS resource. The resource can be an Amazon CloudFront distribution, Elastic Load Balancing load balancer, AWS Global Accelerator accelerator, Elastic IP Address, or an Amazon Route 53 hosted zone.
--
-- You can add protection to only a single resource with each CreateProtection request. If you want to add protection to multiple resources at once, use the <https://console.aws.amazon.com/waf/ AWS WAF console> . For more information see <https://docs.aws.amazon.com/waf/latest/developerguide/getting-started-ddos.html Getting Started with AWS Shield Advanced> and <https://docs.aws.amazon.com/waf/latest/developerguide/configure-new-protection.html Add AWS Shield Advanced Protection to more AWS Resources> .
module Network.AWS.Shield.CreateProtection
  ( -- * Creating a request
    CreateProtection (..),
    mkCreateProtection,

    -- ** Request lenses
    cpResourceARN,
    cpName,

    -- * Destructuring the response
    CreateProtectionResponse (..),
    mkCreateProtectionResponse,

    -- ** Response lenses
    cprsProtectionId,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkCreateProtection' smart constructor.
data CreateProtection = CreateProtection'
  { -- | The ARN (Amazon Resource Name) of the resource to be protected.
    --
    -- The ARN should be in one of the following formats:
    --
    --     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
    --
    --
    --     * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @
    --
    --
    --     * For an AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @
    --
    --
    --     * For an AWS Global Accelerator accelerator: @arn:aws:globalaccelerator::/account-id/ :accelerator//accelerator-id/ @
    --
    --
    --     * For Amazon Route 53: @arn:aws:route53:::hostedzone//hosted-zone-id/ @
    --
    --
    --     * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @
    resourceARN :: Lude.Text,
    -- | Friendly name for the @Protection@ you are creating.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProtection' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @
--
--
--     * For an AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @
--
--
--     * For an AWS Global Accelerator accelerator: @arn:aws:globalaccelerator::/account-id/ :accelerator//accelerator-id/ @
--
--
--     * For Amazon Route 53: @arn:aws:route53:::hostedzone//hosted-zone-id/ @
--
--
--     * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @
--
--
-- * 'name' - Friendly name for the @Protection@ you are creating.
mkCreateProtection ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateProtection
mkCreateProtection pResourceARN_ pName_ =
  CreateProtection' {resourceARN = pResourceARN_, name = pName_}

-- | The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @
--
--
--     * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @
--
--
--     * For an AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @
--
--
--     * For an AWS Global Accelerator accelerator: @arn:aws:globalaccelerator::/account-id/ :accelerator//accelerator-id/ @
--
--
--     * For Amazon Route 53: @arn:aws:route53:::hostedzone//hosted-zone-id/ @
--
--
--     * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @
--
--
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResourceARN :: Lens.Lens' CreateProtection Lude.Text
cpResourceARN = Lens.lens (resourceARN :: CreateProtection -> Lude.Text) (\s a -> s {resourceARN = a} :: CreateProtection)
{-# DEPRECATED cpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Friendly name for the @Protection@ you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProtection Lude.Text
cpName = Lens.lens (name :: CreateProtection -> Lude.Text) (\s a -> s {name = a} :: CreateProtection)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateProtection where
  type Rs CreateProtection = CreateProtectionResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProtectionResponse'
            Lude.<$> (x Lude..?> "ProtectionId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProtection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.CreateProtection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProtection where
  toJSON CreateProtection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateProtection where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProtection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProtectionResponse' smart constructor.
data CreateProtectionResponse = CreateProtectionResponse'
  { -- | The unique identifier (ID) for the 'Protection' object that is created.
    protectionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProtectionResponse' with the minimum fields required to make a request.
--
-- * 'protectionId' - The unique identifier (ID) for the 'Protection' object that is created.
-- * 'responseStatus' - The response status code.
mkCreateProtectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProtectionResponse
mkCreateProtectionResponse pResponseStatus_ =
  CreateProtectionResponse'
    { protectionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier (ID) for the 'Protection' object that is created.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProtectionId :: Lens.Lens' CreateProtectionResponse (Lude.Maybe Lude.Text)
cprsProtectionId = Lens.lens (protectionId :: CreateProtectionResponse -> Lude.Maybe Lude.Text) (\s a -> s {protectionId = a} :: CreateProtectionResponse)
{-# DEPRECATED cprsProtectionId "Use generic-lens or generic-optics with 'protectionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProtectionResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProtectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProtectionResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
