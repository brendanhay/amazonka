{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.RegisterUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Paid container software products sold through AWS Marketplace must integrate with the AWS Marketplace Metering Service and call the RegisterUsage operation for software entitlement and metering. Free and BYOL products for Amazon ECS or Amazon EKS aren't required to call RegisterUsage, but you may choose to do so if you would like to receive usage data in your seller reports. The sections below explain the behavior of RegisterUsage. RegisterUsage performs two primary functions: metering and entitlement.
--
--
--     * /Entitlement/ : RegisterUsage allows you to verify that the customer running your paid software is subscribed to your product on AWS Marketplace, enabling you to guard against unauthorized use. Your container image that integrates with RegisterUsage is only required to guard against unauthorized use at container startup, as such a CustomerNotSubscribedException/PlatformNotSupportedException will only be thrown on the initial call to RegisterUsage. Subsequent calls from the same Amazon ECS task instance (e.g. task-id) or Amazon EKS pod will not throw a CustomerNotSubscribedException, even if the customer unsubscribes while the Amazon ECS task or Amazon EKS pod is still running.
--
--
--     * /Metering/ : RegisterUsage meters software use per ECS task, per hour, or per pod for Amazon EKS with usage prorated to the second. A minimum of 1 minute of usage applies to tasks that are short lived. For example, if a customer has a 10 node Amazon ECS or Amazon EKS cluster and a service configured as a Daemon Set, then Amazon ECS or Amazon EKS will launch a task on all 10 cluster nodes and the customer will be charged: (10 * hourly_rate). Metering for software use is automatically handled by the AWS Marketplace Metering Control Plane -- your software is not required to perform any metering specific actions, other than call RegisterUsage once for metering of software use to commence. The AWS Marketplace Metering Control Plane will also continue to bill customers for running ECS tasks and Amazon EKS pods, regardless of the customers subscription state, removing the need for your software to perform entitlement checks at runtime.
module Network.AWS.MarketplaceMetering.RegisterUsage
  ( -- * Creating a request
    RegisterUsage (..),
    mkRegisterUsage,

    -- ** Request lenses
    ruNonce,
    ruProductCode,
    ruPublicKeyVersion,

    -- * Destructuring the response
    RegisterUsageResponse (..),
    mkRegisterUsageResponse,

    -- ** Response lenses
    rursSignature,
    rursPublicKeyRotationTimestamp,
    rursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterUsage' smart constructor.
data RegisterUsage = RegisterUsage'
  { nonce :: Lude.Maybe Lude.Text,
    productCode :: Lude.Text,
    publicKeyVersion :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterUsage' with the minimum fields required to make a request.
--
-- * 'nonce' - (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
-- * 'productCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
-- * 'publicKeyVersion' - Public Key Version provided by AWS Marketplace
mkRegisterUsage ::
  -- | 'productCode'
  Lude.Text ->
  -- | 'publicKeyVersion'
  Lude.Natural ->
  RegisterUsage
mkRegisterUsage pProductCode_ pPublicKeyVersion_ =
  RegisterUsage'
    { nonce = Lude.Nothing,
      productCode = pProductCode_,
      publicKeyVersion = pPublicKeyVersion_
    }

-- | (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruNonce :: Lens.Lens' RegisterUsage (Lude.Maybe Lude.Text)
ruNonce = Lens.lens (nonce :: RegisterUsage -> Lude.Maybe Lude.Text) (\s a -> s {nonce = a} :: RegisterUsage)
{-# DEPRECATED ruNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruProductCode :: Lens.Lens' RegisterUsage Lude.Text
ruProductCode = Lens.lens (productCode :: RegisterUsage -> Lude.Text) (\s a -> s {productCode = a} :: RegisterUsage)
{-# DEPRECATED ruProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | Public Key Version provided by AWS Marketplace
--
-- /Note:/ Consider using 'publicKeyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruPublicKeyVersion :: Lens.Lens' RegisterUsage Lude.Natural
ruPublicKeyVersion = Lens.lens (publicKeyVersion :: RegisterUsage -> Lude.Natural) (\s a -> s {publicKeyVersion = a} :: RegisterUsage)
{-# DEPRECATED ruPublicKeyVersion "Use generic-lens or generic-optics with 'publicKeyVersion' instead." #-}

instance Lude.AWSRequest RegisterUsage where
  type Rs RegisterUsage = RegisterUsageResponse
  request = Req.postJSON marketplaceMeteringService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterUsageResponse'
            Lude.<$> (x Lude..?> "Signature")
            Lude.<*> (x Lude..?> "PublicKeyRotationTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterUsage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMPMeteringService.RegisterUsage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterUsage where
  toJSON RegisterUsage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Nonce" Lude..=) Lude.<$> nonce,
            Lude.Just ("ProductCode" Lude..= productCode),
            Lude.Just ("PublicKeyVersion" Lude..= publicKeyVersion)
          ]
      )

instance Lude.ToPath RegisterUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterUsage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterUsageResponse' smart constructor.
data RegisterUsageResponse = RegisterUsageResponse'
  { signature ::
      Lude.Maybe Lude.Text,
    publicKeyRotationTimestamp ::
      Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'RegisterUsageResponse' with the minimum fields required to make a request.
--
-- * 'publicKeyRotationTimestamp' - (Optional) Only included when public key version has expired
-- * 'responseStatus' - The response status code.
-- * 'signature' - JWT Token
mkRegisterUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterUsageResponse
mkRegisterUsageResponse pResponseStatus_ =
  RegisterUsageResponse'
    { signature = Lude.Nothing,
      publicKeyRotationTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | JWT Token
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rursSignature :: Lens.Lens' RegisterUsageResponse (Lude.Maybe Lude.Text)
rursSignature = Lens.lens (signature :: RegisterUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: RegisterUsageResponse)
{-# DEPRECATED rursSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | (Optional) Only included when public key version has expired
--
-- /Note:/ Consider using 'publicKeyRotationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rursPublicKeyRotationTimestamp :: Lens.Lens' RegisterUsageResponse (Lude.Maybe Lude.Timestamp)
rursPublicKeyRotationTimestamp = Lens.lens (publicKeyRotationTimestamp :: RegisterUsageResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {publicKeyRotationTimestamp = a} :: RegisterUsageResponse)
{-# DEPRECATED rursPublicKeyRotationTimestamp "Use generic-lens or generic-optics with 'publicKeyRotationTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rursResponseStatus :: Lens.Lens' RegisterUsageResponse Lude.Int
rursResponseStatus = Lens.lens (responseStatus :: RegisterUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterUsageResponse)
{-# DEPRECATED rursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
