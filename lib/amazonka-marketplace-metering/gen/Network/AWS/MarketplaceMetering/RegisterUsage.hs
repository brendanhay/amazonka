{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.MarketplaceMetering.RegisterUsage
    (
    -- * Creating a request
      RegisterUsage (..)
    , mkRegisterUsage
    -- ** Request lenses
    , ruProductCode
    , ruPublicKeyVersion
    , ruNonce

    -- * Destructuring the response
    , RegisterUsageResponse (..)
    , mkRegisterUsageResponse
    -- ** Response lenses
    , rurrsPublicKeyRotationTimestamp
    , rurrsSignature
    , rurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterUsage' smart constructor.
data RegisterUsage = RegisterUsage'
  { productCode :: Types.ProductCode
    -- ^ Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
  , publicKeyVersion :: Core.Natural
    -- ^ Public Key Version provided by AWS Marketplace
  , nonce :: Core.Maybe Types.Nonce
    -- ^ (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterUsage' value with any optional fields omitted.
mkRegisterUsage
    :: Types.ProductCode -- ^ 'productCode'
    -> Core.Natural -- ^ 'publicKeyVersion'
    -> RegisterUsage
mkRegisterUsage productCode publicKeyVersion
  = RegisterUsage'{productCode, publicKeyVersion,
                   nonce = Core.Nothing}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruProductCode :: Lens.Lens' RegisterUsage Types.ProductCode
ruProductCode = Lens.field @"productCode"
{-# INLINEABLE ruProductCode #-}
{-# DEPRECATED productCode "Use generic-lens or generic-optics with 'productCode' instead"  #-}

-- | Public Key Version provided by AWS Marketplace
--
-- /Note:/ Consider using 'publicKeyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruPublicKeyVersion :: Lens.Lens' RegisterUsage Core.Natural
ruPublicKeyVersion = Lens.field @"publicKeyVersion"
{-# INLINEABLE ruPublicKeyVersion #-}
{-# DEPRECATED publicKeyVersion "Use generic-lens or generic-optics with 'publicKeyVersion' instead"  #-}

-- | (Optional) To scope down the registration to a specific running software instance and guard against replay attacks.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruNonce :: Lens.Lens' RegisterUsage (Core.Maybe Types.Nonce)
ruNonce = Lens.field @"nonce"
{-# INLINEABLE ruNonce #-}
{-# DEPRECATED nonce "Use generic-lens or generic-optics with 'nonce' instead"  #-}

instance Core.ToQuery RegisterUsage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterUsage where
        toHeaders RegisterUsage{..}
          = Core.pure ("X-Amz-Target", "AWSMPMeteringService.RegisterUsage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterUsage where
        toJSON RegisterUsage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductCode" Core..= productCode),
                  Core.Just ("PublicKeyVersion" Core..= publicKeyVersion),
                  ("Nonce" Core..=) Core.<$> nonce])

instance Core.AWSRequest RegisterUsage where
        type Rs RegisterUsage = RegisterUsageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterUsageResponse' Core.<$>
                   (x Core..:? "PublicKeyRotationTimestamp") Core.<*>
                     x Core..:? "Signature"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterUsageResponse' smart constructor.
data RegisterUsageResponse = RegisterUsageResponse'
  { publicKeyRotationTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ (Optional) Only included when public key version has expired
  , signature :: Core.Maybe Types.Signature
    -- ^ JWT Token
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'RegisterUsageResponse' value with any optional fields omitted.
mkRegisterUsageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterUsageResponse
mkRegisterUsageResponse responseStatus
  = RegisterUsageResponse'{publicKeyRotationTimestamp = Core.Nothing,
                           signature = Core.Nothing, responseStatus}

-- | (Optional) Only included when public key version has expired
--
-- /Note:/ Consider using 'publicKeyRotationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rurrsPublicKeyRotationTimestamp :: Lens.Lens' RegisterUsageResponse (Core.Maybe Core.NominalDiffTime)
rurrsPublicKeyRotationTimestamp = Lens.field @"publicKeyRotationTimestamp"
{-# INLINEABLE rurrsPublicKeyRotationTimestamp #-}
{-# DEPRECATED publicKeyRotationTimestamp "Use generic-lens or generic-optics with 'publicKeyRotationTimestamp' instead"  #-}

-- | JWT Token
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rurrsSignature :: Lens.Lens' RegisterUsageResponse (Core.Maybe Types.Signature)
rurrsSignature = Lens.field @"signature"
{-# INLINEABLE rurrsSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rurrsResponseStatus :: Lens.Lens' RegisterUsageResponse Core.Int
rurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
