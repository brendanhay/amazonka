{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachCertificateFromDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an SSL/TLS certificate from your Amazon Lightsail content delivery network (CDN) distribution.
--
-- After the certificate is detached, your distribution stops accepting traffic for all of the domains that are associated with the certificate.
module Network.AWS.Lightsail.DetachCertificateFromDistribution
    (
    -- * Creating a request
      DetachCertificateFromDistribution (..)
    , mkDetachCertificateFromDistribution
    -- ** Request lenses
    , dcfdDistributionName

    -- * Destructuring the response
    , DetachCertificateFromDistributionResponse (..)
    , mkDetachCertificateFromDistributionResponse
    -- ** Response lenses
    , dcfdrrsOperation
    , dcfdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachCertificateFromDistribution' smart constructor.
newtype DetachCertificateFromDistribution = DetachCertificateFromDistribution'
  { distributionName :: Types.DistributionName
    -- ^ The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachCertificateFromDistribution' value with any optional fields omitted.
mkDetachCertificateFromDistribution
    :: Types.DistributionName -- ^ 'distributionName'
    -> DetachCertificateFromDistribution
mkDetachCertificateFromDistribution distributionName
  = DetachCertificateFromDistribution'{distributionName}

-- | The name of the distribution from which to detach the certificate.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdDistributionName :: Lens.Lens' DetachCertificateFromDistribution Types.DistributionName
dcfdDistributionName = Lens.field @"distributionName"
{-# INLINEABLE dcfdDistributionName #-}
{-# DEPRECATED distributionName "Use generic-lens or generic-optics with 'distributionName' instead"  #-}

instance Core.ToQuery DetachCertificateFromDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetachCertificateFromDistribution where
        toHeaders DetachCertificateFromDistribution{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.DetachCertificateFromDistribution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetachCertificateFromDistribution where
        toJSON DetachCertificateFromDistribution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("distributionName" Core..= distributionName)])

instance Core.AWSRequest DetachCertificateFromDistribution where
        type Rs DetachCertificateFromDistribution =
             DetachCertificateFromDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetachCertificateFromDistributionResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachCertificateFromDistributionResponse' smart constructor.
data DetachCertificateFromDistributionResponse = DetachCertificateFromDistributionResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DetachCertificateFromDistributionResponse' value with any optional fields omitted.
mkDetachCertificateFromDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachCertificateFromDistributionResponse
mkDetachCertificateFromDistributionResponse responseStatus
  = DetachCertificateFromDistributionResponse'{operation =
                                                 Core.Nothing,
                                               responseStatus}

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdrrsOperation :: Lens.Lens' DetachCertificateFromDistributionResponse (Core.Maybe Types.Operation)
dcfdrrsOperation = Lens.field @"operation"
{-# INLINEABLE dcfdrrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfdrrsResponseStatus :: Lens.Lens' DetachCertificateFromDistributionResponse Core.Int
dcfdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcfdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
