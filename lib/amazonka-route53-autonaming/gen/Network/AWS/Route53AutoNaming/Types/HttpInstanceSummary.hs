{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.HttpInstanceSummary
  ( HttpInstanceSummary (..)
  -- * Smart constructor
  , mkHttpInstanceSummary
  -- * Lenses
  , hisAttributes
  , hisHealthStatus
  , hisInstanceId
  , hisNamespaceName
  , hisServiceName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.AttrKey as Types
import qualified Network.AWS.Route53AutoNaming.Types.AttrValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.HealthStatus as Types
import qualified Network.AWS.Route53AutoNaming.Types.NamespaceName as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types
import qualified Network.AWS.Route53AutoNaming.Types.ServiceName as Types

-- | In a response to a <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances> request, @HttpInstanceSummary@ contains information about one instance that matches the values that you specified in the request.
--
-- /See:/ 'mkHttpInstanceSummary' smart constructor.
data HttpInstanceSummary = HttpInstanceSummary'
  { attributes :: Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue)
    -- ^ If you included any attributes when you registered the instance, the values of those attributes.
  , healthStatus :: Core.Maybe Types.HealthStatus
    -- ^ If you configured health checking in the service, the current health status of the service instance.
  , instanceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of an instance that matches the values that you specified in the request.
  , namespaceName :: Core.Maybe Types.NamespaceName
    -- ^ The name of the namespace that you specified when you registered the instance.
  , serviceName :: Core.Maybe Types.ServiceName
    -- ^ The name of the service that you specified when you registered the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpInstanceSummary' value with any optional fields omitted.
mkHttpInstanceSummary
    :: HttpInstanceSummary
mkHttpInstanceSummary
  = HttpInstanceSummary'{attributes = Core.Nothing,
                         healthStatus = Core.Nothing, instanceId = Core.Nothing,
                         namespaceName = Core.Nothing, serviceName = Core.Nothing}

-- | If you included any attributes when you registered the instance, the values of those attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisAttributes :: Lens.Lens' HttpInstanceSummary (Core.Maybe (Core.HashMap Types.AttrKey Types.AttrValue))
hisAttributes = Lens.field @"attributes"
{-# INLINEABLE hisAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | If you configured health checking in the service, the current health status of the service instance.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisHealthStatus :: Lens.Lens' HttpInstanceSummary (Core.Maybe Types.HealthStatus)
hisHealthStatus = Lens.field @"healthStatus"
{-# INLINEABLE hisHealthStatus #-}
{-# DEPRECATED healthStatus "Use generic-lens or generic-optics with 'healthStatus' instead"  #-}

-- | The ID of an instance that matches the values that you specified in the request.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisInstanceId :: Lens.Lens' HttpInstanceSummary (Core.Maybe Types.ResourceId)
hisInstanceId = Lens.field @"instanceId"
{-# INLINEABLE hisInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the namespace that you specified when you registered the instance.
--
-- /Note:/ Consider using 'namespaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisNamespaceName :: Lens.Lens' HttpInstanceSummary (Core.Maybe Types.NamespaceName)
hisNamespaceName = Lens.field @"namespaceName"
{-# INLINEABLE hisNamespaceName #-}
{-# DEPRECATED namespaceName "Use generic-lens or generic-optics with 'namespaceName' instead"  #-}

-- | The name of the service that you specified when you registered the instance.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hisServiceName :: Lens.Lens' HttpInstanceSummary (Core.Maybe Types.ServiceName)
hisServiceName = Lens.field @"serviceName"
{-# INLINEABLE hisServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

instance Core.FromJSON HttpInstanceSummary where
        parseJSON
          = Core.withObject "HttpInstanceSummary" Core.$
              \ x ->
                HttpInstanceSummary' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "HealthStatus"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "NamespaceName"
                    Core.<*> x Core..:? "ServiceName"
