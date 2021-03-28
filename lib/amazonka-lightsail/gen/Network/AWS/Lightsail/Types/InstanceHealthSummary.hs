{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceHealthSummary
  ( InstanceHealthSummary (..)
  -- * Smart constructor
  , mkInstanceHealthSummary
  -- * Lenses
  , ihsInstanceHealth
  , ihsInstanceHealthReason
  , ihsInstanceName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.InstanceHealthReason as Types
import qualified Network.AWS.Lightsail.Types.InstanceHealthState as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | Describes information about the health of the instance.
--
-- /See:/ 'mkInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { instanceHealth :: Core.Maybe Types.InstanceHealthState
    -- ^ Describes the overall instance health. Valid values are below.
  , instanceHealthReason :: Core.Maybe Types.InstanceHealthReason
    -- ^ More information about the instance health. If the @instanceHealth@ is @healthy@ , then an @instanceHealthReason@ value is not provided.
--
-- If __@instanceHealth@ __ is @initial@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Lb.RegistrationInProgress@ __ - The target instance is in the process of being registered with the load balancer.
--
--
--     * __@Lb.InitialHealthChecking@ __ - The Lightsail load balancer is still sending the target instance the minimum number of health checks required to determine its health status.
--
--
-- If __@instanceHealth@ __ is @unhealthy@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.ResponseCodeMismatch@ __ - The health checks did not return an expected HTTP code.
--
--
--     * __@Instance.Timeout@ __ - The health check requests timed out.
--
--
--     * __@Instance.FailedHealthChecks@ __ - The health checks failed because the connection to the target instance timed out, the target instance response was malformed, or the target instance failed the health check for an unknown reason.
--
--
--     * __@Lb.InternalError@ __ - The health checks failed due to an internal error.
--
--
-- If __@instanceHealth@ __ is @unused@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.NotRegistered@ __ - The target instance is not registered with the target group.
--
--
--     * __@Instance.NotInUse@ __ - The target group is not used by any load balancer, or the target instance is in an Availability Zone that is not enabled for its load balancer.
--
--
--     * __@Instance.IpUnusable@ __ - The target IP address is reserved for use by a Lightsail load balancer.
--
--
--     * __@Instance.InvalidState@ __ - The target is in the stopped or terminated state.
--
--
-- If __@instanceHealth@ __ is @draining@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.DeregistrationInProgress@ __ - The target instance is in the process of being deregistered and the deregistration delay period has not expired.
--
--
  , instanceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Lightsail instance for which you are requesting health check data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceHealthSummary' value with any optional fields omitted.
mkInstanceHealthSummary
    :: InstanceHealthSummary
mkInstanceHealthSummary
  = InstanceHealthSummary'{instanceHealth = Core.Nothing,
                           instanceHealthReason = Core.Nothing, instanceName = Core.Nothing}

-- | Describes the overall instance health. Valid values are below.
--
-- /Note:/ Consider using 'instanceHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInstanceHealth :: Lens.Lens' InstanceHealthSummary (Core.Maybe Types.InstanceHealthState)
ihsInstanceHealth = Lens.field @"instanceHealth"
{-# INLINEABLE ihsInstanceHealth #-}
{-# DEPRECATED instanceHealth "Use generic-lens or generic-optics with 'instanceHealth' instead"  #-}

-- | More information about the instance health. If the @instanceHealth@ is @healthy@ , then an @instanceHealthReason@ value is not provided.
--
-- If __@instanceHealth@ __ is @initial@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Lb.RegistrationInProgress@ __ - The target instance is in the process of being registered with the load balancer.
--
--
--     * __@Lb.InitialHealthChecking@ __ - The Lightsail load balancer is still sending the target instance the minimum number of health checks required to determine its health status.
--
--
-- If __@instanceHealth@ __ is @unhealthy@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.ResponseCodeMismatch@ __ - The health checks did not return an expected HTTP code.
--
--
--     * __@Instance.Timeout@ __ - The health check requests timed out.
--
--
--     * __@Instance.FailedHealthChecks@ __ - The health checks failed because the connection to the target instance timed out, the target instance response was malformed, or the target instance failed the health check for an unknown reason.
--
--
--     * __@Lb.InternalError@ __ - The health checks failed due to an internal error.
--
--
-- If __@instanceHealth@ __ is @unused@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.NotRegistered@ __ - The target instance is not registered with the target group.
--
--
--     * __@Instance.NotInUse@ __ - The target group is not used by any load balancer, or the target instance is in an Availability Zone that is not enabled for its load balancer.
--
--
--     * __@Instance.IpUnusable@ __ - The target IP address is reserved for use by a Lightsail load balancer.
--
--
--     * __@Instance.InvalidState@ __ - The target is in the stopped or terminated state.
--
--
-- If __@instanceHealth@ __ is @draining@ , the __@instanceHealthReason@ __ value can be one of the following:
--
--     * __@Instance.DeregistrationInProgress@ __ - The target instance is in the process of being deregistered and the deregistration delay period has not expired.
--
--
--
-- /Note:/ Consider using 'instanceHealthReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInstanceHealthReason :: Lens.Lens' InstanceHealthSummary (Core.Maybe Types.InstanceHealthReason)
ihsInstanceHealthReason = Lens.field @"instanceHealthReason"
{-# INLINEABLE ihsInstanceHealthReason #-}
{-# DEPRECATED instanceHealthReason "Use generic-lens or generic-optics with 'instanceHealthReason' instead"  #-}

-- | The name of the Lightsail instance for which you are requesting health check data.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInstanceName :: Lens.Lens' InstanceHealthSummary (Core.Maybe Types.ResourceName)
ihsInstanceName = Lens.field @"instanceName"
{-# INLINEABLE ihsInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.FromJSON InstanceHealthSummary where
        parseJSON
          = Core.withObject "InstanceHealthSummary" Core.$
              \ x ->
                InstanceHealthSummary' Core.<$>
                  (x Core..:? "instanceHealth") Core.<*>
                    x Core..:? "instanceHealthReason"
                    Core.<*> x Core..:? "instanceName"
