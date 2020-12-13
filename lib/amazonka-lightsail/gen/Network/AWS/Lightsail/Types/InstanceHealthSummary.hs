{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthSummary
  ( InstanceHealthSummary (..),

    -- * Smart constructor
    mkInstanceHealthSummary,

    -- * Lenses
    ihsInstanceHealth,
    ihsInstanceName,
    ihsInstanceHealthReason,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstanceHealthReason
import Network.AWS.Lightsail.Types.InstanceHealthState
import qualified Network.AWS.Prelude as Lude

-- | Describes information about the health of the instance.
--
-- /See:/ 'mkInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { -- | Describes the overall instance health. Valid values are below.
    instanceHealth :: Lude.Maybe InstanceHealthState,
    -- | The name of the Lightsail instance for which you are requesting health check data.
    instanceName :: Lude.Maybe Lude.Text,
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
    instanceHealthReason :: Lude.Maybe InstanceHealthReason
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- * 'instanceHealth' - Describes the overall instance health. Valid values are below.
-- * 'instanceName' - The name of the Lightsail instance for which you are requesting health check data.
-- * 'instanceHealthReason' - More information about the instance health. If the @instanceHealth@ is @healthy@ , then an @instanceHealthReason@ value is not provided.
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
mkInstanceHealthSummary ::
  InstanceHealthSummary
mkInstanceHealthSummary =
  InstanceHealthSummary'
    { instanceHealth = Lude.Nothing,
      instanceName = Lude.Nothing,
      instanceHealthReason = Lude.Nothing
    }

-- | Describes the overall instance health. Valid values are below.
--
-- /Note:/ Consider using 'instanceHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInstanceHealth :: Lens.Lens' InstanceHealthSummary (Lude.Maybe InstanceHealthState)
ihsInstanceHealth = Lens.lens (instanceHealth :: InstanceHealthSummary -> Lude.Maybe InstanceHealthState) (\s a -> s {instanceHealth = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsInstanceHealth "Use generic-lens or generic-optics with 'instanceHealth' instead." #-}

-- | The name of the Lightsail instance for which you are requesting health check data.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInstanceName :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Text)
ihsInstanceName = Lens.lens (instanceName :: InstanceHealthSummary -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

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
ihsInstanceHealthReason :: Lens.Lens' InstanceHealthSummary (Lude.Maybe InstanceHealthReason)
ihsInstanceHealthReason = Lens.lens (instanceHealthReason :: InstanceHealthSummary -> Lude.Maybe InstanceHealthReason) (\s a -> s {instanceHealthReason = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsInstanceHealthReason "Use generic-lens or generic-optics with 'instanceHealthReason' instead." #-}

instance Lude.FromJSON InstanceHealthSummary where
  parseJSON =
    Lude.withObject
      "InstanceHealthSummary"
      ( \x ->
          InstanceHealthSummary'
            Lude.<$> (x Lude..:? "instanceHealth")
            Lude.<*> (x Lude..:? "instanceName")
            Lude.<*> (x Lude..:? "instanceHealthReason")
      )
