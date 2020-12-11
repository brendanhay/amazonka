-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.InstanceState
  ( InstanceState (..),

    -- * Smart constructor
    mkInstanceState,

    -- * Lenses
    isInstanceId,
    isState,
    isReasonCode,
    isDescription,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the state of an EC2 instance.
--
-- /See:/ 'mkInstanceState' smart constructor.
data InstanceState = InstanceState'
  { instanceId ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe Lude.Text,
    reasonCode :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceState' with the minimum fields required to make a request.
--
-- * 'description' - A description of the instance state. This string can contain one or more of the following messages.
--
--
--     * @N/A@
--
--
--     * @A transient error occurred. Please try again later.@
--
--
--     * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance registration is still in progress.@
--
--
--     * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
--
--     * @Instance is not currently registered with the LoadBalancer.@
--
--
--     * @Instance deregistration currently in progress.@
--
--
--     * @Disable Availability Zone is currently in progress.@
--
--
--     * @Instance is in pending state.@
--
--
--     * @Instance is in stopped state.@
--
--
--     * @Instance is in terminated state.@
--
--
-- * 'instanceId' - The ID of the instance.
-- * 'reasonCode' - Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N/A@
-- * 'state' - The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
mkInstanceState ::
  InstanceState
mkInstanceState =
  InstanceState'
    { instanceId = Lude.Nothing,
      state = Lude.Nothing,
      reasonCode = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceId :: Lens.Lens' InstanceState (Lude.Maybe Lude.Text)
isInstanceId = Lens.lens (instanceId :: InstanceState -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceState)
{-# DEPRECATED isInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The current state of the instance.
--
-- Valid values: @InService@ | @OutOfService@ | @Unknown@
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceState (Lude.Maybe Lude.Text)
isState = Lens.lens (state :: InstanceState -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: InstanceState)
{-# DEPRECATED isState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the cause of @OutOfService@ instances. Specifically, whether the cause is Elastic Load Balancing or the instance.
--
-- Valid values: @ELB@ | @Instance@ | @N/A@
--
-- /Note:/ Consider using 'reasonCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isReasonCode :: Lens.Lens' InstanceState (Lude.Maybe Lude.Text)
isReasonCode = Lens.lens (reasonCode :: InstanceState -> Lude.Maybe Lude.Text) (\s a -> s {reasonCode = a} :: InstanceState)
{-# DEPRECATED isReasonCode "Use generic-lens or generic-optics with 'reasonCode' instead." #-}

-- | A description of the instance state. This string can contain one or more of the following messages.
--
--
--     * @N/A@
--
--
--     * @A transient error occurred. Please try again later.@
--
--
--     * @Instance has failed at least the UnhealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance has not passed the configured HealthyThreshold number of health checks consecutively.@
--
--
--     * @Instance registration is still in progress.@
--
--
--     * @Instance is in the EC2 Availability Zone for which LoadBalancer is not configured to route traffic to.@
--
--
--     * @Instance is not currently registered with the LoadBalancer.@
--
--
--     * @Instance deregistration currently in progress.@
--
--
--     * @Disable Availability Zone is currently in progress.@
--
--
--     * @Instance is in pending state.@
--
--
--     * @Instance is in stopped state.@
--
--
--     * @Instance is in terminated state.@
--
--
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isDescription :: Lens.Lens' InstanceState (Lude.Maybe Lude.Text)
isDescription = Lens.lens (description :: InstanceState -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: InstanceState)
{-# DEPRECATED isDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML InstanceState where
  parseXML x =
    InstanceState'
      Lude.<$> (x Lude..@? "InstanceId")
      Lude.<*> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "ReasonCode")
      Lude.<*> (x Lude..@? "Description")
