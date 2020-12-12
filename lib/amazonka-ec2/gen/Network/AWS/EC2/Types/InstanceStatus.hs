{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatus
  ( InstanceStatus (..),

    -- * Smart constructor
    mkInstanceStatus,

    -- * Lenses
    iInstanceId,
    iOutpostARN,
    iSystemStatus,
    iEvents,
    iAvailabilityZone,
    iInstanceStatus,
    iInstanceState,
  )
where

import Network.AWS.EC2.Types.InstanceState
import Network.AWS.EC2.Types.InstanceStatusEvent
import Network.AWS.EC2.Types.InstanceStatusSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of an instance.
--
-- /See:/ 'mkInstanceStatus' smart constructor.
data InstanceStatus = InstanceStatus'
  { instanceId ::
      Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    systemStatus :: Lude.Maybe InstanceStatusSummary,
    events :: Lude.Maybe [InstanceStatusEvent],
    availabilityZone :: Lude.Maybe Lude.Text,
    instanceStatus :: Lude.Maybe InstanceStatusSummary,
    instanceState :: Lude.Maybe InstanceState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStatus' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone of the instance.
-- * 'events' - Any scheduled events associated with the instance.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceState' - The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
-- * 'instanceStatus' - Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'systemStatus' - Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
mkInstanceStatus ::
  InstanceStatus
mkInstanceStatus =
  InstanceStatus'
    { instanceId = Lude.Nothing,
      outpostARN = Lude.Nothing,
      systemStatus = Lude.Nothing,
      events = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      instanceStatus = Lude.Nothing,
      instanceState = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceId :: Lens.Lens' InstanceStatus (Lude.Maybe Lude.Text)
iInstanceId = Lens.lens (instanceId :: InstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceStatus)
{-# DEPRECATED iInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iOutpostARN :: Lens.Lens' InstanceStatus (Lude.Maybe Lude.Text)
iOutpostARN = Lens.lens (outpostARN :: InstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: InstanceStatus)
{-# DEPRECATED iOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Reports impaired functionality that stems from issues related to the systems that support an instance, such as hardware failures and network connectivity problems.
--
-- /Note:/ Consider using 'systemStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSystemStatus :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceStatusSummary)
iSystemStatus = Lens.lens (systemStatus :: InstanceStatus -> Lude.Maybe InstanceStatusSummary) (\s a -> s {systemStatus = a} :: InstanceStatus)
{-# DEPRECATED iSystemStatus "Use generic-lens or generic-optics with 'systemStatus' instead." #-}

-- | Any scheduled events associated with the instance.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEvents :: Lens.Lens' InstanceStatus (Lude.Maybe [InstanceStatusEvent])
iEvents = Lens.lens (events :: InstanceStatus -> Lude.Maybe [InstanceStatusEvent]) (\s a -> s {events = a} :: InstanceStatus)
{-# DEPRECATED iEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The Availability Zone of the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAvailabilityZone :: Lens.Lens' InstanceStatus (Lude.Maybe Lude.Text)
iAvailabilityZone = Lens.lens (availabilityZone :: InstanceStatus -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: InstanceStatus)
{-# DEPRECATED iAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Reports impaired functionality that stems from issues internal to the instance, such as impaired reachability.
--
-- /Note:/ Consider using 'instanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceStatus :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceStatusSummary)
iInstanceStatus = Lens.lens (instanceStatus :: InstanceStatus -> Lude.Maybe InstanceStatusSummary) (\s a -> s {instanceStatus = a} :: InstanceStatus)
{-# DEPRECATED iInstanceStatus "Use generic-lens or generic-optics with 'instanceStatus' instead." #-}

-- | The intended state of the instance. 'DescribeInstanceStatus' requires that an instance be in the @running@ state.
--
-- /Note:/ Consider using 'instanceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceState :: Lens.Lens' InstanceStatus (Lude.Maybe InstanceState)
iInstanceState = Lens.lens (instanceState :: InstanceStatus -> Lude.Maybe InstanceState) (\s a -> s {instanceState = a} :: InstanceStatus)
{-# DEPRECATED iInstanceState "Use generic-lens or generic-optics with 'instanceState' instead." #-}

instance Lude.FromXML InstanceStatus where
  parseXML x =
    InstanceStatus'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "systemStatus")
      Lude.<*> ( x Lude..@? "eventsSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "instanceStatus")
      Lude.<*> (x Lude..@? "instanceState")
