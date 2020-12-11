-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceCounts
  ( EC2InstanceCounts (..),

    -- * Smart constructor
    mkEC2InstanceCounts,

    -- * Lenses
    eicIdLE,
    eicTERMINATING,
    eicPENDING,
    eicMAXIMUM,
    eicDESIRED,
    eicMINIMUM,
    eicACTIVE,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an 'UpdateFleetCapacity' request, or if access to resources is temporarily affected.
--
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- /See:/ 'mkEC2InstanceCounts' smart constructor.
data EC2InstanceCounts = EC2InstanceCounts'
  { idLE ::
      Lude.Maybe Lude.Natural,
    tERMINATING :: Lude.Maybe Lude.Natural,
    pENDING :: Lude.Maybe Lude.Natural,
    mAXIMUM :: Lude.Maybe Lude.Natural,
    dESIRED :: Lude.Maybe Lude.Natural,
    mINIMUM :: Lude.Maybe Lude.Natural,
    aCTIVE :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2InstanceCounts' with the minimum fields required to make a request.
--
-- * 'aCTIVE' - Actual number of active instances in the fleet.
-- * 'dESIRED' - Ideal number of active instances in the fleet.
-- * 'idLE' - Number of active instances in the fleet that are not currently hosting a game session.
-- * 'mAXIMUM' - The maximum value allowed for the fleet's instance count.
-- * 'mINIMUM' - The minimum value allowed for the fleet's instance count.
-- * 'pENDING' - Number of instances in the fleet that are starting but not yet active.
-- * 'tERMINATING' - Number of instances in the fleet that are no longer active but haven't yet been terminated.
mkEC2InstanceCounts ::
  EC2InstanceCounts
mkEC2InstanceCounts =
  EC2InstanceCounts'
    { idLE = Lude.Nothing,
      tERMINATING = Lude.Nothing,
      pENDING = Lude.Nothing,
      mAXIMUM = Lude.Nothing,
      dESIRED = Lude.Nothing,
      mINIMUM = Lude.Nothing,
      aCTIVE = Lude.Nothing
    }

-- | Number of active instances in the fleet that are not currently hosting a game session.
--
-- /Note:/ Consider using 'idLE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicIdLE :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicIdLE = Lens.lens (idLE :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {idLE = a} :: EC2InstanceCounts)
{-# DEPRECATED eicIdLE "Use generic-lens or generic-optics with 'idLE' instead." #-}

-- | Number of instances in the fleet that are no longer active but haven't yet been terminated.
--
-- /Note:/ Consider using 'tERMINATING' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicTERMINATING :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicTERMINATING = Lens.lens (tERMINATING :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {tERMINATING = a} :: EC2InstanceCounts)
{-# DEPRECATED eicTERMINATING "Use generic-lens or generic-optics with 'tERMINATING' instead." #-}

-- | Number of instances in the fleet that are starting but not yet active.
--
-- /Note:/ Consider using 'pENDING' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicPENDING :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicPENDING = Lens.lens (pENDING :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {pENDING = a} :: EC2InstanceCounts)
{-# DEPRECATED eicPENDING "Use generic-lens or generic-optics with 'pENDING' instead." #-}

-- | The maximum value allowed for the fleet's instance count.
--
-- /Note:/ Consider using 'mAXIMUM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicMAXIMUM :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicMAXIMUM = Lens.lens (mAXIMUM :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {mAXIMUM = a} :: EC2InstanceCounts)
{-# DEPRECATED eicMAXIMUM "Use generic-lens or generic-optics with 'mAXIMUM' instead." #-}

-- | Ideal number of active instances in the fleet.
--
-- /Note:/ Consider using 'dESIRED' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicDESIRED :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicDESIRED = Lens.lens (dESIRED :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {dESIRED = a} :: EC2InstanceCounts)
{-# DEPRECATED eicDESIRED "Use generic-lens or generic-optics with 'dESIRED' instead." #-}

-- | The minimum value allowed for the fleet's instance count.
--
-- /Note:/ Consider using 'mINIMUM' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicMINIMUM :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicMINIMUM = Lens.lens (mINIMUM :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {mINIMUM = a} :: EC2InstanceCounts)
{-# DEPRECATED eicMINIMUM "Use generic-lens or generic-optics with 'mINIMUM' instead." #-}

-- | Actual number of active instances in the fleet.
--
-- /Note:/ Consider using 'aCTIVE' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eicACTIVE :: Lens.Lens' EC2InstanceCounts (Lude.Maybe Lude.Natural)
eicACTIVE = Lens.lens (aCTIVE :: EC2InstanceCounts -> Lude.Maybe Lude.Natural) (\s a -> s {aCTIVE = a} :: EC2InstanceCounts)
{-# DEPRECATED eicACTIVE "Use generic-lens or generic-optics with 'aCTIVE' instead." #-}

instance Lude.FromJSON EC2InstanceCounts where
  parseJSON =
    Lude.withObject
      "EC2InstanceCounts"
      ( \x ->
          EC2InstanceCounts'
            Lude.<$> (x Lude..:? "IDLE")
            Lude.<*> (x Lude..:? "TERMINATING")
            Lude.<*> (x Lude..:? "PENDING")
            Lude.<*> (x Lude..:? "MAXIMUM")
            Lude.<*> (x Lude..:? "DESIRED")
            Lude.<*> (x Lude..:? "MINIMUM")
            Lude.<*> (x Lude..:? "ACTIVE")
      )
