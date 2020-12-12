{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.HSM
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.HSM
  ( HSM (..),

    -- * Smart constructor
    mkHSM,

    -- * Lenses
    hsmStateMessage,
    hsmState,
    hsmEniId,
    hsmSubnetId,
    hsmAvailabilityZone,
    hsmClusterId,
    hsmEniIP,
    hsmHSMId,
  )
where

import Network.AWS.CloudHSMv2.Types.HSMState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.
--
-- /See:/ 'mkHSM' smart constructor.
data HSM = HSM'
  { stateMessage :: Lude.Maybe Lude.Text,
    state :: Lude.Maybe HSMState,
    eniId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    clusterId :: Lude.Maybe Lude.Text,
    eniIP :: Lude.Maybe Lude.Text,
    hsmId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HSM' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone that contains the HSM.
-- * 'clusterId' - The identifier (ID) of the cluster that contains the HSM.
-- * 'eniIP' - The IP address of the HSM's elastic network interface (ENI).
-- * 'eniId' - The identifier (ID) of the HSM's elastic network interface (ENI).
-- * 'hsmId' - The HSM's identifier (ID).
-- * 'state' - The HSM's state.
-- * 'stateMessage' - A description of the HSM's state.
-- * 'subnetId' - The subnet that contains the HSM's elastic network interface (ENI).
mkHSM ::
  -- | 'hsmId'
  Lude.Text ->
  HSM
mkHSM pHSMId_ =
  HSM'
    { stateMessage = Lude.Nothing,
      state = Lude.Nothing,
      eniId = Lude.Nothing,
      subnetId = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      clusterId = Lude.Nothing,
      eniIP = Lude.Nothing,
      hsmId = pHSMId_
    }

-- | A description of the HSM's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmStateMessage :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmStateMessage = Lens.lens (stateMessage :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {stateMessage = a} :: HSM)
{-# DEPRECATED hsmStateMessage "Use generic-lens or generic-optics with 'stateMessage' instead." #-}

-- | The HSM's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmState :: Lens.Lens' HSM (Lude.Maybe HSMState)
hsmState = Lens.lens (state :: HSM -> Lude.Maybe HSMState) (\s a -> s {state = a} :: HSM)
{-# DEPRECATED hsmState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier (ID) of the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmEniId :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmEniId = Lens.lens (eniId :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {eniId = a} :: HSM)
{-# DEPRECATED hsmEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The subnet that contains the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmSubnetId :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmSubnetId = Lens.lens (subnetId :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: HSM)
{-# DEPRECATED hsmSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Availability Zone that contains the HSM.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmAvailabilityZone :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmAvailabilityZone = Lens.lens (availabilityZone :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: HSM)
{-# DEPRECATED hsmAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The identifier (ID) of the cluster that contains the HSM.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmClusterId :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmClusterId = Lens.lens (clusterId :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: HSM)
{-# DEPRECATED hsmClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The IP address of the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'eniIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmEniIP :: Lens.Lens' HSM (Lude.Maybe Lude.Text)
hsmEniIP = Lens.lens (eniIP :: HSM -> Lude.Maybe Lude.Text) (\s a -> s {eniIP = a} :: HSM)
{-# DEPRECATED hsmEniIP "Use generic-lens or generic-optics with 'eniIP' instead." #-}

-- | The HSM's identifier (ID).
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsmHSMId :: Lens.Lens' HSM Lude.Text
hsmHSMId = Lens.lens (hsmId :: HSM -> Lude.Text) (\s a -> s {hsmId = a} :: HSM)
{-# DEPRECATED hsmHSMId "Use generic-lens or generic-optics with 'hsmId' instead." #-}

instance Lude.FromJSON HSM where
  parseJSON =
    Lude.withObject
      "HSM"
      ( \x ->
          HSM'
            Lude.<$> (x Lude..:? "StateMessage")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "EniId")
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "EniIp")
            Lude.<*> (x Lude..: "HsmId")
      )
