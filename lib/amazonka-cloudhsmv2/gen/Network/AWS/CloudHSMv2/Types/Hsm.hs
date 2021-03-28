{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Hsm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSMv2.Types.Hsm
  ( Hsm (..)
  -- * Smart constructor
  , mkHsm
  -- * Lenses
  , hHsmId
  , hAvailabilityZone
  , hClusterId
  , hEniId
  , hEniIp
  , hState
  , hStateMessage
  , hSubnetId
  ) where

import qualified Network.AWS.CloudHSMv2.Types.AvailabilityZone as Types
import qualified Network.AWS.CloudHSMv2.Types.ClusterId as Types
import qualified Network.AWS.CloudHSMv2.Types.EniId as Types
import qualified Network.AWS.CloudHSMv2.Types.HsmId as Types
import qualified Network.AWS.CloudHSMv2.Types.HsmState as Types
import qualified Network.AWS.CloudHSMv2.Types.IpAddress as Types
import qualified Network.AWS.CloudHSMv2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a hardware security module (HSM) in an AWS CloudHSM cluster.
--
-- /See:/ 'mkHsm' smart constructor.
data Hsm = Hsm'
  { hsmId :: Types.HsmId
    -- ^ The HSM's identifier (ID).
  , availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone that contains the HSM.
  , clusterId :: Core.Maybe Types.ClusterId
    -- ^ The identifier (ID) of the cluster that contains the HSM.
  , eniId :: Core.Maybe Types.EniId
    -- ^ The identifier (ID) of the HSM's elastic network interface (ENI).
  , eniIp :: Core.Maybe Types.IpAddress
    -- ^ The IP address of the HSM's elastic network interface (ENI).
  , state :: Core.Maybe Types.HsmState
    -- ^ The HSM's state.
  , stateMessage :: Core.Maybe Core.Text
    -- ^ A description of the HSM's state.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The subnet that contains the HSM's elastic network interface (ENI).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Hsm' value with any optional fields omitted.
mkHsm
    :: Types.HsmId -- ^ 'hsmId'
    -> Hsm
mkHsm hsmId
  = Hsm'{hsmId, availabilityZone = Core.Nothing,
         clusterId = Core.Nothing, eniId = Core.Nothing,
         eniIp = Core.Nothing, state = Core.Nothing,
         stateMessage = Core.Nothing, subnetId = Core.Nothing}

-- | The HSM's identifier (ID).
--
-- /Note:/ Consider using 'hsmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHsmId :: Lens.Lens' Hsm Types.HsmId
hHsmId = Lens.field @"hsmId"
{-# INLINEABLE hHsmId #-}
{-# DEPRECATED hsmId "Use generic-lens or generic-optics with 'hsmId' instead"  #-}

-- | The Availability Zone that contains the HSM.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hAvailabilityZone :: Lens.Lens' Hsm (Core.Maybe Types.AvailabilityZone)
hAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE hAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The identifier (ID) of the cluster that contains the HSM.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hClusterId :: Lens.Lens' Hsm (Core.Maybe Types.ClusterId)
hClusterId = Lens.field @"clusterId"
{-# INLINEABLE hClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The identifier (ID) of the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hEniId :: Lens.Lens' Hsm (Core.Maybe Types.EniId)
hEniId = Lens.field @"eniId"
{-# INLINEABLE hEniId #-}
{-# DEPRECATED eniId "Use generic-lens or generic-optics with 'eniId' instead"  #-}

-- | The IP address of the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'eniIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hEniIp :: Lens.Lens' Hsm (Core.Maybe Types.IpAddress)
hEniIp = Lens.field @"eniIp"
{-# INLINEABLE hEniIp #-}
{-# DEPRECATED eniIp "Use generic-lens or generic-optics with 'eniIp' instead"  #-}

-- | The HSM's state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hState :: Lens.Lens' Hsm (Core.Maybe Types.HsmState)
hState = Lens.field @"state"
{-# INLINEABLE hState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A description of the HSM's state.
--
-- /Note:/ Consider using 'stateMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hStateMessage :: Lens.Lens' Hsm (Core.Maybe Core.Text)
hStateMessage = Lens.field @"stateMessage"
{-# INLINEABLE hStateMessage #-}
{-# DEPRECATED stateMessage "Use generic-lens or generic-optics with 'stateMessage' instead"  #-}

-- | The subnet that contains the HSM's elastic network interface (ENI).
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hSubnetId :: Lens.Lens' Hsm (Core.Maybe Types.SubnetId)
hSubnetId = Lens.field @"subnetId"
{-# INLINEABLE hSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.FromJSON Hsm where
        parseJSON
          = Core.withObject "Hsm" Core.$
              \ x ->
                Hsm' Core.<$>
                  (x Core..: "HsmId") Core.<*> x Core..:? "AvailabilityZone" Core.<*>
                    x Core..:? "ClusterId"
                    Core.<*> x Core..:? "EniId"
                    Core.<*> x Core..:? "EniIp"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateMessage"
                    Core.<*> x Core..:? "SubnetId"
