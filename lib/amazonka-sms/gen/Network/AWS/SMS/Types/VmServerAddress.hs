{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VmServerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.VmServerAddress
  ( VmServerAddress (..)
  -- * Smart constructor
  , mkVmServerAddress
  -- * Lenses
  , vsaVmId
  , vsaVmManagerId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.VmId as Types
import qualified Network.AWS.SMS.Types.VmManagerId as Types

-- | Represents a VM server location.
--
-- /See:/ 'mkVmServerAddress' smart constructor.
data VmServerAddress = VmServerAddress'
  { vmId :: Core.Maybe Types.VmId
    -- ^ The ID of the VM.
  , vmManagerId :: Core.Maybe Types.VmManagerId
    -- ^ The ID of the VM manager.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VmServerAddress' value with any optional fields omitted.
mkVmServerAddress
    :: VmServerAddress
mkVmServerAddress
  = VmServerAddress'{vmId = Core.Nothing, vmManagerId = Core.Nothing}

-- | The ID of the VM.
--
-- /Note:/ Consider using 'vmId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaVmId :: Lens.Lens' VmServerAddress (Core.Maybe Types.VmId)
vsaVmId = Lens.field @"vmId"
{-# INLINEABLE vsaVmId #-}
{-# DEPRECATED vmId "Use generic-lens or generic-optics with 'vmId' instead"  #-}

-- | The ID of the VM manager.
--
-- /Note:/ Consider using 'vmManagerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsaVmManagerId :: Lens.Lens' VmServerAddress (Core.Maybe Types.VmManagerId)
vsaVmManagerId = Lens.field @"vmManagerId"
{-# INLINEABLE vsaVmManagerId #-}
{-# DEPRECATED vmManagerId "Use generic-lens or generic-optics with 'vmManagerId' instead"  #-}

instance Core.FromJSON VmServerAddress where
        toJSON VmServerAddress{..}
          = Core.object
              (Core.catMaybes
                 [("vmId" Core..=) Core.<$> vmId,
                  ("vmManagerId" Core..=) Core.<$> vmManagerId])

instance Core.FromJSON VmServerAddress where
        parseJSON
          = Core.withObject "VmServerAddress" Core.$
              \ x ->
                VmServerAddress' Core.<$>
                  (x Core..:? "vmId") Core.<*> x Core..:? "vmManagerId"
