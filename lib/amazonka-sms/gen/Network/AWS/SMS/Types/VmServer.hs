{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.VmServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.VmServer
  ( VmServer (..)
  -- * Smart constructor
  , mkVmServer
  -- * Lenses
  , vsVmManagerName
  , vsVmManagerType
  , vsVmName
  , vsVmPath
  , vsVmServerAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.VmManagerName as Types
import qualified Network.AWS.SMS.Types.VmManagerType as Types
import qualified Network.AWS.SMS.Types.VmName as Types
import qualified Network.AWS.SMS.Types.VmPath as Types
import qualified Network.AWS.SMS.Types.VmServerAddress as Types

-- | Represents a VM server.
--
-- /See:/ 'mkVmServer' smart constructor.
data VmServer = VmServer'
  { vmManagerName :: Core.Maybe Types.VmManagerName
    -- ^ The name of the VM manager.
  , vmManagerType :: Core.Maybe Types.VmManagerType
    -- ^ The type of VM management product.
  , vmName :: Core.Maybe Types.VmName
    -- ^ The name of the VM.
  , vmPath :: Core.Maybe Types.VmPath
    -- ^ The VM folder path in the vCenter Server virtual machine inventory tree.
  , vmServerAddress :: Core.Maybe Types.VmServerAddress
    -- ^ The VM server location.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VmServer' value with any optional fields omitted.
mkVmServer
    :: VmServer
mkVmServer
  = VmServer'{vmManagerName = Core.Nothing,
              vmManagerType = Core.Nothing, vmName = Core.Nothing,
              vmPath = Core.Nothing, vmServerAddress = Core.Nothing}

-- | The name of the VM manager.
--
-- /Note:/ Consider using 'vmManagerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVmManagerName :: Lens.Lens' VmServer (Core.Maybe Types.VmManagerName)
vsVmManagerName = Lens.field @"vmManagerName"
{-# INLINEABLE vsVmManagerName #-}
{-# DEPRECATED vmManagerName "Use generic-lens or generic-optics with 'vmManagerName' instead"  #-}

-- | The type of VM management product.
--
-- /Note:/ Consider using 'vmManagerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVmManagerType :: Lens.Lens' VmServer (Core.Maybe Types.VmManagerType)
vsVmManagerType = Lens.field @"vmManagerType"
{-# INLINEABLE vsVmManagerType #-}
{-# DEPRECATED vmManagerType "Use generic-lens or generic-optics with 'vmManagerType' instead"  #-}

-- | The name of the VM.
--
-- /Note:/ Consider using 'vmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVmName :: Lens.Lens' VmServer (Core.Maybe Types.VmName)
vsVmName = Lens.field @"vmName"
{-# INLINEABLE vsVmName #-}
{-# DEPRECATED vmName "Use generic-lens or generic-optics with 'vmName' instead"  #-}

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
--
-- /Note:/ Consider using 'vmPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVmPath :: Lens.Lens' VmServer (Core.Maybe Types.VmPath)
vsVmPath = Lens.field @"vmPath"
{-# INLINEABLE vsVmPath #-}
{-# DEPRECATED vmPath "Use generic-lens or generic-optics with 'vmPath' instead"  #-}

-- | The VM server location.
--
-- /Note:/ Consider using 'vmServerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsVmServerAddress :: Lens.Lens' VmServer (Core.Maybe Types.VmServerAddress)
vsVmServerAddress = Lens.field @"vmServerAddress"
{-# INLINEABLE vsVmServerAddress #-}
{-# DEPRECATED vmServerAddress "Use generic-lens or generic-optics with 'vmServerAddress' instead"  #-}

instance Core.FromJSON VmServer where
        toJSON VmServer{..}
          = Core.object
              (Core.catMaybes
                 [("vmManagerName" Core..=) Core.<$> vmManagerName,
                  ("vmManagerType" Core..=) Core.<$> vmManagerType,
                  ("vmName" Core..=) Core.<$> vmName,
                  ("vmPath" Core..=) Core.<$> vmPath,
                  ("vmServerAddress" Core..=) Core.<$> vmServerAddress])

instance Core.FromJSON VmServer where
        parseJSON
          = Core.withObject "VmServer" Core.$
              \ x ->
                VmServer' Core.<$>
                  (x Core..:? "vmManagerName") Core.<*> x Core..:? "vmManagerType"
                    Core.<*> x Core..:? "vmName"
                    Core.<*> x Core..:? "vmPath"
                    Core.<*> x Core..:? "vmServerAddress"
