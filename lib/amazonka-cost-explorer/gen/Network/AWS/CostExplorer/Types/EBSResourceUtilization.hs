{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EBSResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.EBSResourceUtilization
  ( EBSResourceUtilization (..)
  -- * Smart constructor
  , mkEBSResourceUtilization
  -- * Lenses
  , ebsruEbsReadBytesPerSecond
  , ebsruEbsReadOpsPerSecond
  , ebsruEbsWriteBytesPerSecond
  , ebsruEbsWriteOpsPerSecond
  ) where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The EBS field that contains a list of EBS metrics associated with the current instance. 
--
-- /See:/ 'mkEBSResourceUtilization' smart constructor.
data EBSResourceUtilization = EBSResourceUtilization'
  { ebsReadBytesPerSecond :: Core.Maybe Types.GenericString
    -- ^ The maximum size of read operations per second 
  , ebsReadOpsPerSecond :: Core.Maybe Types.GenericString
    -- ^ The maximum number of read operations per second. 
  , ebsWriteBytesPerSecond :: Core.Maybe Types.GenericString
    -- ^ The maximum size of write operations per second. 
  , ebsWriteOpsPerSecond :: Core.Maybe Types.GenericString
    -- ^ The maximum number of write operations per second. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EBSResourceUtilization' value with any optional fields omitted.
mkEBSResourceUtilization
    :: EBSResourceUtilization
mkEBSResourceUtilization
  = EBSResourceUtilization'{ebsReadBytesPerSecond = Core.Nothing,
                            ebsReadOpsPerSecond = Core.Nothing,
                            ebsWriteBytesPerSecond = Core.Nothing,
                            ebsWriteOpsPerSecond = Core.Nothing}

-- | The maximum size of read operations per second 
--
-- /Note:/ Consider using 'ebsReadBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsruEbsReadBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Types.GenericString)
ebsruEbsReadBytesPerSecond = Lens.field @"ebsReadBytesPerSecond"
{-# INLINEABLE ebsruEbsReadBytesPerSecond #-}
{-# DEPRECATED ebsReadBytesPerSecond "Use generic-lens or generic-optics with 'ebsReadBytesPerSecond' instead"  #-}

-- | The maximum number of read operations per second. 
--
-- /Note:/ Consider using 'ebsReadOpsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsruEbsReadOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Types.GenericString)
ebsruEbsReadOpsPerSecond = Lens.field @"ebsReadOpsPerSecond"
{-# INLINEABLE ebsruEbsReadOpsPerSecond #-}
{-# DEPRECATED ebsReadOpsPerSecond "Use generic-lens or generic-optics with 'ebsReadOpsPerSecond' instead"  #-}

-- | The maximum size of write operations per second. 
--
-- /Note:/ Consider using 'ebsWriteBytesPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsruEbsWriteBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Types.GenericString)
ebsruEbsWriteBytesPerSecond = Lens.field @"ebsWriteBytesPerSecond"
{-# INLINEABLE ebsruEbsWriteBytesPerSecond #-}
{-# DEPRECATED ebsWriteBytesPerSecond "Use generic-lens or generic-optics with 'ebsWriteBytesPerSecond' instead"  #-}

-- | The maximum number of write operations per second. 
--
-- /Note:/ Consider using 'ebsWriteOpsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebsruEbsWriteOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Types.GenericString)
ebsruEbsWriteOpsPerSecond = Lens.field @"ebsWriteOpsPerSecond"
{-# INLINEABLE ebsruEbsWriteOpsPerSecond #-}
{-# DEPRECATED ebsWriteOpsPerSecond "Use generic-lens or generic-optics with 'ebsWriteOpsPerSecond' instead"  #-}

instance Core.FromJSON EBSResourceUtilization where
        parseJSON
          = Core.withObject "EBSResourceUtilization" Core.$
              \ x ->
                EBSResourceUtilization' Core.<$>
                  (x Core..:? "EbsReadBytesPerSecond") Core.<*>
                    x Core..:? "EbsReadOpsPerSecond"
                    Core.<*> x Core..:? "EbsWriteBytesPerSecond"
                    Core.<*> x Core..:? "EbsWriteOpsPerSecond"
