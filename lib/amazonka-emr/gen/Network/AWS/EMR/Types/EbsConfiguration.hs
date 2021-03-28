{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.EbsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.EbsConfiguration
  ( EbsConfiguration (..)
  -- * Smart constructor
  , mkEbsConfiguration
  -- * Lenses
  , ecEbsBlockDeviceConfigs
  , ecEbsOptimized
  ) where

import qualified Network.AWS.EMR.Types.EbsBlockDeviceConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon EBS configuration of a cluster instance.
--
-- /See:/ 'mkEbsConfiguration' smart constructor.
data EbsConfiguration = EbsConfiguration'
  { ebsBlockDeviceConfigs :: Core.Maybe [Types.EbsBlockDeviceConfig]
    -- ^ An array of Amazon EBS volume specifications attached to a cluster instance.
  , ebsOptimized :: Core.Maybe Core.Bool
    -- ^ Indicates whether an Amazon EBS volume is EBS-optimized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsConfiguration' value with any optional fields omitted.
mkEbsConfiguration
    :: EbsConfiguration
mkEbsConfiguration
  = EbsConfiguration'{ebsBlockDeviceConfigs = Core.Nothing,
                      ebsOptimized = Core.Nothing}

-- | An array of Amazon EBS volume specifications attached to a cluster instance.
--
-- /Note:/ Consider using 'ebsBlockDeviceConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEbsBlockDeviceConfigs :: Lens.Lens' EbsConfiguration (Core.Maybe [Types.EbsBlockDeviceConfig])
ecEbsBlockDeviceConfigs = Lens.field @"ebsBlockDeviceConfigs"
{-# INLINEABLE ecEbsBlockDeviceConfigs #-}
{-# DEPRECATED ebsBlockDeviceConfigs "Use generic-lens or generic-optics with 'ebsBlockDeviceConfigs' instead"  #-}

-- | Indicates whether an Amazon EBS volume is EBS-optimized.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEbsOptimized :: Lens.Lens' EbsConfiguration (Core.Maybe Core.Bool)
ecEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE ecEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

instance Core.FromJSON EbsConfiguration where
        toJSON EbsConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EbsBlockDeviceConfigs" Core..=) Core.<$> ebsBlockDeviceConfigs,
                  ("EbsOptimized" Core..=) Core.<$> ebsOptimized])
