{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProcessorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ProcessorInfo
  ( ProcessorInfo (..)
  -- * Smart constructor
  , mkProcessorInfo
  -- * Lenses
  , piSupportedArchitectures
  , piSustainedClockSpeedInGhz
  ) where

import qualified Network.AWS.EC2.Types.ArchitectureType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the processor used by the instance type.
--
-- /See:/ 'mkProcessorInfo' smart constructor.
data ProcessorInfo = ProcessorInfo'
  { supportedArchitectures :: Core.Maybe [Types.ArchitectureType]
    -- ^ The architectures supported by the instance type.
  , sustainedClockSpeedInGhz :: Core.Maybe Core.Double
    -- ^ The speed of the processor, in GHz.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessorInfo' value with any optional fields omitted.
mkProcessorInfo
    :: ProcessorInfo
mkProcessorInfo
  = ProcessorInfo'{supportedArchitectures = Core.Nothing,
                   sustainedClockSpeedInGhz = Core.Nothing}

-- | The architectures supported by the instance type.
--
-- /Note:/ Consider using 'supportedArchitectures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSupportedArchitectures :: Lens.Lens' ProcessorInfo (Core.Maybe [Types.ArchitectureType])
piSupportedArchitectures = Lens.field @"supportedArchitectures"
{-# INLINEABLE piSupportedArchitectures #-}
{-# DEPRECATED supportedArchitectures "Use generic-lens or generic-optics with 'supportedArchitectures' instead"  #-}

-- | The speed of the processor, in GHz.
--
-- /Note:/ Consider using 'sustainedClockSpeedInGhz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSustainedClockSpeedInGhz :: Lens.Lens' ProcessorInfo (Core.Maybe Core.Double)
piSustainedClockSpeedInGhz = Lens.field @"sustainedClockSpeedInGhz"
{-# INLINEABLE piSustainedClockSpeedInGhz #-}
{-# DEPRECATED sustainedClockSpeedInGhz "Use generic-lens or generic-optics with 'sustainedClockSpeedInGhz' instead"  #-}

instance Core.FromXML ProcessorInfo where
        parseXML x
          = ProcessorInfo' Core.<$>
              (x Core..@? "supportedArchitectures" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "sustainedClockSpeedInGhz"
