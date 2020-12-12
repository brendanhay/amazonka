{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProcessorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProcessorInfo
  ( ProcessorInfo (..),

    -- * Smart constructor
    mkProcessorInfo,

    -- * Lenses
    piSupportedArchitectures,
    piSustainedClockSpeedInGhz,
  )
where

import Network.AWS.EC2.Types.ArchitectureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the processor used by the instance type.
--
-- /See:/ 'mkProcessorInfo' smart constructor.
data ProcessorInfo = ProcessorInfo'
  { supportedArchitectures ::
      Lude.Maybe [ArchitectureType],
    sustainedClockSpeedInGhz :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessorInfo' with the minimum fields required to make a request.
--
-- * 'supportedArchitectures' - The architectures supported by the instance type.
-- * 'sustainedClockSpeedInGhz' - The speed of the processor, in GHz.
mkProcessorInfo ::
  ProcessorInfo
mkProcessorInfo =
  ProcessorInfo'
    { supportedArchitectures = Lude.Nothing,
      sustainedClockSpeedInGhz = Lude.Nothing
    }

-- | The architectures supported by the instance type.
--
-- /Note:/ Consider using 'supportedArchitectures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSupportedArchitectures :: Lens.Lens' ProcessorInfo (Lude.Maybe [ArchitectureType])
piSupportedArchitectures = Lens.lens (supportedArchitectures :: ProcessorInfo -> Lude.Maybe [ArchitectureType]) (\s a -> s {supportedArchitectures = a} :: ProcessorInfo)
{-# DEPRECATED piSupportedArchitectures "Use generic-lens or generic-optics with 'supportedArchitectures' instead." #-}

-- | The speed of the processor, in GHz.
--
-- /Note:/ Consider using 'sustainedClockSpeedInGhz' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piSustainedClockSpeedInGhz :: Lens.Lens' ProcessorInfo (Lude.Maybe Lude.Double)
piSustainedClockSpeedInGhz = Lens.lens (sustainedClockSpeedInGhz :: ProcessorInfo -> Lude.Maybe Lude.Double) (\s a -> s {sustainedClockSpeedInGhz = a} :: ProcessorInfo)
{-# DEPRECATED piSustainedClockSpeedInGhz "Use generic-lens or generic-optics with 'sustainedClockSpeedInGhz' instead." #-}

instance Lude.FromXML ProcessorInfo where
  parseXML x =
    ProcessorInfo'
      Lude.<$> ( x Lude..@? "supportedArchitectures" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "sustainedClockSpeedInGhz")
