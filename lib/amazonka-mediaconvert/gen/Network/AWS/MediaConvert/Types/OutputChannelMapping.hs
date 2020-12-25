{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputChannelMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputChannelMapping
  ( OutputChannelMapping (..),

    -- * Smart constructor
    mkOutputChannelMapping,

    -- * Lenses
    ocmInputChannels,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | OutputChannel mapping settings.
--
-- /See:/ 'mkOutputChannelMapping' smart constructor.
newtype OutputChannelMapping = OutputChannelMapping'
  { -- | List of input channels
    inputChannels :: Core.Maybe [Core.Int]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputChannelMapping' value with any optional fields omitted.
mkOutputChannelMapping ::
  OutputChannelMapping
mkOutputChannelMapping =
  OutputChannelMapping' {inputChannels = Core.Nothing}

-- | List of input channels
--
-- /Note:/ Consider using 'inputChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocmInputChannels :: Lens.Lens' OutputChannelMapping (Core.Maybe [Core.Int])
ocmInputChannels = Lens.field @"inputChannels"
{-# DEPRECATED ocmInputChannels "Use generic-lens or generic-optics with 'inputChannels' instead." #-}

instance Core.FromJSON OutputChannelMapping where
  toJSON OutputChannelMapping {..} =
    Core.object
      (Core.catMaybes [("inputChannels" Core..=) Core.<$> inputChannels])

instance Core.FromJSON OutputChannelMapping where
  parseJSON =
    Core.withObject "OutputChannelMapping" Core.$
      \x -> OutputChannelMapping' Core.<$> (x Core..:? "inputChannels")
