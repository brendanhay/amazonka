{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessingConfiguration
  ( ProcessingConfiguration (..),

    -- * Smart constructor
    mkProcessingConfiguration,

    -- * Lenses
    pcEnabled,
    pcProcessors,
  )
where

import qualified Network.AWS.Firehose.Types.Processor as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a data processing configuration.
--
-- /See:/ 'mkProcessingConfiguration' smart constructor.
data ProcessingConfiguration = ProcessingConfiguration'
  { -- | Enables or disables data processing.
    enabled :: Core.Maybe Core.Bool,
    -- | The data processors.
    processors :: Core.Maybe [Types.Processor]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingConfiguration' value with any optional fields omitted.
mkProcessingConfiguration ::
  ProcessingConfiguration
mkProcessingConfiguration =
  ProcessingConfiguration'
    { enabled = Core.Nothing,
      processors = Core.Nothing
    }

-- | Enables or disables data processing.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcEnabled :: Lens.Lens' ProcessingConfiguration (Core.Maybe Core.Bool)
pcEnabled = Lens.field @"enabled"
{-# DEPRECATED pcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The data processors.
--
-- /Note:/ Consider using 'processors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProcessors :: Lens.Lens' ProcessingConfiguration (Core.Maybe [Types.Processor])
pcProcessors = Lens.field @"processors"
{-# DEPRECATED pcProcessors "Use generic-lens or generic-optics with 'processors' instead." #-}

instance Core.FromJSON ProcessingConfiguration where
  toJSON ProcessingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("Processors" Core..=) Core.<$> processors
          ]
      )

instance Core.FromJSON ProcessingConfiguration where
  parseJSON =
    Core.withObject "ProcessingConfiguration" Core.$
      \x ->
        ProcessingConfiguration'
          Core.<$> (x Core..:? "Enabled") Core.<*> (x Core..:? "Processors")
