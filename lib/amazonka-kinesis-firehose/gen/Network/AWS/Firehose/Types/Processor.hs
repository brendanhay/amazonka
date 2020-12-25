{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Processor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.Processor
  ( Processor (..),

    -- * Smart constructor
    mkProcessor,

    -- * Lenses
    pType,
    pParameters,
  )
where

import qualified Network.AWS.Firehose.Types.ProcessorParameter as Types
import qualified Network.AWS.Firehose.Types.ProcessorType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a data processor.
--
-- /See:/ 'mkProcessor' smart constructor.
data Processor = Processor'
  { -- | The type of processor.
    type' :: Types.ProcessorType,
    -- | The processor parameters.
    parameters :: Core.Maybe [Types.ProcessorParameter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Processor' value with any optional fields omitted.
mkProcessor ::
  -- | 'type\''
  Types.ProcessorType ->
  Processor
mkProcessor type' = Processor' {type', parameters = Core.Nothing}

-- | The type of processor.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Processor Types.ProcessorType
pType = Lens.field @"type'"
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The processor parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pParameters :: Lens.Lens' Processor (Core.Maybe [Types.ProcessorParameter])
pParameters = Lens.field @"parameters"
{-# DEPRECATED pParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON Processor where
  toJSON Processor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.FromJSON Processor where
  parseJSON =
    Core.withObject "Processor" Core.$
      \x ->
        Processor'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..:? "Parameters")
