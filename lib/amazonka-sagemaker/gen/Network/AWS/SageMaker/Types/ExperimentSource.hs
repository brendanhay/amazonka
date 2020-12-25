{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSource
  ( ExperimentSource (..),

    -- * Smart constructor
    mkExperimentSource,

    -- * Lenses
    esSourceArn,
    esSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SourceArn as Types
import qualified Network.AWS.SageMaker.Types.SourceType as Types

-- | The source of the experiment.
--
-- /See:/ 'mkExperimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Types.SourceArn,
    -- | The source type.
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExperimentSource' value with any optional fields omitted.
mkExperimentSource ::
  -- | 'sourceArn'
  Types.SourceArn ->
  ExperimentSource
mkExperimentSource sourceArn =
  ExperimentSource' {sourceArn, sourceType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceArn :: Lens.Lens' ExperimentSource Types.SourceArn
esSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED esSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' ExperimentSource (Core.Maybe Types.SourceType)
esSourceType = Lens.field @"sourceType"
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON ExperimentSource where
  parseJSON =
    Core.withObject "ExperimentSource" Core.$
      \x ->
        ExperimentSource'
          Core.<$> (x Core..: "SourceArn") Core.<*> (x Core..:? "SourceType")
