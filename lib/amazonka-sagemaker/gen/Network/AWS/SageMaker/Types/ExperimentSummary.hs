{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ExperimentSummary
  ( ExperimentSummary (..)
  -- * Smart constructor
  , mkExperimentSummary
  -- * Lenses
  , esfCreationTime
  , esfDisplayName
  , esfExperimentArn
  , esfExperimentName
  , esfExperimentSource
  , esfLastModifiedTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExperimentArn as Types
import qualified Network.AWS.SageMaker.Types.ExperimentEntityName as Types
import qualified Network.AWS.SageMaker.Types.ExperimentSource as Types

-- | A summary of the properties of an experiment. To get the complete set of properties, call the 'DescribeExperiment' API and provide the @ExperimentName@ .
--
-- /See:/ 'mkExperimentSummary' smart constructor.
data ExperimentSummary = ExperimentSummary'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was created.
  , displayName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
  , experimentArn :: Core.Maybe Types.ExperimentArn
    -- ^ The Amazon Resource Name (ARN) of the experiment.
  , experimentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment.
  , experimentSource :: Core.Maybe Types.ExperimentSource
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was last modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExperimentSummary' value with any optional fields omitted.
mkExperimentSummary
    :: ExperimentSummary
mkExperimentSummary
  = ExperimentSummary'{creationTime = Core.Nothing,
                       displayName = Core.Nothing, experimentArn = Core.Nothing,
                       experimentName = Core.Nothing, experimentSource = Core.Nothing,
                       lastModifiedTime = Core.Nothing}

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfCreationTime :: Lens.Lens' ExperimentSummary (Core.Maybe Core.NominalDiffTime)
esfCreationTime = Lens.field @"creationTime"
{-# INLINEABLE esfCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfDisplayName :: Lens.Lens' ExperimentSummary (Core.Maybe Types.ExperimentEntityName)
esfDisplayName = Lens.field @"displayName"
{-# INLINEABLE esfDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentArn :: Lens.Lens' ExperimentSummary (Core.Maybe Types.ExperimentArn)
esfExperimentArn = Lens.field @"experimentArn"
{-# INLINEABLE esfExperimentArn #-}
{-# DEPRECATED experimentArn "Use generic-lens or generic-optics with 'experimentArn' instead"  #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentName :: Lens.Lens' ExperimentSummary (Core.Maybe Types.ExperimentEntityName)
esfExperimentName = Lens.field @"experimentName"
{-# INLINEABLE esfExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'experimentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfExperimentSource :: Lens.Lens' ExperimentSummary (Core.Maybe Types.ExperimentSource)
esfExperimentSource = Lens.field @"experimentSource"
{-# INLINEABLE esfExperimentSource #-}
{-# DEPRECATED experimentSource "Use generic-lens or generic-optics with 'experimentSource' instead"  #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esfLastModifiedTime :: Lens.Lens' ExperimentSummary (Core.Maybe Core.NominalDiffTime)
esfLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE esfLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

instance Core.FromJSON ExperimentSummary where
        parseJSON
          = Core.withObject "ExperimentSummary" Core.$
              \ x ->
                ExperimentSummary' Core.<$>
                  (x Core..:? "CreationTime") Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "ExperimentArn"
                    Core.<*> x Core..:? "ExperimentName"
                    Core.<*> x Core..:? "ExperimentSource"
                    Core.<*> x Core..:? "LastModifiedTime"
