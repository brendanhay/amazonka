{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Experiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Experiment
  ( Experiment (..)
  -- * Smart constructor
  , mkExperiment
  -- * Lenses
  , eCreatedBy
  , eCreationTime
  , eDescription
  , eDisplayName
  , eExperimentArn
  , eExperimentName
  , eLastModifiedBy
  , eLastModifiedTime
  , eSource
  , eTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Description as Types
import qualified Network.AWS.SageMaker.Types.ExperimentArn as Types
import qualified Network.AWS.SageMaker.Types.ExperimentEntityName as Types
import qualified Network.AWS.SageMaker.Types.ExperimentSource as Types
import qualified Network.AWS.SageMaker.Types.Tag as Types
import qualified Network.AWS.SageMaker.Types.UserContext as Types

-- | The properties of an experiment as returned by the 'Search' API.
--
-- /See:/ 'mkExperiment' smart constructor.
data Experiment = Experiment'
  { createdBy :: Core.Maybe Types.UserContext
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was created.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the experiment.
  , displayName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
  , experimentArn :: Core.Maybe Types.ExperimentArn
    -- ^ The Amazon Resource Name (ARN) of the experiment.
  , experimentName :: Core.Maybe Types.ExperimentEntityName
    -- ^ The name of the experiment.
  , lastModifiedBy :: Core.Maybe Types.UserContext
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the experiment was last modified.
  , source :: Core.Maybe Types.ExperimentSource
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Experiment' value with any optional fields omitted.
mkExperiment
    :: Experiment
mkExperiment
  = Experiment'{createdBy = Core.Nothing,
                creationTime = Core.Nothing, description = Core.Nothing,
                displayName = Core.Nothing, experimentArn = Core.Nothing,
                experimentName = Core.Nothing, lastModifiedBy = Core.Nothing,
                lastModifiedTime = Core.Nothing, source = Core.Nothing,
                tags = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreatedBy :: Lens.Lens' Experiment (Core.Maybe Types.UserContext)
eCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE eCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the experiment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCreationTime :: Lens.Lens' Experiment (Core.Maybe Core.NominalDiffTime)
eCreationTime = Lens.field @"creationTime"
{-# INLINEABLE eCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Experiment (Core.Maybe Types.Description)
eDescription = Lens.field @"description"
{-# INLINEABLE eDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDisplayName :: Lens.Lens' Experiment (Core.Maybe Types.ExperimentEntityName)
eDisplayName = Lens.field @"displayName"
{-# INLINEABLE eDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExperimentArn :: Lens.Lens' Experiment (Core.Maybe Types.ExperimentArn)
eExperimentArn = Lens.field @"experimentArn"
{-# INLINEABLE eExperimentArn #-}
{-# DEPRECATED experimentArn "Use generic-lens or generic-optics with 'experimentArn' instead"  #-}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExperimentName :: Lens.Lens' Experiment (Core.Maybe Types.ExperimentEntityName)
eExperimentName = Lens.field @"experimentName"
{-# INLINEABLE eExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastModifiedBy :: Lens.Lens' Experiment (Core.Maybe Types.UserContext)
eLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE eLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the experiment was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastModifiedTime :: Lens.Lens' Experiment (Core.Maybe Core.NominalDiffTime)
eLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE eLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSource :: Lens.Lens' Experiment (Core.Maybe Types.ExperimentSource)
eSource = Lens.field @"source"
{-# INLINEABLE eSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The list of tags that are associated with the experiment. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' Experiment (Core.Maybe [Types.Tag])
eTags = Lens.field @"tags"
{-# INLINEABLE eTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON Experiment where
        parseJSON
          = Core.withObject "Experiment" Core.$
              \ x ->
                Experiment' Core.<$>
                  (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "ExperimentArn"
                    Core.<*> x Core..:? "ExperimentName"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "Source"
                    Core.<*> x Core..:? "Tags"
