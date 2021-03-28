{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Trial
  ( Trial (..)
  -- * Smart constructor
  , mkTrial
  -- * Lenses
  , tCreatedBy
  , tCreationTime
  , tDisplayName
  , tExperimentName
  , tLastModifiedBy
  , tLastModifiedTime
  , tSource
  , tTags
  , tTrialArn
  , tTrialComponentSummaries
  , tTrialName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DisplayName as Types
import qualified Network.AWS.SageMaker.Types.ExperimentName as Types
import qualified Network.AWS.SageMaker.Types.Tag as Types
import qualified Network.AWS.SageMaker.Types.TrialArn as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSimpleSummary as Types
import qualified Network.AWS.SageMaker.Types.TrialName as Types
import qualified Network.AWS.SageMaker.Types.TrialSource as Types
import qualified Network.AWS.SageMaker.Types.UserContext as Types

-- | The properties of a trial as returned by the 'Search' API.
--
-- /See:/ 'mkTrial' smart constructor.
data Trial = Trial'
  { createdBy :: Core.Maybe Types.UserContext
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the trial was created.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
  , experimentName :: Core.Maybe Types.ExperimentName
    -- ^ The name of the experiment the trial is part of.
  , lastModifiedBy :: Core.Maybe Types.UserContext
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Who last modified the trial.
  , source :: Core.Maybe Types.TrialSource
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
  , trialArn :: Core.Maybe Types.TrialArn
    -- ^ The Amazon Resource Name (ARN) of the trial.
  , trialComponentSummaries :: Core.Maybe [Types.TrialComponentSimpleSummary]
    -- ^ A list of the components associated with the trial. For each component, a summary of the component's properties is included.
  , trialName :: Core.Maybe Types.TrialName
    -- ^ The name of the trial.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Trial' value with any optional fields omitted.
mkTrial
    :: Trial
mkTrial
  = Trial'{createdBy = Core.Nothing, creationTime = Core.Nothing,
           displayName = Core.Nothing, experimentName = Core.Nothing,
           lastModifiedBy = Core.Nothing, lastModifiedTime = Core.Nothing,
           source = Core.Nothing, tags = Core.Nothing,
           trialArn = Core.Nothing, trialComponentSummaries = Core.Nothing,
           trialName = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedBy :: Lens.Lens' Trial (Core.Maybe Types.UserContext)
tCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE tCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the trial was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreationTime :: Lens.Lens' Trial (Core.Maybe Core.NominalDiffTime)
tCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the trial as displayed. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDisplayName :: Lens.Lens' Trial (Core.Maybe Types.DisplayName)
tDisplayName = Lens.field @"displayName"
{-# INLINEABLE tDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the experiment the trial is part of.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tExperimentName :: Lens.Lens' Trial (Core.Maybe Types.ExperimentName)
tExperimentName = Lens.field @"experimentName"
{-# INLINEABLE tExperimentName #-}
{-# DEPRECATED experimentName "Use generic-lens or generic-optics with 'experimentName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastModifiedBy :: Lens.Lens' Trial (Core.Maybe Types.UserContext)
tLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE tLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | Who last modified the trial.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastModifiedTime :: Lens.Lens' Trial (Core.Maybe Core.NominalDiffTime)
tLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE tLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSource :: Lens.Lens' Trial (Core.Maybe Types.TrialSource)
tSource = Lens.field @"source"
{-# INLINEABLE tSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | The list of tags that are associated with the trial. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTags :: Lens.Lens' Trial (Core.Maybe [Types.Tag])
tTags = Lens.field @"tags"
{-# INLINEABLE tTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialArn :: Lens.Lens' Trial (Core.Maybe Types.TrialArn)
tTrialArn = Lens.field @"trialArn"
{-# INLINEABLE tTrialArn #-}
{-# DEPRECATED trialArn "Use generic-lens or generic-optics with 'trialArn' instead"  #-}

-- | A list of the components associated with the trial. For each component, a summary of the component's properties is included.
--
-- /Note:/ Consider using 'trialComponentSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialComponentSummaries :: Lens.Lens' Trial (Core.Maybe [Types.TrialComponentSimpleSummary])
tTrialComponentSummaries = Lens.field @"trialComponentSummaries"
{-# INLINEABLE tTrialComponentSummaries #-}
{-# DEPRECATED trialComponentSummaries "Use generic-lens or generic-optics with 'trialComponentSummaries' instead"  #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrialName :: Lens.Lens' Trial (Core.Maybe Types.TrialName)
tTrialName = Lens.field @"trialName"
{-# INLINEABLE tTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

instance Core.FromJSON Trial where
        parseJSON
          = Core.withObject "Trial" Core.$
              \ x ->
                Trial' Core.<$>
                  (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "ExperimentName"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "Source"
                    Core.<*> x Core..:? "Tags"
                    Core.<*> x Core..:? "TrialArn"
                    Core.<*> x Core..:? "TrialComponentSummaries"
                    Core.<*> x Core..:? "TrialName"
