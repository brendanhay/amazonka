{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponent
  ( TrialComponent (..)
  -- * Smart constructor
  , mkTrialComponent
  -- * Lenses
  , tcCreatedBy
  , tcCreationTime
  , tcDisplayName
  , tcEndTime
  , tcInputArtifacts
  , tcLastModifiedBy
  , tcLastModifiedTime
  , tcMetrics
  , tcOutputArtifacts
  , tcParameters
  , tcParents
  , tcSource
  , tcSourceDetail
  , tcStartTime
  , tcStatus
  , tcTags
  , tcTrialComponentArn
  , tcTrialComponentName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DisplayName as Types
import qualified Network.AWS.SageMaker.Types.Parent as Types
import qualified Network.AWS.SageMaker.Types.Tag as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentArn as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentArtifact as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentKey256 as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentKey64 as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentMetricSummary as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentName as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentParameterValue as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSource as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSourceDetail as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentStatus as Types
import qualified Network.AWS.SageMaker.Types.UserContext as Types

-- | The properties of a trial component as returned by the 'Search' API.
--
-- /See:/ 'mkTrialComponent' smart constructor.
data TrialComponent = TrialComponent'
  { createdBy :: Core.Maybe Types.UserContext
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was created.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component ended.
  , inputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact)
    -- ^ The input artifacts of the component.
  , lastModifiedBy :: Core.Maybe Types.UserContext
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component was last modified.
  , metrics :: Core.Maybe [Types.TrialComponentMetricSummary]
    -- ^ The metrics for the component.
  , outputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact)
    -- ^ The output artifacts of the component.
  , parameters :: Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue)
    -- ^ The hyperparameters of the component.
  , parents :: Core.Maybe [Types.Parent]
    -- ^ An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
  , source :: Core.Maybe Types.TrialComponentSource
    -- ^ The Amazon Resource Name (ARN) and job type of the source of the component.
  , sourceDetail :: Core.Maybe Types.TrialComponentSourceDetail
    -- ^ Details of the source of the component.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the component started.
  , status :: Core.Maybe Types.TrialComponentStatus
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
  , trialComponentArn :: Core.Maybe Types.TrialComponentArn
    -- ^ The Amazon Resource Name (ARN) of the trial component.
  , trialComponentName :: Core.Maybe Types.TrialComponentName
    -- ^ The name of the trial component.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TrialComponent' value with any optional fields omitted.
mkTrialComponent
    :: TrialComponent
mkTrialComponent
  = TrialComponent'{createdBy = Core.Nothing,
                    creationTime = Core.Nothing, displayName = Core.Nothing,
                    endTime = Core.Nothing, inputArtifacts = Core.Nothing,
                    lastModifiedBy = Core.Nothing, lastModifiedTime = Core.Nothing,
                    metrics = Core.Nothing, outputArtifacts = Core.Nothing,
                    parameters = Core.Nothing, parents = Core.Nothing,
                    source = Core.Nothing, sourceDetail = Core.Nothing,
                    startTime = Core.Nothing, status = Core.Nothing,
                    tags = Core.Nothing, trialComponentArn = Core.Nothing,
                    trialComponentName = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreatedBy :: Lens.Lens' TrialComponent (Core.Maybe Types.UserContext)
tcCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE tcCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreationTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcCreationTime = Lens.field @"creationTime"
{-# INLINEABLE tcCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcDisplayName :: Lens.Lens' TrialComponent (Core.Maybe Types.DisplayName)
tcDisplayName = Lens.field @"displayName"
{-# INLINEABLE tcDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcEndTime = Lens.field @"endTime"
{-# INLINEABLE tcEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
tcInputArtifacts = Lens.field @"inputArtifacts"
{-# INLINEABLE tcInputArtifacts #-}
{-# DEPRECATED inputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedBy :: Lens.Lens' TrialComponent (Core.Maybe Types.UserContext)
tcLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE tcLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE tcLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMetrics :: Lens.Lens' TrialComponent (Core.Maybe [Types.TrialComponentMetricSummary])
tcMetrics = Lens.field @"metrics"
{-# INLINEABLE tcMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcOutputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
tcOutputArtifacts = Lens.field @"outputArtifacts"
{-# INLINEABLE tcOutputArtifacts #-}
{-# DEPRECATED outputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead"  #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParameters :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue))
tcParameters = Lens.field @"parameters"
{-# INLINEABLE tcParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParents :: Lens.Lens' TrialComponent (Core.Maybe [Types.Parent])
tcParents = Lens.field @"parents"
{-# INLINEABLE tcParents #-}
{-# DEPRECATED parents "Use generic-lens or generic-optics with 'parents' instead"  #-}

-- | The Amazon Resource Name (ARN) and job type of the source of the component.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentSource)
tcSource = Lens.field @"source"
{-# INLINEABLE tcSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | Details of the source of the component.
--
-- /Note:/ Consider using 'sourceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSourceDetail :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentSourceDetail)
tcSourceDetail = Lens.field @"sourceDetail"
{-# INLINEABLE tcSourceDetail #-}
{-# DEPRECATED sourceDetail "Use generic-lens or generic-optics with 'sourceDetail' instead"  #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStartTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcStartTime = Lens.field @"startTime"
{-# INLINEABLE tcStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStatus :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentStatus)
tcStatus = Lens.field @"status"
{-# INLINEABLE tcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTags :: Lens.Lens' TrialComponent (Core.Maybe [Types.Tag])
tcTags = Lens.field @"tags"
{-# INLINEABLE tcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentArn :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentArn)
tcTrialComponentArn = Lens.field @"trialComponentArn"
{-# INLINEABLE tcTrialComponentArn #-}
{-# DEPRECATED trialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead"  #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentName :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentName)
tcTrialComponentName = Lens.field @"trialComponentName"
{-# INLINEABLE tcTrialComponentName #-}
{-# DEPRECATED trialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead"  #-}

instance Core.FromJSON TrialComponent where
        parseJSON
          = Core.withObject "TrialComponent" Core.$
              \ x ->
                TrialComponent' Core.<$>
                  (x Core..:? "CreatedBy") Core.<*> x Core..:? "CreationTime"
                    Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "InputArtifacts"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "Metrics"
                    Core.<*> x Core..:? "OutputArtifacts"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "Parents"
                    Core.<*> x Core..:? "Source"
                    Core.<*> x Core..:? "SourceDetail"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "Tags"
                    Core.<*> x Core..:? "TrialComponentArn"
                    Core.<*> x Core..:? "TrialComponentName"
