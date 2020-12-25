{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponent
  ( TrialComponent (..),

    -- * Smart constructor
    mkTrialComponent,

    -- * Lenses
    tcCreatedBy,
    tcCreationTime,
    tcDisplayName,
    tcEndTime,
    tcInputArtifacts,
    tcLastModifiedBy,
    tcLastModifiedTime,
    tcMetrics,
    tcOutputArtifacts,
    tcParameters,
    tcParents,
    tcSource,
    tcSourceDetail,
    tcStartTime,
    tcStatus,
    tcTags,
    tcTrialComponentArn,
    tcTrialComponentName,
  )
where

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
  { createdBy :: Core.Maybe Types.UserContext,
    -- | When the component was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Types.DisplayName,
    -- | When the component ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input artifacts of the component.
    inputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    lastModifiedBy :: Core.Maybe Types.UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The metrics for the component.
    metrics :: Core.Maybe [Types.TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    -- | The hyperparameters of the component.
    parameters :: Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue),
    -- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
    parents :: Core.Maybe [Types.Parent],
    -- | The Amazon Resource Name (ARN) and job type of the source of the component.
    source :: Core.Maybe Types.TrialComponentSource,
    -- | Details of the source of the component.
    sourceDetail :: Core.Maybe Types.TrialComponentSourceDetail,
    -- | When the component started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    status :: Core.Maybe Types.TrialComponentStatus,
    -- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
    tags :: Core.Maybe [Types.Tag],
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Types.TrialComponentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TrialComponent' value with any optional fields omitted.
mkTrialComponent ::
  TrialComponent
mkTrialComponent =
  TrialComponent'
    { createdBy = Core.Nothing,
      creationTime = Core.Nothing,
      displayName = Core.Nothing,
      endTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      metrics = Core.Nothing,
      outputArtifacts = Core.Nothing,
      parameters = Core.Nothing,
      parents = Core.Nothing,
      source = Core.Nothing,
      sourceDetail = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      trialComponentArn = Core.Nothing,
      trialComponentName = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreatedBy :: Lens.Lens' TrialComponent (Core.Maybe Types.UserContext)
tcCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED tcCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreationTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcDisplayName :: Lens.Lens' TrialComponent (Core.Maybe Types.DisplayName)
tcDisplayName = Lens.field @"displayName"
{-# DEPRECATED tcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcEndTime = Lens.field @"endTime"
{-# DEPRECATED tcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
tcInputArtifacts = Lens.field @"inputArtifacts"
{-# DEPRECATED tcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedBy :: Lens.Lens' TrialComponent (Core.Maybe Types.UserContext)
tcLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED tcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED tcLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMetrics :: Lens.Lens' TrialComponent (Core.Maybe [Types.TrialComponentMetricSummary])
tcMetrics = Lens.field @"metrics"
{-# DEPRECATED tcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcOutputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
tcOutputArtifacts = Lens.field @"outputArtifacts"
{-# DEPRECATED tcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParameters :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue))
tcParameters = Lens.field @"parameters"
{-# DEPRECATED tcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParents :: Lens.Lens' TrialComponent (Core.Maybe [Types.Parent])
tcParents = Lens.field @"parents"
{-# DEPRECATED tcParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | The Amazon Resource Name (ARN) and job type of the source of the component.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentSource)
tcSource = Lens.field @"source"
{-# DEPRECATED tcSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Details of the source of the component.
--
-- /Note:/ Consider using 'sourceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSourceDetail :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentSourceDetail)
tcSourceDetail = Lens.field @"sourceDetail"
{-# DEPRECATED tcSourceDetail "Use generic-lens or generic-optics with 'sourceDetail' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStartTime :: Lens.Lens' TrialComponent (Core.Maybe Core.NominalDiffTime)
tcStartTime = Lens.field @"startTime"
{-# DEPRECATED tcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStatus :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentStatus)
tcStatus = Lens.field @"status"
{-# DEPRECATED tcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTags :: Lens.Lens' TrialComponent (Core.Maybe [Types.Tag])
tcTags = Lens.field @"tags"
{-# DEPRECATED tcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentArn :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentArn)
tcTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED tcTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentName :: Lens.Lens' TrialComponent (Core.Maybe Types.TrialComponentName)
tcTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED tcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Core.FromJSON TrialComponent where
  parseJSON =
    Core.withObject "TrialComponent" Core.$
      \x ->
        TrialComponent'
          Core.<$> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "EndTime")
          Core.<*> (x Core..:? "InputArtifacts")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "Metrics")
          Core.<*> (x Core..:? "OutputArtifacts")
          Core.<*> (x Core..:? "Parameters")
          Core.<*> (x Core..:? "Parents")
          Core.<*> (x Core..:? "Source")
          Core.<*> (x Core..:? "SourceDetail")
          Core.<*> (x Core..:? "StartTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "TrialComponentArn")
          Core.<*> (x Core..:? "TrialComponentName")
