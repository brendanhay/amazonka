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
    tcCreationTime,
    tcStatus,
    tcSourceDetail,
    tcMetrics,
    tcOutputArtifacts,
    tcStartTime,
    tcCreatedBy,
    tcLastModifiedTime,
    tcParents,
    tcEndTime,
    tcTrialComponentName,
    tcParameters,
    tcSource,
    tcDisplayName,
    tcLastModifiedBy,
    tcTrialComponentARN,
    tcInputArtifacts,
    tcTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentArtifact
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial component as returned by the 'Search' API.
--
-- /See:/ 'mkTrialComponent' smart constructor.
data TrialComponent = TrialComponent'
  { -- | When the component was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe TrialComponentStatus,
    -- | Details of the source of the component.
    sourceDetail :: Lude.Maybe TrialComponentSourceDetail,
    -- | The metrics for the component.
    metrics :: Lude.Maybe [TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)),
    -- | When the component started.
    startTime :: Lude.Maybe Lude.Timestamp,
    createdBy :: Lude.Maybe UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
    parents :: Lude.Maybe [Parent],
    -- | When the component ended.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the trial component.
    trialComponentName :: Lude.Maybe Lude.Text,
    -- | The hyperparameters of the component.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)),
    -- | The Amazon Resource Name (ARN) and job type of the source of the component.
    source :: Lude.Maybe TrialComponentSource,
    -- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentARN :: Lude.Maybe Lude.Text,
    -- | The input artifacts of the component.
    inputArtifacts :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)),
    -- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponent' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the component was created.
-- * 'status' -
-- * 'sourceDetail' - Details of the source of the component.
-- * 'metrics' - The metrics for the component.
-- * 'outputArtifacts' - The output artifacts of the component.
-- * 'startTime' - When the component started.
-- * 'createdBy' -
-- * 'lastModifiedTime' - When the component was last modified.
-- * 'parents' - An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
-- * 'endTime' - When the component ended.
-- * 'trialComponentName' - The name of the trial component.
-- * 'parameters' - The hyperparameters of the component.
-- * 'source' - The Amazon Resource Name (ARN) and job type of the source of the component.
-- * 'displayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'lastModifiedBy' -
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
-- * 'inputArtifacts' - The input artifacts of the component.
-- * 'tags' - The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
mkTrialComponent ::
  TrialComponent
mkTrialComponent =
  TrialComponent'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      sourceDetail = Lude.Nothing,
      metrics = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      startTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      parents = Lude.Nothing,
      endTime = Lude.Nothing,
      trialComponentName = Lude.Nothing,
      parameters = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreationTime :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Timestamp)
tcCreationTime = Lens.lens (creationTime :: TrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: TrialComponent)
{-# DEPRECATED tcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStatus :: Lens.Lens' TrialComponent (Lude.Maybe TrialComponentStatus)
tcStatus = Lens.lens (status :: TrialComponent -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: TrialComponent)
{-# DEPRECATED tcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Details of the source of the component.
--
-- /Note:/ Consider using 'sourceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSourceDetail :: Lens.Lens' TrialComponent (Lude.Maybe TrialComponentSourceDetail)
tcSourceDetail = Lens.lens (sourceDetail :: TrialComponent -> Lude.Maybe TrialComponentSourceDetail) (\s a -> s {sourceDetail = a} :: TrialComponent)
{-# DEPRECATED tcSourceDetail "Use generic-lens or generic-optics with 'sourceDetail' instead." #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMetrics :: Lens.Lens' TrialComponent (Lude.Maybe [TrialComponentMetricSummary])
tcMetrics = Lens.lens (metrics :: TrialComponent -> Lude.Maybe [TrialComponentMetricSummary]) (\s a -> s {metrics = a} :: TrialComponent)
{-# DEPRECATED tcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcOutputArtifacts :: Lens.Lens' TrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
tcOutputArtifacts = Lens.lens (outputArtifacts :: TrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {outputArtifacts = a} :: TrialComponent)
{-# DEPRECATED tcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStartTime :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Timestamp)
tcStartTime = Lens.lens (startTime :: TrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: TrialComponent)
{-# DEPRECATED tcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcCreatedBy :: Lens.Lens' TrialComponent (Lude.Maybe UserContext)
tcCreatedBy = Lens.lens (createdBy :: TrialComponent -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: TrialComponent)
{-# DEPRECATED tcCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedTime :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Timestamp)
tcLastModifiedTime = Lens.lens (lastModifiedTime :: TrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrialComponent)
{-# DEPRECATED tcLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | An array of the parents of the component. A parent is a trial the component is associated with and the experiment the trial is part of. A component might not have any parents.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParents :: Lens.Lens' TrialComponent (Lude.Maybe [Parent])
tcParents = Lens.lens (parents :: TrialComponent -> Lude.Maybe [Parent]) (\s a -> s {parents = a} :: TrialComponent)
{-# DEPRECATED tcParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndTime :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Timestamp)
tcEndTime = Lens.lens (endTime :: TrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: TrialComponent)
{-# DEPRECATED tcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentName :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Text)
tcTrialComponentName = Lens.lens (trialComponentName :: TrialComponent -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: TrialComponent)
{-# DEPRECATED tcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcParameters :: Lens.Lens' TrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)))
tcParameters = Lens.lens (parameters :: TrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue))) (\s a -> s {parameters = a} :: TrialComponent)
{-# DEPRECATED tcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The Amazon Resource Name (ARN) and job type of the source of the component.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TrialComponent (Lude.Maybe TrialComponentSource)
tcSource = Lens.lens (source :: TrialComponent -> Lude.Maybe TrialComponentSource) (\s a -> s {source = a} :: TrialComponent)
{-# DEPRECATED tcSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcDisplayName :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Text)
tcDisplayName = Lens.lens (displayName :: TrialComponent -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: TrialComponent)
{-# DEPRECATED tcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcLastModifiedBy :: Lens.Lens' TrialComponent (Lude.Maybe UserContext)
tcLastModifiedBy = Lens.lens (lastModifiedBy :: TrialComponent -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: TrialComponent)
{-# DEPRECATED tcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTrialComponentARN :: Lens.Lens' TrialComponent (Lude.Maybe Lude.Text)
tcTrialComponentARN = Lens.lens (trialComponentARN :: TrialComponent -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: TrialComponent)
{-# DEPRECATED tcTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInputArtifacts :: Lens.Lens' TrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
tcInputArtifacts = Lens.lens (inputArtifacts :: TrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {inputArtifacts = a} :: TrialComponent)
{-# DEPRECATED tcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The list of tags that are associated with the component. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTags :: Lens.Lens' TrialComponent (Lude.Maybe [Tag])
tcTags = Lens.lens (tags :: TrialComponent -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TrialComponent)
{-# DEPRECATED tcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON TrialComponent where
  parseJSON =
    Lude.withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "SourceDetail")
            Lude.<*> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OutputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "Parents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "TrialComponentName")
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "TrialComponentArn")
            Lude.<*> (x Lude..:? "InputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
