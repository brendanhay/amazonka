{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentArtifact
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial component as returned by the Search API.
--
-- /See:/ 'newTrialComponent' smart constructor.
data TrialComponent = TrialComponent'
  { -- | An array of the parents of the component. A parent is a trial the
    -- component is associated with and the experiment the trial is part of. A
    -- component might not have any parents.
    parents :: Core.Maybe [Parent],
    status :: Core.Maybe TrialComponentStatus,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the component was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | Details of the source of the component.
    sourceDetail :: Core.Maybe TrialComponentSourceDetail,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    -- | When the component started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) and job type of the source of the
    -- component.
    source :: Core.Maybe TrialComponentSource,
    -- | When the component ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | The metrics for the component.
    metrics :: Core.Maybe [TrialComponentMetricSummary],
    -- | The list of tags that are associated with the component. You can use
    -- Search API to search on the tags.
    tags :: Core.Maybe [Tag],
    -- | When the component was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The input artifacts of the component.
    inputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    createdBy :: Core.Maybe UserContext,
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The hyperparameters of the component.
    parameters :: Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue),
    -- | The output artifacts of the component.
    outputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parents', 'trialComponent_parents' - An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
--
-- 'status', 'trialComponent_status' - Undocumented member.
--
-- 'metadataProperties', 'trialComponent_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'trialComponent_creationTime' - When the component was created.
--
-- 'sourceDetail', 'trialComponent_sourceDetail' - Details of the source of the component.
--
-- 'trialComponentArn', 'trialComponent_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'startTime', 'trialComponent_startTime' - When the component started.
--
-- 'source', 'trialComponent_source' - The Amazon Resource Name (ARN) and job type of the source of the
-- component.
--
-- 'endTime', 'trialComponent_endTime' - When the component ended.
--
-- 'metrics', 'trialComponent_metrics' - The metrics for the component.
--
-- 'tags', 'trialComponent_tags' - The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
--
-- 'lastModifiedTime', 'trialComponent_lastModifiedTime' - When the component was last modified.
--
-- 'inputArtifacts', 'trialComponent_inputArtifacts' - The input artifacts of the component.
--
-- 'createdBy', 'trialComponent_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'trialComponent_lastModifiedBy' - Undocumented member.
--
-- 'displayName', 'trialComponent_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'parameters', 'trialComponent_parameters' - The hyperparameters of the component.
--
-- 'outputArtifacts', 'trialComponent_outputArtifacts' - The output artifacts of the component.
--
-- 'trialComponentName', 'trialComponent_trialComponentName' - The name of the trial component.
newTrialComponent ::
  TrialComponent
newTrialComponent =
  TrialComponent'
    { parents = Core.Nothing,
      status = Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      sourceDetail = Core.Nothing,
      trialComponentArn = Core.Nothing,
      startTime = Core.Nothing,
      source = Core.Nothing,
      endTime = Core.Nothing,
      metrics = Core.Nothing,
      tags = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing,
      parameters = Core.Nothing,
      outputArtifacts = Core.Nothing,
      trialComponentName = Core.Nothing
    }

-- | An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
trialComponent_parents :: Lens.Lens' TrialComponent (Core.Maybe [Parent])
trialComponent_parents = Lens.lens (\TrialComponent' {parents} -> parents) (\s@TrialComponent' {} a -> s {parents = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
trialComponent_status :: Lens.Lens' TrialComponent (Core.Maybe TrialComponentStatus)
trialComponent_status = Lens.lens (\TrialComponent' {status} -> status) (\s@TrialComponent' {} a -> s {status = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_metadataProperties :: Lens.Lens' TrialComponent (Core.Maybe MetadataProperties)
trialComponent_metadataProperties = Lens.lens (\TrialComponent' {metadataProperties} -> metadataProperties) (\s@TrialComponent' {} a -> s {metadataProperties = a} :: TrialComponent)

-- | When the component was created.
trialComponent_creationTime :: Lens.Lens' TrialComponent (Core.Maybe Core.UTCTime)
trialComponent_creationTime = Lens.lens (\TrialComponent' {creationTime} -> creationTime) (\s@TrialComponent' {} a -> s {creationTime = a} :: TrialComponent) Core.. Lens.mapping Core._Time

-- | Details of the source of the component.
trialComponent_sourceDetail :: Lens.Lens' TrialComponent (Core.Maybe TrialComponentSourceDetail)
trialComponent_sourceDetail = Lens.lens (\TrialComponent' {sourceDetail} -> sourceDetail) (\s@TrialComponent' {} a -> s {sourceDetail = a} :: TrialComponent)

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponent_trialComponentArn :: Lens.Lens' TrialComponent (Core.Maybe Core.Text)
trialComponent_trialComponentArn = Lens.lens (\TrialComponent' {trialComponentArn} -> trialComponentArn) (\s@TrialComponent' {} a -> s {trialComponentArn = a} :: TrialComponent)

-- | When the component started.
trialComponent_startTime :: Lens.Lens' TrialComponent (Core.Maybe Core.UTCTime)
trialComponent_startTime = Lens.lens (\TrialComponent' {startTime} -> startTime) (\s@TrialComponent' {} a -> s {startTime = a} :: TrialComponent) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) and job type of the source of the
-- component.
trialComponent_source :: Lens.Lens' TrialComponent (Core.Maybe TrialComponentSource)
trialComponent_source = Lens.lens (\TrialComponent' {source} -> source) (\s@TrialComponent' {} a -> s {source = a} :: TrialComponent)

-- | When the component ended.
trialComponent_endTime :: Lens.Lens' TrialComponent (Core.Maybe Core.UTCTime)
trialComponent_endTime = Lens.lens (\TrialComponent' {endTime} -> endTime) (\s@TrialComponent' {} a -> s {endTime = a} :: TrialComponent) Core.. Lens.mapping Core._Time

-- | The metrics for the component.
trialComponent_metrics :: Lens.Lens' TrialComponent (Core.Maybe [TrialComponentMetricSummary])
trialComponent_metrics = Lens.lens (\TrialComponent' {metrics} -> metrics) (\s@TrialComponent' {} a -> s {metrics = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
trialComponent_tags :: Lens.Lens' TrialComponent (Core.Maybe [Tag])
trialComponent_tags = Lens.lens (\TrialComponent' {tags} -> tags) (\s@TrialComponent' {} a -> s {tags = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | When the component was last modified.
trialComponent_lastModifiedTime :: Lens.Lens' TrialComponent (Core.Maybe Core.UTCTime)
trialComponent_lastModifiedTime = Lens.lens (\TrialComponent' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponent' {} a -> s {lastModifiedTime = a} :: TrialComponent) Core.. Lens.mapping Core._Time

-- | The input artifacts of the component.
trialComponent_inputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
trialComponent_inputArtifacts = Lens.lens (\TrialComponent' {inputArtifacts} -> inputArtifacts) (\s@TrialComponent' {} a -> s {inputArtifacts = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
trialComponent_createdBy :: Lens.Lens' TrialComponent (Core.Maybe UserContext)
trialComponent_createdBy = Lens.lens (\TrialComponent' {createdBy} -> createdBy) (\s@TrialComponent' {} a -> s {createdBy = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_lastModifiedBy :: Lens.Lens' TrialComponent (Core.Maybe UserContext)
trialComponent_lastModifiedBy = Lens.lens (\TrialComponent' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponent' {} a -> s {lastModifiedBy = a} :: TrialComponent)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponent_displayName :: Lens.Lens' TrialComponent (Core.Maybe Core.Text)
trialComponent_displayName = Lens.lens (\TrialComponent' {displayName} -> displayName) (\s@TrialComponent' {} a -> s {displayName = a} :: TrialComponent)

-- | The hyperparameters of the component.
trialComponent_parameters :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue))
trialComponent_parameters = Lens.lens (\TrialComponent' {parameters} -> parameters) (\s@TrialComponent' {} a -> s {parameters = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The output artifacts of the component.
trialComponent_outputArtifacts :: Lens.Lens' TrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
trialComponent_outputArtifacts = Lens.lens (\TrialComponent' {outputArtifacts} -> outputArtifacts) (\s@TrialComponent' {} a -> s {outputArtifacts = a} :: TrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The name of the trial component.
trialComponent_trialComponentName :: Lens.Lens' TrialComponent (Core.Maybe Core.Text)
trialComponent_trialComponentName = Lens.lens (\TrialComponent' {trialComponentName} -> trialComponentName) (\s@TrialComponent' {} a -> s {trialComponentName = a} :: TrialComponent)

instance Core.FromJSON TrialComponent where
  parseJSON =
    Core.withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            Core.<$> (x Core..:? "Parents" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "MetadataProperties")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "SourceDetail")
            Core.<*> (x Core..:? "TrialComponentArn")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "Source")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "InputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "LastModifiedBy")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "Parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "OutputArtifacts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "TrialComponentName")
      )

instance Core.Hashable TrialComponent

instance Core.NFData TrialComponent
