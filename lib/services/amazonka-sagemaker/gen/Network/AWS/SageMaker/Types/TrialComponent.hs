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
import qualified Network.AWS.Prelude as Prelude
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
  { -- | When the component was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    status :: Prelude.Maybe TrialComponentStatus,
    -- | Details of the source of the component.
    sourceDetail :: Prelude.Maybe TrialComponentSourceDetail,
    -- | The metrics for the component.
    metrics :: Prelude.Maybe [TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Who created the trial component.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | An array of the parents of the component. A parent is a trial the
    -- component is associated with and the experiment the trial is part of. A
    -- component might not have any parents.
    parents :: Prelude.Maybe [Parent],
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text,
    -- | The hyperparameters of the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The Amazon Resource Name (ARN) and job type of the source of the
    -- component.
    source :: Prelude.Maybe TrialComponentSource,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The input artifacts of the component.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The list of tags that are associated with the component. You can use
    -- Search API to search on the tags.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'trialComponent_creationTime' - When the component was created.
--
-- 'metadataProperties', 'trialComponent_metadataProperties' - Undocumented member.
--
-- 'status', 'trialComponent_status' - Undocumented member.
--
-- 'sourceDetail', 'trialComponent_sourceDetail' - Details of the source of the component.
--
-- 'metrics', 'trialComponent_metrics' - The metrics for the component.
--
-- 'outputArtifacts', 'trialComponent_outputArtifacts' - The output artifacts of the component.
--
-- 'startTime', 'trialComponent_startTime' - When the component started.
--
-- 'createdBy', 'trialComponent_createdBy' - Who created the trial component.
--
-- 'lastModifiedTime', 'trialComponent_lastModifiedTime' - When the component was last modified.
--
-- 'parents', 'trialComponent_parents' - An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
--
-- 'endTime', 'trialComponent_endTime' - When the component ended.
--
-- 'trialComponentName', 'trialComponent_trialComponentName' - The name of the trial component.
--
-- 'parameters', 'trialComponent_parameters' - The hyperparameters of the component.
--
-- 'source', 'trialComponent_source' - The Amazon Resource Name (ARN) and job type of the source of the
-- component.
--
-- 'displayName', 'trialComponent_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'lastModifiedBy', 'trialComponent_lastModifiedBy' - Undocumented member.
--
-- 'trialComponentArn', 'trialComponent_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'inputArtifacts', 'trialComponent_inputArtifacts' - The input artifacts of the component.
--
-- 'tags', 'trialComponent_tags' - The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
newTrialComponent ::
  TrialComponent
newTrialComponent =
  TrialComponent'
    { creationTime = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceDetail = Prelude.Nothing,
      metrics = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      startTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parents = Prelude.Nothing,
      endTime = Prelude.Nothing,
      trialComponentName = Prelude.Nothing,
      parameters = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | When the component was created.
trialComponent_creationTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_creationTime = Lens.lens (\TrialComponent' {creationTime} -> creationTime) (\s@TrialComponent' {} a -> s {creationTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
trialComponent_metadataProperties :: Lens.Lens' TrialComponent (Prelude.Maybe MetadataProperties)
trialComponent_metadataProperties = Lens.lens (\TrialComponent' {metadataProperties} -> metadataProperties) (\s@TrialComponent' {} a -> s {metadataProperties = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_status :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentStatus)
trialComponent_status = Lens.lens (\TrialComponent' {status} -> status) (\s@TrialComponent' {} a -> s {status = a} :: TrialComponent)

-- | Details of the source of the component.
trialComponent_sourceDetail :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSourceDetail)
trialComponent_sourceDetail = Lens.lens (\TrialComponent' {sourceDetail} -> sourceDetail) (\s@TrialComponent' {} a -> s {sourceDetail = a} :: TrialComponent)

-- | The metrics for the component.
trialComponent_metrics :: Lens.Lens' TrialComponent (Prelude.Maybe [TrialComponentMetricSummary])
trialComponent_metrics = Lens.lens (\TrialComponent' {metrics} -> metrics) (\s@TrialComponent' {} a -> s {metrics = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The output artifacts of the component.
trialComponent_outputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_outputArtifacts = Lens.lens (\TrialComponent' {outputArtifacts} -> outputArtifacts) (\s@TrialComponent' {} a -> s {outputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
trialComponent_startTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_startTime = Lens.lens (\TrialComponent' {startTime} -> startTime) (\s@TrialComponent' {} a -> s {startTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | Who created the trial component.
trialComponent_createdBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_createdBy = Lens.lens (\TrialComponent' {createdBy} -> createdBy) (\s@TrialComponent' {} a -> s {createdBy = a} :: TrialComponent)

-- | When the component was last modified.
trialComponent_lastModifiedTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_lastModifiedTime = Lens.lens (\TrialComponent' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponent' {} a -> s {lastModifiedTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
trialComponent_parents :: Lens.Lens' TrialComponent (Prelude.Maybe [Parent])
trialComponent_parents = Lens.lens (\TrialComponent' {parents} -> parents) (\s@TrialComponent' {} a -> s {parents = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component ended.
trialComponent_endTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_endTime = Lens.lens (\TrialComponent' {endTime} -> endTime) (\s@TrialComponent' {} a -> s {endTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | The name of the trial component.
trialComponent_trialComponentName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentName = Lens.lens (\TrialComponent' {trialComponentName} -> trialComponentName) (\s@TrialComponent' {} a -> s {trialComponentName = a} :: TrialComponent)

-- | The hyperparameters of the component.
trialComponent_parameters :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
trialComponent_parameters = Lens.lens (\TrialComponent' {parameters} -> parameters) (\s@TrialComponent' {} a -> s {parameters = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) and job type of the source of the
-- component.
trialComponent_source :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSource)
trialComponent_source = Lens.lens (\TrialComponent' {source} -> source) (\s@TrialComponent' {} a -> s {source = a} :: TrialComponent)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponent_displayName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_displayName = Lens.lens (\TrialComponent' {displayName} -> displayName) (\s@TrialComponent' {} a -> s {displayName = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_lastModifiedBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_lastModifiedBy = Lens.lens (\TrialComponent' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponent' {} a -> s {lastModifiedBy = a} :: TrialComponent)

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponent_trialComponentArn :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentArn = Lens.lens (\TrialComponent' {trialComponentArn} -> trialComponentArn) (\s@TrialComponent' {} a -> s {trialComponentArn = a} :: TrialComponent)

-- | The input artifacts of the component.
trialComponent_inputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_inputArtifacts = Lens.lens (\TrialComponent' {inputArtifacts} -> inputArtifacts) (\s@TrialComponent' {} a -> s {inputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
trialComponent_tags :: Lens.Lens' TrialComponent (Prelude.Maybe [Tag])
trialComponent_tags = Lens.lens (\TrialComponent' {tags} -> tags) (\s@TrialComponent' {} a -> s {tags = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TrialComponent where
  parseJSON =
    Core.withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "MetadataProperties")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SourceDetail")
            Prelude.<*> (x Core..:? "Metrics" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "OutputArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "Parents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "TrialComponentName")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "TrialComponentArn")
            Prelude.<*> (x Core..:? "InputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TrialComponent

instance Prelude.NFData TrialComponent
