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
-- Module      : Amazonka.SageMaker.Types.TrialComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.Parent
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TrialComponentArtifact
import Amazonka.SageMaker.Types.TrialComponentMetricSummary
import Amazonka.SageMaker.Types.TrialComponentParameterValue
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.TrialComponentSourceDetail
import Amazonka.SageMaker.Types.TrialComponentStatus
import Amazonka.SageMaker.Types.UserContext

-- | The properties of a trial component as returned by the Search API.
--
-- /See:/ 'newTrialComponent' smart constructor.
data TrialComponent = TrialComponent'
  { -- | The list of tags that are associated with the component. You can use
    -- Search API to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text,
    -- | Details of the source of the component.
    sourceDetail :: Prelude.Maybe TrialComponentSourceDetail,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe TrialComponentStatus,
    -- | The metrics for the component.
    metrics :: Prelude.Maybe [TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | When the component was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) and job type of the source of the
    -- component.
    source :: Prelude.Maybe TrialComponentSource,
    -- | An array of the parents of the component. A parent is a trial the
    -- component is associated with and the experiment the trial is part of. A
    -- component might not have any parents.
    parents :: Prelude.Maybe [Parent],
    -- | The Amazon Resource Name (ARN) of the lineage group resource.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | When the component was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The input artifacts of the component.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | Who created the trial component.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The hyperparameters of the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue)
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
-- 'tags', 'trialComponent_tags' - The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
--
-- 'trialComponentArn', 'trialComponent_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'trialComponentName', 'trialComponent_trialComponentName' - The name of the trial component.
--
-- 'sourceDetail', 'trialComponent_sourceDetail' - Details of the source of the component.
--
-- 'metadataProperties', 'trialComponent_metadataProperties' - Undocumented member.
--
-- 'displayName', 'trialComponent_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'status', 'trialComponent_status' - Undocumented member.
--
-- 'metrics', 'trialComponent_metrics' - The metrics for the component.
--
-- 'outputArtifacts', 'trialComponent_outputArtifacts' - The output artifacts of the component.
--
-- 'endTime', 'trialComponent_endTime' - When the component ended.
--
-- 'lastModifiedTime', 'trialComponent_lastModifiedTime' - When the component was last modified.
--
-- 'source', 'trialComponent_source' - The Amazon Resource Name (ARN) and job type of the source of the
-- component.
--
-- 'parents', 'trialComponent_parents' - An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
--
-- 'lineageGroupArn', 'trialComponent_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group resource.
--
-- 'creationTime', 'trialComponent_creationTime' - When the component was created.
--
-- 'inputArtifacts', 'trialComponent_inputArtifacts' - The input artifacts of the component.
--
-- 'lastModifiedBy', 'trialComponent_lastModifiedBy' - Undocumented member.
--
-- 'createdBy', 'trialComponent_createdBy' - Who created the trial component.
--
-- 'startTime', 'trialComponent_startTime' - When the component started.
--
-- 'parameters', 'trialComponent_parameters' - The hyperparameters of the component.
newTrialComponent ::
  TrialComponent
newTrialComponent =
  TrialComponent'
    { tags = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      trialComponentName = Prelude.Nothing,
      sourceDetail = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      displayName = Prelude.Nothing,
      status = Prelude.Nothing,
      metrics = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      endTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      source = Prelude.Nothing,
      parents = Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      startTime = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
trialComponent_tags :: Lens.Lens' TrialComponent (Prelude.Maybe [Tag])
trialComponent_tags = Lens.lens (\TrialComponent' {tags} -> tags) (\s@TrialComponent' {} a -> s {tags = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponent_trialComponentArn :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentArn = Lens.lens (\TrialComponent' {trialComponentArn} -> trialComponentArn) (\s@TrialComponent' {} a -> s {trialComponentArn = a} :: TrialComponent)

-- | The name of the trial component.
trialComponent_trialComponentName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentName = Lens.lens (\TrialComponent' {trialComponentName} -> trialComponentName) (\s@TrialComponent' {} a -> s {trialComponentName = a} :: TrialComponent)

-- | Details of the source of the component.
trialComponent_sourceDetail :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSourceDetail)
trialComponent_sourceDetail = Lens.lens (\TrialComponent' {sourceDetail} -> sourceDetail) (\s@TrialComponent' {} a -> s {sourceDetail = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_metadataProperties :: Lens.Lens' TrialComponent (Prelude.Maybe MetadataProperties)
trialComponent_metadataProperties = Lens.lens (\TrialComponent' {metadataProperties} -> metadataProperties) (\s@TrialComponent' {} a -> s {metadataProperties = a} :: TrialComponent)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponent_displayName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_displayName = Lens.lens (\TrialComponent' {displayName} -> displayName) (\s@TrialComponent' {} a -> s {displayName = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_status :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentStatus)
trialComponent_status = Lens.lens (\TrialComponent' {status} -> status) (\s@TrialComponent' {} a -> s {status = a} :: TrialComponent)

-- | The metrics for the component.
trialComponent_metrics :: Lens.Lens' TrialComponent (Prelude.Maybe [TrialComponentMetricSummary])
trialComponent_metrics = Lens.lens (\TrialComponent' {metrics} -> metrics) (\s@TrialComponent' {} a -> s {metrics = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The output artifacts of the component.
trialComponent_outputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_outputArtifacts = Lens.lens (\TrialComponent' {outputArtifacts} -> outputArtifacts) (\s@TrialComponent' {} a -> s {outputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component ended.
trialComponent_endTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_endTime = Lens.lens (\TrialComponent' {endTime} -> endTime) (\s@TrialComponent' {} a -> s {endTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | When the component was last modified.
trialComponent_lastModifiedTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_lastModifiedTime = Lens.lens (\TrialComponent' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponent' {} a -> s {lastModifiedTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) and job type of the source of the
-- component.
trialComponent_source :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSource)
trialComponent_source = Lens.lens (\TrialComponent' {source} -> source) (\s@TrialComponent' {} a -> s {source = a} :: TrialComponent)

-- | An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
trialComponent_parents :: Lens.Lens' TrialComponent (Prelude.Maybe [Parent])
trialComponent_parents = Lens.lens (\TrialComponent' {parents} -> parents) (\s@TrialComponent' {} a -> s {parents = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the lineage group resource.
trialComponent_lineageGroupArn :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_lineageGroupArn = Lens.lens (\TrialComponent' {lineageGroupArn} -> lineageGroupArn) (\s@TrialComponent' {} a -> s {lineageGroupArn = a} :: TrialComponent)

-- | When the component was created.
trialComponent_creationTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_creationTime = Lens.lens (\TrialComponent' {creationTime} -> creationTime) (\s@TrialComponent' {} a -> s {creationTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | The input artifacts of the component.
trialComponent_inputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_inputArtifacts = Lens.lens (\TrialComponent' {inputArtifacts} -> inputArtifacts) (\s@TrialComponent' {} a -> s {inputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
trialComponent_lastModifiedBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_lastModifiedBy = Lens.lens (\TrialComponent' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponent' {} a -> s {lastModifiedBy = a} :: TrialComponent)

-- | Who created the trial component.
trialComponent_createdBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_createdBy = Lens.lens (\TrialComponent' {createdBy} -> createdBy) (\s@TrialComponent' {} a -> s {createdBy = a} :: TrialComponent)

-- | When the component started.
trialComponent_startTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_startTime = Lens.lens (\TrialComponent' {startTime} -> startTime) (\s@TrialComponent' {} a -> s {startTime = a} :: TrialComponent) Prelude.. Lens.mapping Core._Time

-- | The hyperparameters of the component.
trialComponent_parameters :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
trialComponent_parameters = Lens.lens (\TrialComponent' {parameters} -> parameters) (\s@TrialComponent' {} a -> s {parameters = a} :: TrialComponent) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TrialComponent where
  parseJSON =
    Core.withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TrialComponentArn")
            Prelude.<*> (x Core..:? "TrialComponentName")
            Prelude.<*> (x Core..:? "SourceDetail")
            Prelude.<*> (x Core..:? "MetadataProperties")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Metrics" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "OutputArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "Parents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LineageGroupArn")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "InputArtifacts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TrialComponent where
  hashWithSalt _salt TrialComponent' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trialComponentArn
      `Prelude.hashWithSalt` trialComponentName
      `Prelude.hashWithSalt` sourceDetail
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` parents
      `Prelude.hashWithSalt` lineageGroupArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData TrialComponent where
  rnf TrialComponent' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf sourceDetail
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf parents
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf parameters
