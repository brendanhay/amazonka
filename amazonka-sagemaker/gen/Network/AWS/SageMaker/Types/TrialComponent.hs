{-# LANGUAGE DeriveDataTypeable #-}
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
  { -- | An array of the parents of the component. A parent is a trial the
    -- component is associated with and the experiment the trial is part of. A
    -- component might not have any parents.
    parents :: Prelude.Maybe [Parent],
    status :: Prelude.Maybe TrialComponentStatus,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | When the component was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Details of the source of the component.
    sourceDetail :: Prelude.Maybe TrialComponentSourceDetail,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | When the component started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) and job type of the source of the
    -- component.
    source :: Prelude.Maybe TrialComponentSource,
    -- | When the component ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The metrics for the component.
    metrics :: Prelude.Maybe [TrialComponentMetricSummary],
    -- | The list of tags that are associated with the component. You can use
    -- Search API to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | When the component was last modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The input artifacts of the component.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The hyperparameters of the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The output artifacts of the component.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { parents = Prelude.Nothing,
      status = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      sourceDetail = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      source = Prelude.Nothing,
      endTime = Prelude.Nothing,
      metrics = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      displayName = Prelude.Nothing,
      parameters = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      trialComponentName = Prelude.Nothing
    }

-- | An array of the parents of the component. A parent is a trial the
-- component is associated with and the experiment the trial is part of. A
-- component might not have any parents.
trialComponent_parents :: Lens.Lens' TrialComponent (Prelude.Maybe [Parent])
trialComponent_parents = Lens.lens (\TrialComponent' {parents} -> parents) (\s@TrialComponent' {} a -> s {parents = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
trialComponent_status :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentStatus)
trialComponent_status = Lens.lens (\TrialComponent' {status} -> status) (\s@TrialComponent' {} a -> s {status = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_metadataProperties :: Lens.Lens' TrialComponent (Prelude.Maybe MetadataProperties)
trialComponent_metadataProperties = Lens.lens (\TrialComponent' {metadataProperties} -> metadataProperties) (\s@TrialComponent' {} a -> s {metadataProperties = a} :: TrialComponent)

-- | When the component was created.
trialComponent_creationTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_creationTime = Lens.lens (\TrialComponent' {creationTime} -> creationTime) (\s@TrialComponent' {} a -> s {creationTime = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Time

-- | Details of the source of the component.
trialComponent_sourceDetail :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSourceDetail)
trialComponent_sourceDetail = Lens.lens (\TrialComponent' {sourceDetail} -> sourceDetail) (\s@TrialComponent' {} a -> s {sourceDetail = a} :: TrialComponent)

-- | The Amazon Resource Name (ARN) of the trial component.
trialComponent_trialComponentArn :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentArn = Lens.lens (\TrialComponent' {trialComponentArn} -> trialComponentArn) (\s@TrialComponent' {} a -> s {trialComponentArn = a} :: TrialComponent)

-- | When the component started.
trialComponent_startTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_startTime = Lens.lens (\TrialComponent' {startTime} -> startTime) (\s@TrialComponent' {} a -> s {startTime = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) and job type of the source of the
-- component.
trialComponent_source :: Lens.Lens' TrialComponent (Prelude.Maybe TrialComponentSource)
trialComponent_source = Lens.lens (\TrialComponent' {source} -> source) (\s@TrialComponent' {} a -> s {source = a} :: TrialComponent)

-- | When the component ended.
trialComponent_endTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_endTime = Lens.lens (\TrialComponent' {endTime} -> endTime) (\s@TrialComponent' {} a -> s {endTime = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Time

-- | The metrics for the component.
trialComponent_metrics :: Lens.Lens' TrialComponent (Prelude.Maybe [TrialComponentMetricSummary])
trialComponent_metrics = Lens.lens (\TrialComponent' {metrics} -> metrics) (\s@TrialComponent' {} a -> s {metrics = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of tags that are associated with the component. You can use
-- Search API to search on the tags.
trialComponent_tags :: Lens.Lens' TrialComponent (Prelude.Maybe [Tag])
trialComponent_tags = Lens.lens (\TrialComponent' {tags} -> tags) (\s@TrialComponent' {} a -> s {tags = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | When the component was last modified.
trialComponent_lastModifiedTime :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.UTCTime)
trialComponent_lastModifiedTime = Lens.lens (\TrialComponent' {lastModifiedTime} -> lastModifiedTime) (\s@TrialComponent' {} a -> s {lastModifiedTime = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Time

-- | The input artifacts of the component.
trialComponent_inputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_inputArtifacts = Lens.lens (\TrialComponent' {inputArtifacts} -> inputArtifacts) (\s@TrialComponent' {} a -> s {inputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
trialComponent_createdBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_createdBy = Lens.lens (\TrialComponent' {createdBy} -> createdBy) (\s@TrialComponent' {} a -> s {createdBy = a} :: TrialComponent)

-- | Undocumented member.
trialComponent_lastModifiedBy :: Lens.Lens' TrialComponent (Prelude.Maybe UserContext)
trialComponent_lastModifiedBy = Lens.lens (\TrialComponent' {lastModifiedBy} -> lastModifiedBy) (\s@TrialComponent' {} a -> s {lastModifiedBy = a} :: TrialComponent)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
trialComponent_displayName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_displayName = Lens.lens (\TrialComponent' {displayName} -> displayName) (\s@TrialComponent' {} a -> s {displayName = a} :: TrialComponent)

-- | The hyperparameters of the component.
trialComponent_parameters :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
trialComponent_parameters = Lens.lens (\TrialComponent' {parameters} -> parameters) (\s@TrialComponent' {} a -> s {parameters = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | The output artifacts of the component.
trialComponent_outputArtifacts :: Lens.Lens' TrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
trialComponent_outputArtifacts = Lens.lens (\TrialComponent' {outputArtifacts} -> outputArtifacts) (\s@TrialComponent' {} a -> s {outputArtifacts = a} :: TrialComponent) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the trial component.
trialComponent_trialComponentName :: Lens.Lens' TrialComponent (Prelude.Maybe Prelude.Text)
trialComponent_trialComponentName = Lens.lens (\TrialComponent' {trialComponentName} -> trialComponentName) (\s@TrialComponent' {} a -> s {trialComponentName = a} :: TrialComponent)

instance Prelude.FromJSON TrialComponent where
  parseJSON =
    Prelude.withObject
      "TrialComponent"
      ( \x ->
          TrialComponent'
            Prelude.<$> (x Prelude..:? "Parents" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "MetadataProperties")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "SourceDetail")
            Prelude.<*> (x Prelude..:? "TrialComponentArn")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> (x Prelude..:? "EndTime")
            Prelude.<*> (x Prelude..:? "Metrics" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> ( x Prelude..:? "InputArtifacts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> ( x Prelude..:? "Parameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "OutputArtifacts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TrialComponentName")
      )

instance Prelude.Hashable TrialComponent

instance Prelude.NFData TrialComponent
