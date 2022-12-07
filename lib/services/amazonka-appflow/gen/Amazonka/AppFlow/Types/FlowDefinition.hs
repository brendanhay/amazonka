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
-- Module      : Amazonka.AppFlow.Types.FlowDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.FlowDefinition where

import Amazonka.AppFlow.Types.ConnectorType
import Amazonka.AppFlow.Types.ExecutionDetails
import Amazonka.AppFlow.Types.FlowStatus
import Amazonka.AppFlow.Types.TriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties of the flow, such as its source, destination, trigger
-- type, and so on.
--
-- /See:/ 'newFlowDefinition' smart constructor.
data FlowDefinition = FlowDefinition'
  { -- | The tags used to organize, track, or control access for your flow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The label of the destination connector in the flow.
    destinationConnectorLabel :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the flow was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Maybe Prelude.Text,
    -- | The label of the source connector in the flow.
    sourceConnectorLabel :: Prelude.Maybe Prelude.Text,
    -- | A user-entered description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the source connector type, such as Salesforce, Amazon S3,
    -- Amplitude, and so on.
    sourceConnectorType :: Prelude.Maybe ConnectorType,
    -- | Describes the details of the most recent flow run.
    lastRunExecutionDetails :: Prelude.Maybe ExecutionDetails,
    -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
    -- or @Event@.
    triggerType :: Prelude.Maybe TriggerType,
    -- | Specifies the destination connector type, such as Salesforce, Amazon S3,
    -- Amplitude, and so on.
    destinationConnectorType :: Prelude.Maybe ConnectorType,
    -- | The ARN of the user who created the flow.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The flow\'s Amazon Resource Name (ARN).
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the flow was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Specifies the account user name that most recently updated the flow.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'flowDefinition_tags' - The tags used to organize, track, or control access for your flow.
--
-- 'destinationConnectorLabel', 'flowDefinition_destinationConnectorLabel' - The label of the destination connector in the flow.
--
-- 'lastUpdatedAt', 'flowDefinition_lastUpdatedAt' - Specifies when the flow was last updated.
--
-- 'flowName', 'flowDefinition_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
--
-- 'sourceConnectorLabel', 'flowDefinition_sourceConnectorLabel' - The label of the source connector in the flow.
--
-- 'description', 'flowDefinition_description' - A user-entered description of the flow.
--
-- 'sourceConnectorType', 'flowDefinition_sourceConnectorType' - Specifies the source connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
--
-- 'lastRunExecutionDetails', 'flowDefinition_lastRunExecutionDetails' - Describes the details of the most recent flow run.
--
-- 'flowStatus', 'flowDefinition_flowStatus' - Indicates the current status of the flow.
--
-- 'triggerType', 'flowDefinition_triggerType' - Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
--
-- 'destinationConnectorType', 'flowDefinition_destinationConnectorType' - Specifies the destination connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
--
-- 'createdBy', 'flowDefinition_createdBy' - The ARN of the user who created the flow.
--
-- 'flowArn', 'flowDefinition_flowArn' - The flow\'s Amazon Resource Name (ARN).
--
-- 'createdAt', 'flowDefinition_createdAt' - Specifies when the flow was created.
--
-- 'lastUpdatedBy', 'flowDefinition_lastUpdatedBy' - Specifies the account user name that most recently updated the flow.
newFlowDefinition ::
  FlowDefinition
newFlowDefinition =
  FlowDefinition'
    { tags = Prelude.Nothing,
      destinationConnectorLabel = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      flowName = Prelude.Nothing,
      sourceConnectorLabel = Prelude.Nothing,
      description = Prelude.Nothing,
      sourceConnectorType = Prelude.Nothing,
      lastRunExecutionDetails = Prelude.Nothing,
      flowStatus = Prelude.Nothing,
      triggerType = Prelude.Nothing,
      destinationConnectorType = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing
    }

-- | The tags used to organize, track, or control access for your flow.
flowDefinition_tags :: Lens.Lens' FlowDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
flowDefinition_tags = Lens.lens (\FlowDefinition' {tags} -> tags) (\s@FlowDefinition' {} a -> s {tags = a} :: FlowDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The label of the destination connector in the flow.
flowDefinition_destinationConnectorLabel :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_destinationConnectorLabel = Lens.lens (\FlowDefinition' {destinationConnectorLabel} -> destinationConnectorLabel) (\s@FlowDefinition' {} a -> s {destinationConnectorLabel = a} :: FlowDefinition)

-- | Specifies when the flow was last updated.
flowDefinition_lastUpdatedAt :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.UTCTime)
flowDefinition_lastUpdatedAt = Lens.lens (\FlowDefinition' {lastUpdatedAt} -> lastUpdatedAt) (\s@FlowDefinition' {} a -> s {lastUpdatedAt = a} :: FlowDefinition) Prelude.. Lens.mapping Data._Time

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
flowDefinition_flowName :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_flowName = Lens.lens (\FlowDefinition' {flowName} -> flowName) (\s@FlowDefinition' {} a -> s {flowName = a} :: FlowDefinition)

-- | The label of the source connector in the flow.
flowDefinition_sourceConnectorLabel :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_sourceConnectorLabel = Lens.lens (\FlowDefinition' {sourceConnectorLabel} -> sourceConnectorLabel) (\s@FlowDefinition' {} a -> s {sourceConnectorLabel = a} :: FlowDefinition)

-- | A user-entered description of the flow.
flowDefinition_description :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_description = Lens.lens (\FlowDefinition' {description} -> description) (\s@FlowDefinition' {} a -> s {description = a} :: FlowDefinition)

-- | Specifies the source connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
flowDefinition_sourceConnectorType :: Lens.Lens' FlowDefinition (Prelude.Maybe ConnectorType)
flowDefinition_sourceConnectorType = Lens.lens (\FlowDefinition' {sourceConnectorType} -> sourceConnectorType) (\s@FlowDefinition' {} a -> s {sourceConnectorType = a} :: FlowDefinition)

-- | Describes the details of the most recent flow run.
flowDefinition_lastRunExecutionDetails :: Lens.Lens' FlowDefinition (Prelude.Maybe ExecutionDetails)
flowDefinition_lastRunExecutionDetails = Lens.lens (\FlowDefinition' {lastRunExecutionDetails} -> lastRunExecutionDetails) (\s@FlowDefinition' {} a -> s {lastRunExecutionDetails = a} :: FlowDefinition)

-- | Indicates the current status of the flow.
flowDefinition_flowStatus :: Lens.Lens' FlowDefinition (Prelude.Maybe FlowStatus)
flowDefinition_flowStatus = Lens.lens (\FlowDefinition' {flowStatus} -> flowStatus) (\s@FlowDefinition' {} a -> s {flowStatus = a} :: FlowDefinition)

-- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
flowDefinition_triggerType :: Lens.Lens' FlowDefinition (Prelude.Maybe TriggerType)
flowDefinition_triggerType = Lens.lens (\FlowDefinition' {triggerType} -> triggerType) (\s@FlowDefinition' {} a -> s {triggerType = a} :: FlowDefinition)

-- | Specifies the destination connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
flowDefinition_destinationConnectorType :: Lens.Lens' FlowDefinition (Prelude.Maybe ConnectorType)
flowDefinition_destinationConnectorType = Lens.lens (\FlowDefinition' {destinationConnectorType} -> destinationConnectorType) (\s@FlowDefinition' {} a -> s {destinationConnectorType = a} :: FlowDefinition)

-- | The ARN of the user who created the flow.
flowDefinition_createdBy :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_createdBy = Lens.lens (\FlowDefinition' {createdBy} -> createdBy) (\s@FlowDefinition' {} a -> s {createdBy = a} :: FlowDefinition)

-- | The flow\'s Amazon Resource Name (ARN).
flowDefinition_flowArn :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_flowArn = Lens.lens (\FlowDefinition' {flowArn} -> flowArn) (\s@FlowDefinition' {} a -> s {flowArn = a} :: FlowDefinition)

-- | Specifies when the flow was created.
flowDefinition_createdAt :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.UTCTime)
flowDefinition_createdAt = Lens.lens (\FlowDefinition' {createdAt} -> createdAt) (\s@FlowDefinition' {} a -> s {createdAt = a} :: FlowDefinition) Prelude.. Lens.mapping Data._Time

-- | Specifies the account user name that most recently updated the flow.
flowDefinition_lastUpdatedBy :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_lastUpdatedBy = Lens.lens (\FlowDefinition' {lastUpdatedBy} -> lastUpdatedBy) (\s@FlowDefinition' {} a -> s {lastUpdatedBy = a} :: FlowDefinition)

instance Data.FromJSON FlowDefinition where
  parseJSON =
    Data.withObject
      "FlowDefinition"
      ( \x ->
          FlowDefinition'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "destinationConnectorLabel")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "flowName")
            Prelude.<*> (x Data..:? "sourceConnectorLabel")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "sourceConnectorType")
            Prelude.<*> (x Data..:? "lastRunExecutionDetails")
            Prelude.<*> (x Data..:? "flowStatus")
            Prelude.<*> (x Data..:? "triggerType")
            Prelude.<*> (x Data..:? "destinationConnectorType")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "flowArn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "lastUpdatedBy")
      )

instance Prelude.Hashable FlowDefinition where
  hashWithSalt _salt FlowDefinition' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destinationConnectorLabel
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` sourceConnectorLabel
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceConnectorType
      `Prelude.hashWithSalt` lastRunExecutionDetails
      `Prelude.hashWithSalt` flowStatus
      `Prelude.hashWithSalt` triggerType
      `Prelude.hashWithSalt` destinationConnectorType
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedBy

instance Prelude.NFData FlowDefinition where
  rnf FlowDefinition' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationConnectorLabel
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf flowName
      `Prelude.seq` Prelude.rnf sourceConnectorLabel
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceConnectorType
      `Prelude.seq` Prelude.rnf lastRunExecutionDetails
      `Prelude.seq` Prelude.rnf flowStatus
      `Prelude.seq` Prelude.rnf triggerType
      `Prelude.seq` Prelude.rnf destinationConnectorType
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedBy
