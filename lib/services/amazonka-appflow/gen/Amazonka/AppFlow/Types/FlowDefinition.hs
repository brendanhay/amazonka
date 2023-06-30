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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | Specifies when the flow was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the user who created the flow.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | A user-entered description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The label of the destination connector in the flow.
    destinationConnectorLabel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the destination connector type, such as Salesforce, Amazon S3,
    -- Amplitude, and so on.
    destinationConnectorType :: Prelude.Maybe ConnectorType,
    -- | The flow\'s Amazon Resource Name (ARN).
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | Describes the details of the most recent flow run.
    lastRunExecutionDetails :: Prelude.Maybe ExecutionDetails,
    -- | Specifies when the flow was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Specifies the account user name that most recently updated the flow.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The label of the source connector in the flow.
    sourceConnectorLabel :: Prelude.Maybe Prelude.Text,
    -- | Specifies the source connector type, such as Salesforce, Amazon S3,
    -- Amplitude, and so on.
    sourceConnectorType :: Prelude.Maybe ConnectorType,
    -- | The tags used to organize, track, or control access for your flow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
    -- or @Event@.
    triggerType :: Prelude.Maybe TriggerType
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
-- 'createdAt', 'flowDefinition_createdAt' - Specifies when the flow was created.
--
-- 'createdBy', 'flowDefinition_createdBy' - The ARN of the user who created the flow.
--
-- 'description', 'flowDefinition_description' - A user-entered description of the flow.
--
-- 'destinationConnectorLabel', 'flowDefinition_destinationConnectorLabel' - The label of the destination connector in the flow.
--
-- 'destinationConnectorType', 'flowDefinition_destinationConnectorType' - Specifies the destination connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
--
-- 'flowArn', 'flowDefinition_flowArn' - The flow\'s Amazon Resource Name (ARN).
--
-- 'flowName', 'flowDefinition_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
--
-- 'flowStatus', 'flowDefinition_flowStatus' - Indicates the current status of the flow.
--
-- 'lastRunExecutionDetails', 'flowDefinition_lastRunExecutionDetails' - Describes the details of the most recent flow run.
--
-- 'lastUpdatedAt', 'flowDefinition_lastUpdatedAt' - Specifies when the flow was last updated.
--
-- 'lastUpdatedBy', 'flowDefinition_lastUpdatedBy' - Specifies the account user name that most recently updated the flow.
--
-- 'sourceConnectorLabel', 'flowDefinition_sourceConnectorLabel' - The label of the source connector in the flow.
--
-- 'sourceConnectorType', 'flowDefinition_sourceConnectorType' - Specifies the source connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
--
-- 'tags', 'flowDefinition_tags' - The tags used to organize, track, or control access for your flow.
--
-- 'triggerType', 'flowDefinition_triggerType' - Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
newFlowDefinition ::
  FlowDefinition
newFlowDefinition =
  FlowDefinition'
    { createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationConnectorLabel = Prelude.Nothing,
      destinationConnectorType = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      flowName = Prelude.Nothing,
      flowStatus = Prelude.Nothing,
      lastRunExecutionDetails = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      sourceConnectorLabel = Prelude.Nothing,
      sourceConnectorType = Prelude.Nothing,
      tags = Prelude.Nothing,
      triggerType = Prelude.Nothing
    }

-- | Specifies when the flow was created.
flowDefinition_createdAt :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.UTCTime)
flowDefinition_createdAt = Lens.lens (\FlowDefinition' {createdAt} -> createdAt) (\s@FlowDefinition' {} a -> s {createdAt = a} :: FlowDefinition) Prelude.. Lens.mapping Data._Time

-- | The ARN of the user who created the flow.
flowDefinition_createdBy :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_createdBy = Lens.lens (\FlowDefinition' {createdBy} -> createdBy) (\s@FlowDefinition' {} a -> s {createdBy = a} :: FlowDefinition)

-- | A user-entered description of the flow.
flowDefinition_description :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_description = Lens.lens (\FlowDefinition' {description} -> description) (\s@FlowDefinition' {} a -> s {description = a} :: FlowDefinition)

-- | The label of the destination connector in the flow.
flowDefinition_destinationConnectorLabel :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_destinationConnectorLabel = Lens.lens (\FlowDefinition' {destinationConnectorLabel} -> destinationConnectorLabel) (\s@FlowDefinition' {} a -> s {destinationConnectorLabel = a} :: FlowDefinition)

-- | Specifies the destination connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
flowDefinition_destinationConnectorType :: Lens.Lens' FlowDefinition (Prelude.Maybe ConnectorType)
flowDefinition_destinationConnectorType = Lens.lens (\FlowDefinition' {destinationConnectorType} -> destinationConnectorType) (\s@FlowDefinition' {} a -> s {destinationConnectorType = a} :: FlowDefinition)

-- | The flow\'s Amazon Resource Name (ARN).
flowDefinition_flowArn :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_flowArn = Lens.lens (\FlowDefinition' {flowArn} -> flowArn) (\s@FlowDefinition' {} a -> s {flowArn = a} :: FlowDefinition)

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
flowDefinition_flowName :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_flowName = Lens.lens (\FlowDefinition' {flowName} -> flowName) (\s@FlowDefinition' {} a -> s {flowName = a} :: FlowDefinition)

-- | Indicates the current status of the flow.
flowDefinition_flowStatus :: Lens.Lens' FlowDefinition (Prelude.Maybe FlowStatus)
flowDefinition_flowStatus = Lens.lens (\FlowDefinition' {flowStatus} -> flowStatus) (\s@FlowDefinition' {} a -> s {flowStatus = a} :: FlowDefinition)

-- | Describes the details of the most recent flow run.
flowDefinition_lastRunExecutionDetails :: Lens.Lens' FlowDefinition (Prelude.Maybe ExecutionDetails)
flowDefinition_lastRunExecutionDetails = Lens.lens (\FlowDefinition' {lastRunExecutionDetails} -> lastRunExecutionDetails) (\s@FlowDefinition' {} a -> s {lastRunExecutionDetails = a} :: FlowDefinition)

-- | Specifies when the flow was last updated.
flowDefinition_lastUpdatedAt :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.UTCTime)
flowDefinition_lastUpdatedAt = Lens.lens (\FlowDefinition' {lastUpdatedAt} -> lastUpdatedAt) (\s@FlowDefinition' {} a -> s {lastUpdatedAt = a} :: FlowDefinition) Prelude.. Lens.mapping Data._Time

-- | Specifies the account user name that most recently updated the flow.
flowDefinition_lastUpdatedBy :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_lastUpdatedBy = Lens.lens (\FlowDefinition' {lastUpdatedBy} -> lastUpdatedBy) (\s@FlowDefinition' {} a -> s {lastUpdatedBy = a} :: FlowDefinition)

-- | The label of the source connector in the flow.
flowDefinition_sourceConnectorLabel :: Lens.Lens' FlowDefinition (Prelude.Maybe Prelude.Text)
flowDefinition_sourceConnectorLabel = Lens.lens (\FlowDefinition' {sourceConnectorLabel} -> sourceConnectorLabel) (\s@FlowDefinition' {} a -> s {sourceConnectorLabel = a} :: FlowDefinition)

-- | Specifies the source connector type, such as Salesforce, Amazon S3,
-- Amplitude, and so on.
flowDefinition_sourceConnectorType :: Lens.Lens' FlowDefinition (Prelude.Maybe ConnectorType)
flowDefinition_sourceConnectorType = Lens.lens (\FlowDefinition' {sourceConnectorType} -> sourceConnectorType) (\s@FlowDefinition' {} a -> s {sourceConnectorType = a} :: FlowDefinition)

-- | The tags used to organize, track, or control access for your flow.
flowDefinition_tags :: Lens.Lens' FlowDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
flowDefinition_tags = Lens.lens (\FlowDefinition' {tags} -> tags) (\s@FlowDefinition' {} a -> s {tags = a} :: FlowDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of flow trigger. This can be @OnDemand@, @Scheduled@,
-- or @Event@.
flowDefinition_triggerType :: Lens.Lens' FlowDefinition (Prelude.Maybe TriggerType)
flowDefinition_triggerType = Lens.lens (\FlowDefinition' {triggerType} -> triggerType) (\s@FlowDefinition' {} a -> s {triggerType = a} :: FlowDefinition)

instance Data.FromJSON FlowDefinition where
  parseJSON =
    Data.withObject
      "FlowDefinition"
      ( \x ->
          FlowDefinition'
            Prelude.<$> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdBy")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "destinationConnectorLabel")
            Prelude.<*> (x Data..:? "destinationConnectorType")
            Prelude.<*> (x Data..:? "flowArn")
            Prelude.<*> (x Data..:? "flowName")
            Prelude.<*> (x Data..:? "flowStatus")
            Prelude.<*> (x Data..:? "lastRunExecutionDetails")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "lastUpdatedBy")
            Prelude.<*> (x Data..:? "sourceConnectorLabel")
            Prelude.<*> (x Data..:? "sourceConnectorType")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "triggerType")
      )

instance Prelude.Hashable FlowDefinition where
  hashWithSalt _salt FlowDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationConnectorLabel
      `Prelude.hashWithSalt` destinationConnectorType
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` flowStatus
      `Prelude.hashWithSalt` lastRunExecutionDetails
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` lastUpdatedBy
      `Prelude.hashWithSalt` sourceConnectorLabel
      `Prelude.hashWithSalt` sourceConnectorType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` triggerType

instance Prelude.NFData FlowDefinition where
  rnf FlowDefinition' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationConnectorLabel
      `Prelude.seq` Prelude.rnf destinationConnectorType
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf flowName
      `Prelude.seq` Prelude.rnf flowStatus
      `Prelude.seq` Prelude.rnf lastRunExecutionDetails
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf sourceConnectorLabel
      `Prelude.seq` Prelude.rnf sourceConnectorType
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf triggerType
