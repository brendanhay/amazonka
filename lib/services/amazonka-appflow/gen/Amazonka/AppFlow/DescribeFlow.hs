{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.DescribeFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the specified flow.
module Amazonka.AppFlow.DescribeFlow
  ( -- * Creating a Request
    DescribeFlow (..),
    newDescribeFlow,

    -- * Request Lenses
    describeFlow_flowName,

    -- * Destructuring the Response
    DescribeFlowResponse (..),
    newDescribeFlowResponse,

    -- * Response Lenses
    describeFlowResponse_tags,
    describeFlowResponse_sourceFlowConfig,
    describeFlowResponse_tasks,
    describeFlowResponse_kmsArn,
    describeFlowResponse_lastUpdatedAt,
    describeFlowResponse_metadataCatalogConfig,
    describeFlowResponse_flowName,
    describeFlowResponse_destinationFlowConfigList,
    describeFlowResponse_flowStatusMessage,
    describeFlowResponse_description,
    describeFlowResponse_lastRunExecutionDetails,
    describeFlowResponse_lastRunMetadataCatalogDetails,
    describeFlowResponse_flowStatus,
    describeFlowResponse_schemaVersion,
    describeFlowResponse_createdBy,
    describeFlowResponse_triggerConfig,
    describeFlowResponse_flowArn,
    describeFlowResponse_createdAt,
    describeFlowResponse_lastUpdatedBy,
    describeFlowResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlow' smart constructor.
data DescribeFlow = DescribeFlow'
  { -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowName', 'describeFlow_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
newDescribeFlow ::
  -- | 'flowName'
  Prelude.Text ->
  DescribeFlow
newDescribeFlow pFlowName_ =
  DescribeFlow' {flowName = pFlowName_}

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
describeFlow_flowName :: Lens.Lens' DescribeFlow Prelude.Text
describeFlow_flowName = Lens.lens (\DescribeFlow' {flowName} -> flowName) (\s@DescribeFlow' {} a -> s {flowName = a} :: DescribeFlow)

instance Core.AWSRequest DescribeFlow where
  type AWSResponse DescribeFlow = DescribeFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlowResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "sourceFlowConfig")
            Prelude.<*> (x Core..?> "tasks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "kmsArn")
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "metadataCatalogConfig")
            Prelude.<*> (x Core..?> "flowName")
            Prelude.<*> ( x Core..?> "destinationFlowConfigList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "flowStatusMessage")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "lastRunExecutionDetails")
            Prelude.<*> ( x Core..?> "lastRunMetadataCatalogDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "flowStatus")
            Prelude.<*> (x Core..?> "schemaVersion")
            Prelude.<*> (x Core..?> "createdBy")
            Prelude.<*> (x Core..?> "triggerConfig")
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "lastUpdatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFlow where
  hashWithSalt _salt DescribeFlow' {..} =
    _salt `Prelude.hashWithSalt` flowName

instance Prelude.NFData DescribeFlow where
  rnf DescribeFlow' {..} = Prelude.rnf flowName

instance Core.ToHeaders DescribeFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFlow where
  toJSON DescribeFlow' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("flowName" Core..= flowName)]
      )

instance Core.ToPath DescribeFlow where
  toPath = Prelude.const "/describe-flow"

instance Core.ToQuery DescribeFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlowResponse' smart constructor.
data DescribeFlowResponse = DescribeFlowResponse'
  { -- | The tags used to organize, track, or control access for your flow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The configuration that controls how Amazon AppFlow retrieves data from
    -- the source connector.
    sourceFlowConfig :: Prelude.Maybe SourceFlowConfig,
    -- | A list of tasks that Amazon AppFlow performs while transferring the data
    -- in the flow run.
    tasks :: Prelude.Maybe [Task],
    -- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
    -- you provide for encryption. This is required if you do not want to use
    -- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
    -- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
    kmsArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the flow was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies the configuration that Amazon AppFlow uses when it catalogs
    -- the data that\'s transferred by the associated flow. When Amazon AppFlow
    -- catalogs the data from a flow, it stores metadata in a data catalog.
    metadataCatalogConfig :: Prelude.Maybe MetadataCatalogConfig,
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Maybe Prelude.Text,
    -- | The configuration that controls how Amazon AppFlow transfers data to the
    -- destination connector.
    destinationFlowConfigList :: Prelude.Maybe [DestinationFlowConfig],
    -- | Contains an error message if the flow status is in a suspended or error
    -- state. This applies only to scheduled or event-triggered flows.
    flowStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | A description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | Describes the details of the most recent flow run.
    lastRunExecutionDetails :: Prelude.Maybe ExecutionDetails,
    -- | Describes the metadata catalog, metadata table, and data partitions that
    -- Amazon AppFlow used for the associated flow run.
    lastRunMetadataCatalogDetails :: Prelude.Maybe [MetadataCatalogDetail],
    -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | The version number of your data schema. Amazon AppFlow assigns this
    -- version number. The version number increases by one when you change any
    -- of the following settings in your flow configuration:
    --
    -- -   Source-to-destination field mappings
    --
    -- -   Field data types
    --
    -- -   Partition keys
    schemaVersion :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the user who created the flow.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The trigger settings that determine how and when the flow runs.
    triggerConfig :: Prelude.Maybe TriggerConfig,
    -- | The flow\'s Amazon Resource Name (ARN).
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the flow was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies the user name of the account that performed the most recent
    -- update.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeFlowResponse_tags' - The tags used to organize, track, or control access for your flow.
--
-- 'sourceFlowConfig', 'describeFlowResponse_sourceFlowConfig' - The configuration that controls how Amazon AppFlow retrieves data from
-- the source connector.
--
-- 'tasks', 'describeFlowResponse_tasks' - A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
--
-- 'kmsArn', 'describeFlowResponse_kmsArn' - The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
--
-- 'lastUpdatedAt', 'describeFlowResponse_lastUpdatedAt' - Specifies when the flow was last updated.
--
-- 'metadataCatalogConfig', 'describeFlowResponse_metadataCatalogConfig' - Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
--
-- 'flowName', 'describeFlowResponse_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
--
-- 'destinationFlowConfigList', 'describeFlowResponse_destinationFlowConfigList' - The configuration that controls how Amazon AppFlow transfers data to the
-- destination connector.
--
-- 'flowStatusMessage', 'describeFlowResponse_flowStatusMessage' - Contains an error message if the flow status is in a suspended or error
-- state. This applies only to scheduled or event-triggered flows.
--
-- 'description', 'describeFlowResponse_description' - A description of the flow.
--
-- 'lastRunExecutionDetails', 'describeFlowResponse_lastRunExecutionDetails' - Describes the details of the most recent flow run.
--
-- 'lastRunMetadataCatalogDetails', 'describeFlowResponse_lastRunMetadataCatalogDetails' - Describes the metadata catalog, metadata table, and data partitions that
-- Amazon AppFlow used for the associated flow run.
--
-- 'flowStatus', 'describeFlowResponse_flowStatus' - Indicates the current status of the flow.
--
-- 'schemaVersion', 'describeFlowResponse_schemaVersion' - The version number of your data schema. Amazon AppFlow assigns this
-- version number. The version number increases by one when you change any
-- of the following settings in your flow configuration:
--
-- -   Source-to-destination field mappings
--
-- -   Field data types
--
-- -   Partition keys
--
-- 'createdBy', 'describeFlowResponse_createdBy' - The ARN of the user who created the flow.
--
-- 'triggerConfig', 'describeFlowResponse_triggerConfig' - The trigger settings that determine how and when the flow runs.
--
-- 'flowArn', 'describeFlowResponse_flowArn' - The flow\'s Amazon Resource Name (ARN).
--
-- 'createdAt', 'describeFlowResponse_createdAt' - Specifies when the flow was created.
--
-- 'lastUpdatedBy', 'describeFlowResponse_lastUpdatedBy' - Specifies the user name of the account that performed the most recent
-- update.
--
-- 'httpStatus', 'describeFlowResponse_httpStatus' - The response's http status code.
newDescribeFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFlowResponse
newDescribeFlowResponse pHttpStatus_ =
  DescribeFlowResponse'
    { tags = Prelude.Nothing,
      sourceFlowConfig = Prelude.Nothing,
      tasks = Prelude.Nothing,
      kmsArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      metadataCatalogConfig = Prelude.Nothing,
      flowName = Prelude.Nothing,
      destinationFlowConfigList = Prelude.Nothing,
      flowStatusMessage = Prelude.Nothing,
      description = Prelude.Nothing,
      lastRunExecutionDetails = Prelude.Nothing,
      lastRunMetadataCatalogDetails = Prelude.Nothing,
      flowStatus = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      triggerConfig = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags used to organize, track, or control access for your flow.
describeFlowResponse_tags :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeFlowResponse_tags = Lens.lens (\DescribeFlowResponse' {tags} -> tags) (\s@DescribeFlowResponse' {} a -> s {tags = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The configuration that controls how Amazon AppFlow retrieves data from
-- the source connector.
describeFlowResponse_sourceFlowConfig :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe SourceFlowConfig)
describeFlowResponse_sourceFlowConfig = Lens.lens (\DescribeFlowResponse' {sourceFlowConfig} -> sourceFlowConfig) (\s@DescribeFlowResponse' {} a -> s {sourceFlowConfig = a} :: DescribeFlowResponse)

-- | A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
describeFlowResponse_tasks :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe [Task])
describeFlowResponse_tasks = Lens.lens (\DescribeFlowResponse' {tasks} -> tasks) (\s@DescribeFlowResponse' {} a -> s {tasks = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
describeFlowResponse_kmsArn :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_kmsArn = Lens.lens (\DescribeFlowResponse' {kmsArn} -> kmsArn) (\s@DescribeFlowResponse' {} a -> s {kmsArn = a} :: DescribeFlowResponse)

-- | Specifies when the flow was last updated.
describeFlowResponse_lastUpdatedAt :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.UTCTime)
describeFlowResponse_lastUpdatedAt = Lens.lens (\DescribeFlowResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeFlowResponse' {} a -> s {lastUpdatedAt = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
describeFlowResponse_metadataCatalogConfig :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe MetadataCatalogConfig)
describeFlowResponse_metadataCatalogConfig = Lens.lens (\DescribeFlowResponse' {metadataCatalogConfig} -> metadataCatalogConfig) (\s@DescribeFlowResponse' {} a -> s {metadataCatalogConfig = a} :: DescribeFlowResponse)

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
describeFlowResponse_flowName :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_flowName = Lens.lens (\DescribeFlowResponse' {flowName} -> flowName) (\s@DescribeFlowResponse' {} a -> s {flowName = a} :: DescribeFlowResponse)

-- | The configuration that controls how Amazon AppFlow transfers data to the
-- destination connector.
describeFlowResponse_destinationFlowConfigList :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe [DestinationFlowConfig])
describeFlowResponse_destinationFlowConfigList = Lens.lens (\DescribeFlowResponse' {destinationFlowConfigList} -> destinationFlowConfigList) (\s@DescribeFlowResponse' {} a -> s {destinationFlowConfigList = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Lens.coerced

-- | Contains an error message if the flow status is in a suspended or error
-- state. This applies only to scheduled or event-triggered flows.
describeFlowResponse_flowStatusMessage :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_flowStatusMessage = Lens.lens (\DescribeFlowResponse' {flowStatusMessage} -> flowStatusMessage) (\s@DescribeFlowResponse' {} a -> s {flowStatusMessage = a} :: DescribeFlowResponse)

-- | A description of the flow.
describeFlowResponse_description :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_description = Lens.lens (\DescribeFlowResponse' {description} -> description) (\s@DescribeFlowResponse' {} a -> s {description = a} :: DescribeFlowResponse)

-- | Describes the details of the most recent flow run.
describeFlowResponse_lastRunExecutionDetails :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe ExecutionDetails)
describeFlowResponse_lastRunExecutionDetails = Lens.lens (\DescribeFlowResponse' {lastRunExecutionDetails} -> lastRunExecutionDetails) (\s@DescribeFlowResponse' {} a -> s {lastRunExecutionDetails = a} :: DescribeFlowResponse)

-- | Describes the metadata catalog, metadata table, and data partitions that
-- Amazon AppFlow used for the associated flow run.
describeFlowResponse_lastRunMetadataCatalogDetails :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe [MetadataCatalogDetail])
describeFlowResponse_lastRunMetadataCatalogDetails = Lens.lens (\DescribeFlowResponse' {lastRunMetadataCatalogDetails} -> lastRunMetadataCatalogDetails) (\s@DescribeFlowResponse' {} a -> s {lastRunMetadataCatalogDetails = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the current status of the flow.
describeFlowResponse_flowStatus :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe FlowStatus)
describeFlowResponse_flowStatus = Lens.lens (\DescribeFlowResponse' {flowStatus} -> flowStatus) (\s@DescribeFlowResponse' {} a -> s {flowStatus = a} :: DescribeFlowResponse)

-- | The version number of your data schema. Amazon AppFlow assigns this
-- version number. The version number increases by one when you change any
-- of the following settings in your flow configuration:
--
-- -   Source-to-destination field mappings
--
-- -   Field data types
--
-- -   Partition keys
describeFlowResponse_schemaVersion :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Integer)
describeFlowResponse_schemaVersion = Lens.lens (\DescribeFlowResponse' {schemaVersion} -> schemaVersion) (\s@DescribeFlowResponse' {} a -> s {schemaVersion = a} :: DescribeFlowResponse)

-- | The ARN of the user who created the flow.
describeFlowResponse_createdBy :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_createdBy = Lens.lens (\DescribeFlowResponse' {createdBy} -> createdBy) (\s@DescribeFlowResponse' {} a -> s {createdBy = a} :: DescribeFlowResponse)

-- | The trigger settings that determine how and when the flow runs.
describeFlowResponse_triggerConfig :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe TriggerConfig)
describeFlowResponse_triggerConfig = Lens.lens (\DescribeFlowResponse' {triggerConfig} -> triggerConfig) (\s@DescribeFlowResponse' {} a -> s {triggerConfig = a} :: DescribeFlowResponse)

-- | The flow\'s Amazon Resource Name (ARN).
describeFlowResponse_flowArn :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_flowArn = Lens.lens (\DescribeFlowResponse' {flowArn} -> flowArn) (\s@DescribeFlowResponse' {} a -> s {flowArn = a} :: DescribeFlowResponse)

-- | Specifies when the flow was created.
describeFlowResponse_createdAt :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.UTCTime)
describeFlowResponse_createdAt = Lens.lens (\DescribeFlowResponse' {createdAt} -> createdAt) (\s@DescribeFlowResponse' {} a -> s {createdAt = a} :: DescribeFlowResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies the user name of the account that performed the most recent
-- update.
describeFlowResponse_lastUpdatedBy :: Lens.Lens' DescribeFlowResponse (Prelude.Maybe Prelude.Text)
describeFlowResponse_lastUpdatedBy = Lens.lens (\DescribeFlowResponse' {lastUpdatedBy} -> lastUpdatedBy) (\s@DescribeFlowResponse' {} a -> s {lastUpdatedBy = a} :: DescribeFlowResponse)

-- | The response's http status code.
describeFlowResponse_httpStatus :: Lens.Lens' DescribeFlowResponse Prelude.Int
describeFlowResponse_httpStatus = Lens.lens (\DescribeFlowResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowResponse' {} a -> s {httpStatus = a} :: DescribeFlowResponse)

instance Prelude.NFData DescribeFlowResponse where
  rnf DescribeFlowResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceFlowConfig
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf kmsArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf metadataCatalogConfig
      `Prelude.seq` Prelude.rnf flowName
      `Prelude.seq` Prelude.rnf destinationFlowConfigList
      `Prelude.seq` Prelude.rnf flowStatusMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastRunExecutionDetails
      `Prelude.seq` Prelude.rnf lastRunMetadataCatalogDetails
      `Prelude.seq` Prelude.rnf flowStatus
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf triggerConfig
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf httpStatus
