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
-- Module      : Amazonka.AppFlow.CreateFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables your application to create a new flow using Amazon AppFlow. You
-- must create a connector profile before calling this API. Please note
-- that the Request Syntax below shows syntax for multiple destinations,
-- however, you can only transfer data to one item in this list at a time.
-- Amazon AppFlow does not currently support flows to multiple destinations
-- at once.
module Amazonka.AppFlow.CreateFlow
  ( -- * Creating a Request
    CreateFlow (..),
    newCreateFlow,

    -- * Request Lenses
    createFlow_description,
    createFlow_kmsArn,
    createFlow_metadataCatalogConfig,
    createFlow_tags,
    createFlow_flowName,
    createFlow_triggerConfig,
    createFlow_sourceFlowConfig,
    createFlow_destinationFlowConfigList,
    createFlow_tasks,

    -- * Destructuring the Response
    CreateFlowResponse (..),
    newCreateFlowResponse,

    -- * Response Lenses
    createFlowResponse_flowArn,
    createFlowResponse_flowStatus,
    createFlowResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFlow' smart constructor.
data CreateFlow = CreateFlow'
  { -- | A description of the flow you want to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
    -- you provide for encryption. This is required if you do not want to use
    -- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
    -- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
    kmsArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration that Amazon AppFlow uses when it catalogs
    -- the data that\'s transferred by the associated flow. When Amazon AppFlow
    -- catalogs the data from a flow, it stores metadata in a data catalog.
    metadataCatalogConfig :: Prelude.Maybe MetadataCatalogConfig,
    -- | The tags used to organize, track, or control access for your flow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Text,
    -- | The trigger settings that determine how and when the flow runs.
    triggerConfig :: TriggerConfig,
    -- | The configuration that controls how Amazon AppFlow retrieves data from
    -- the source connector.
    sourceFlowConfig :: SourceFlowConfig,
    -- | The configuration that controls how Amazon AppFlow places data in the
    -- destination connector.
    destinationFlowConfigList :: [DestinationFlowConfig],
    -- | A list of tasks that Amazon AppFlow performs while transferring the data
    -- in the flow run.
    tasks :: [Task]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createFlow_description' - A description of the flow you want to create.
--
-- 'kmsArn', 'createFlow_kmsArn' - The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
--
-- 'metadataCatalogConfig', 'createFlow_metadataCatalogConfig' - Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
--
-- 'tags', 'createFlow_tags' - The tags used to organize, track, or control access for your flow.
--
-- 'flowName', 'createFlow_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
--
-- 'triggerConfig', 'createFlow_triggerConfig' - The trigger settings that determine how and when the flow runs.
--
-- 'sourceFlowConfig', 'createFlow_sourceFlowConfig' - The configuration that controls how Amazon AppFlow retrieves data from
-- the source connector.
--
-- 'destinationFlowConfigList', 'createFlow_destinationFlowConfigList' - The configuration that controls how Amazon AppFlow places data in the
-- destination connector.
--
-- 'tasks', 'createFlow_tasks' - A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
newCreateFlow ::
  -- | 'flowName'
  Prelude.Text ->
  -- | 'triggerConfig'
  TriggerConfig ->
  -- | 'sourceFlowConfig'
  SourceFlowConfig ->
  CreateFlow
newCreateFlow
  pFlowName_
  pTriggerConfig_
  pSourceFlowConfig_ =
    CreateFlow'
      { description = Prelude.Nothing,
        kmsArn = Prelude.Nothing,
        metadataCatalogConfig = Prelude.Nothing,
        tags = Prelude.Nothing,
        flowName = pFlowName_,
        triggerConfig = pTriggerConfig_,
        sourceFlowConfig = pSourceFlowConfig_,
        destinationFlowConfigList = Prelude.mempty,
        tasks = Prelude.mempty
      }

-- | A description of the flow you want to create.
createFlow_description :: Lens.Lens' CreateFlow (Prelude.Maybe Prelude.Text)
createFlow_description = Lens.lens (\CreateFlow' {description} -> description) (\s@CreateFlow' {} a -> s {description = a} :: CreateFlow)

-- | The ARN (Amazon Resource Name) of the Key Management Service (KMS) key
-- you provide for encryption. This is required if you do not want to use
-- the Amazon AppFlow-managed KMS key. If you don\'t provide anything here,
-- Amazon AppFlow uses the Amazon AppFlow-managed KMS key.
createFlow_kmsArn :: Lens.Lens' CreateFlow (Prelude.Maybe Prelude.Text)
createFlow_kmsArn = Lens.lens (\CreateFlow' {kmsArn} -> kmsArn) (\s@CreateFlow' {} a -> s {kmsArn = a} :: CreateFlow)

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
createFlow_metadataCatalogConfig :: Lens.Lens' CreateFlow (Prelude.Maybe MetadataCatalogConfig)
createFlow_metadataCatalogConfig = Lens.lens (\CreateFlow' {metadataCatalogConfig} -> metadataCatalogConfig) (\s@CreateFlow' {} a -> s {metadataCatalogConfig = a} :: CreateFlow)

-- | The tags used to organize, track, or control access for your flow.
createFlow_tags :: Lens.Lens' CreateFlow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFlow_tags = Lens.lens (\CreateFlow' {tags} -> tags) (\s@CreateFlow' {} a -> s {tags = a} :: CreateFlow) Prelude.. Lens.mapping Lens.coerced

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
createFlow_flowName :: Lens.Lens' CreateFlow Prelude.Text
createFlow_flowName = Lens.lens (\CreateFlow' {flowName} -> flowName) (\s@CreateFlow' {} a -> s {flowName = a} :: CreateFlow)

-- | The trigger settings that determine how and when the flow runs.
createFlow_triggerConfig :: Lens.Lens' CreateFlow TriggerConfig
createFlow_triggerConfig = Lens.lens (\CreateFlow' {triggerConfig} -> triggerConfig) (\s@CreateFlow' {} a -> s {triggerConfig = a} :: CreateFlow)

-- | The configuration that controls how Amazon AppFlow retrieves data from
-- the source connector.
createFlow_sourceFlowConfig :: Lens.Lens' CreateFlow SourceFlowConfig
createFlow_sourceFlowConfig = Lens.lens (\CreateFlow' {sourceFlowConfig} -> sourceFlowConfig) (\s@CreateFlow' {} a -> s {sourceFlowConfig = a} :: CreateFlow)

-- | The configuration that controls how Amazon AppFlow places data in the
-- destination connector.
createFlow_destinationFlowConfigList :: Lens.Lens' CreateFlow [DestinationFlowConfig]
createFlow_destinationFlowConfigList = Lens.lens (\CreateFlow' {destinationFlowConfigList} -> destinationFlowConfigList) (\s@CreateFlow' {} a -> s {destinationFlowConfigList = a} :: CreateFlow) Prelude.. Lens.coerced

-- | A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
createFlow_tasks :: Lens.Lens' CreateFlow [Task]
createFlow_tasks = Lens.lens (\CreateFlow' {tasks} -> tasks) (\s@CreateFlow' {} a -> s {tasks = a} :: CreateFlow) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFlow where
  type AWSResponse CreateFlow = CreateFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlowResponse'
            Prelude.<$> (x Data..?> "flowArn")
            Prelude.<*> (x Data..?> "flowStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFlow where
  hashWithSalt _salt CreateFlow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsArn
      `Prelude.hashWithSalt` metadataCatalogConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` triggerConfig
      `Prelude.hashWithSalt` sourceFlowConfig
      `Prelude.hashWithSalt` destinationFlowConfigList
      `Prelude.hashWithSalt` tasks

instance Prelude.NFData CreateFlow where
  rnf CreateFlow' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf kmsArn `Prelude.seq`
        Prelude.rnf metadataCatalogConfig `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf flowName `Prelude.seq`
              Prelude.rnf triggerConfig `Prelude.seq`
                Prelude.rnf sourceFlowConfig `Prelude.seq`
                  Prelude.rnf destinationFlowConfigList `Prelude.seq`
                    Prelude.rnf tasks

instance Data.ToHeaders CreateFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFlow where
  toJSON CreateFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("kmsArn" Data..=) Prelude.<$> kmsArn,
            ("metadataCatalogConfig" Data..=)
              Prelude.<$> metadataCatalogConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("flowName" Data..= flowName),
            Prelude.Just ("triggerConfig" Data..= triggerConfig),
            Prelude.Just
              ("sourceFlowConfig" Data..= sourceFlowConfig),
            Prelude.Just
              ( "destinationFlowConfigList"
                  Data..= destinationFlowConfigList
              ),
            Prelude.Just ("tasks" Data..= tasks)
          ]
      )

instance Data.ToPath CreateFlow where
  toPath = Prelude.const "/create-flow"

instance Data.ToQuery CreateFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFlowResponse' smart constructor.
data CreateFlowResponse = CreateFlowResponse'
  { -- | The flow\'s Amazon Resource Name (ARN).
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'createFlowResponse_flowArn' - The flow\'s Amazon Resource Name (ARN).
--
-- 'flowStatus', 'createFlowResponse_flowStatus' - Indicates the current status of the flow.
--
-- 'httpStatus', 'createFlowResponse_httpStatus' - The response's http status code.
newCreateFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFlowResponse
newCreateFlowResponse pHttpStatus_ =
  CreateFlowResponse'
    { flowArn = Prelude.Nothing,
      flowStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The flow\'s Amazon Resource Name (ARN).
createFlowResponse_flowArn :: Lens.Lens' CreateFlowResponse (Prelude.Maybe Prelude.Text)
createFlowResponse_flowArn = Lens.lens (\CreateFlowResponse' {flowArn} -> flowArn) (\s@CreateFlowResponse' {} a -> s {flowArn = a} :: CreateFlowResponse)

-- | Indicates the current status of the flow.
createFlowResponse_flowStatus :: Lens.Lens' CreateFlowResponse (Prelude.Maybe FlowStatus)
createFlowResponse_flowStatus = Lens.lens (\CreateFlowResponse' {flowStatus} -> flowStatus) (\s@CreateFlowResponse' {} a -> s {flowStatus = a} :: CreateFlowResponse)

-- | The response's http status code.
createFlowResponse_httpStatus :: Lens.Lens' CreateFlowResponse Prelude.Int
createFlowResponse_httpStatus = Lens.lens (\CreateFlowResponse' {httpStatus} -> httpStatus) (\s@CreateFlowResponse' {} a -> s {httpStatus = a} :: CreateFlowResponse)

instance Prelude.NFData CreateFlowResponse where
  rnf CreateFlowResponse' {..} =
    Prelude.rnf flowArn `Prelude.seq`
      Prelude.rnf flowStatus `Prelude.seq`
        Prelude.rnf httpStatus
