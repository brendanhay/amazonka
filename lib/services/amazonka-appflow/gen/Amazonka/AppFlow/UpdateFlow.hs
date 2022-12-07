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
-- Module      : Amazonka.AppFlow.UpdateFlow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing flow.
module Amazonka.AppFlow.UpdateFlow
  ( -- * Creating a Request
    UpdateFlow (..),
    newUpdateFlow,

    -- * Request Lenses
    updateFlow_metadataCatalogConfig,
    updateFlow_description,
    updateFlow_flowName,
    updateFlow_triggerConfig,
    updateFlow_sourceFlowConfig,
    updateFlow_destinationFlowConfigList,
    updateFlow_tasks,

    -- * Destructuring the Response
    UpdateFlowResponse (..),
    newUpdateFlowResponse,

    -- * Response Lenses
    updateFlowResponse_flowStatus,
    updateFlowResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFlow' smart constructor.
data UpdateFlow = UpdateFlow'
  { -- | Specifies the configuration that Amazon AppFlow uses when it catalogs
    -- the data that\'s transferred by the associated flow. When Amazon AppFlow
    -- catalogs the data from a flow, it stores metadata in a data catalog.
    metadataCatalogConfig :: Prelude.Maybe MetadataCatalogConfig,
    -- | A description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Text,
    -- | The trigger settings that determine how and when the flow runs.
    triggerConfig :: TriggerConfig,
    sourceFlowConfig :: SourceFlowConfig,
    -- | The configuration that controls how Amazon AppFlow transfers data to the
    -- destination connector.
    destinationFlowConfigList :: [DestinationFlowConfig],
    -- | A list of tasks that Amazon AppFlow performs while transferring the data
    -- in the flow run.
    tasks :: [Task]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataCatalogConfig', 'updateFlow_metadataCatalogConfig' - Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
--
-- 'description', 'updateFlow_description' - A description of the flow.
--
-- 'flowName', 'updateFlow_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
--
-- 'triggerConfig', 'updateFlow_triggerConfig' - The trigger settings that determine how and when the flow runs.
--
-- 'sourceFlowConfig', 'updateFlow_sourceFlowConfig' - Undocumented member.
--
-- 'destinationFlowConfigList', 'updateFlow_destinationFlowConfigList' - The configuration that controls how Amazon AppFlow transfers data to the
-- destination connector.
--
-- 'tasks', 'updateFlow_tasks' - A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
newUpdateFlow ::
  -- | 'flowName'
  Prelude.Text ->
  -- | 'triggerConfig'
  TriggerConfig ->
  -- | 'sourceFlowConfig'
  SourceFlowConfig ->
  UpdateFlow
newUpdateFlow
  pFlowName_
  pTriggerConfig_
  pSourceFlowConfig_ =
    UpdateFlow'
      { metadataCatalogConfig =
          Prelude.Nothing,
        description = Prelude.Nothing,
        flowName = pFlowName_,
        triggerConfig = pTriggerConfig_,
        sourceFlowConfig = pSourceFlowConfig_,
        destinationFlowConfigList = Prelude.mempty,
        tasks = Prelude.mempty
      }

-- | Specifies the configuration that Amazon AppFlow uses when it catalogs
-- the data that\'s transferred by the associated flow. When Amazon AppFlow
-- catalogs the data from a flow, it stores metadata in a data catalog.
updateFlow_metadataCatalogConfig :: Lens.Lens' UpdateFlow (Prelude.Maybe MetadataCatalogConfig)
updateFlow_metadataCatalogConfig = Lens.lens (\UpdateFlow' {metadataCatalogConfig} -> metadataCatalogConfig) (\s@UpdateFlow' {} a -> s {metadataCatalogConfig = a} :: UpdateFlow)

-- | A description of the flow.
updateFlow_description :: Lens.Lens' UpdateFlow (Prelude.Maybe Prelude.Text)
updateFlow_description = Lens.lens (\UpdateFlow' {description} -> description) (\s@UpdateFlow' {} a -> s {description = a} :: UpdateFlow)

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
updateFlow_flowName :: Lens.Lens' UpdateFlow Prelude.Text
updateFlow_flowName = Lens.lens (\UpdateFlow' {flowName} -> flowName) (\s@UpdateFlow' {} a -> s {flowName = a} :: UpdateFlow)

-- | The trigger settings that determine how and when the flow runs.
updateFlow_triggerConfig :: Lens.Lens' UpdateFlow TriggerConfig
updateFlow_triggerConfig = Lens.lens (\UpdateFlow' {triggerConfig} -> triggerConfig) (\s@UpdateFlow' {} a -> s {triggerConfig = a} :: UpdateFlow)

-- | Undocumented member.
updateFlow_sourceFlowConfig :: Lens.Lens' UpdateFlow SourceFlowConfig
updateFlow_sourceFlowConfig = Lens.lens (\UpdateFlow' {sourceFlowConfig} -> sourceFlowConfig) (\s@UpdateFlow' {} a -> s {sourceFlowConfig = a} :: UpdateFlow)

-- | The configuration that controls how Amazon AppFlow transfers data to the
-- destination connector.
updateFlow_destinationFlowConfigList :: Lens.Lens' UpdateFlow [DestinationFlowConfig]
updateFlow_destinationFlowConfigList = Lens.lens (\UpdateFlow' {destinationFlowConfigList} -> destinationFlowConfigList) (\s@UpdateFlow' {} a -> s {destinationFlowConfigList = a} :: UpdateFlow) Prelude.. Lens.coerced

-- | A list of tasks that Amazon AppFlow performs while transferring the data
-- in the flow run.
updateFlow_tasks :: Lens.Lens' UpdateFlow [Task]
updateFlow_tasks = Lens.lens (\UpdateFlow' {tasks} -> tasks) (\s@UpdateFlow' {} a -> s {tasks = a} :: UpdateFlow) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateFlow where
  type AWSResponse UpdateFlow = UpdateFlowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlowResponse'
            Prelude.<$> (x Data..?> "flowStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlow where
  hashWithSalt _salt UpdateFlow' {..} =
    _salt `Prelude.hashWithSalt` metadataCatalogConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` flowName
      `Prelude.hashWithSalt` triggerConfig
      `Prelude.hashWithSalt` sourceFlowConfig
      `Prelude.hashWithSalt` destinationFlowConfigList
      `Prelude.hashWithSalt` tasks

instance Prelude.NFData UpdateFlow where
  rnf UpdateFlow' {..} =
    Prelude.rnf metadataCatalogConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf flowName
      `Prelude.seq` Prelude.rnf triggerConfig
      `Prelude.seq` Prelude.rnf sourceFlowConfig
      `Prelude.seq` Prelude.rnf destinationFlowConfigList
      `Prelude.seq` Prelude.rnf tasks

instance Data.ToHeaders UpdateFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFlow where
  toJSON UpdateFlow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("metadataCatalogConfig" Data..=)
              Prelude.<$> metadataCatalogConfig,
            ("description" Data..=) Prelude.<$> description,
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

instance Data.ToPath UpdateFlow where
  toPath = Prelude.const "/update-flow"

instance Data.ToQuery UpdateFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlowResponse' smart constructor.
data UpdateFlowResponse = UpdateFlowResponse'
  { -- | Indicates the current status of the flow.
    flowStatus :: Prelude.Maybe FlowStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowStatus', 'updateFlowResponse_flowStatus' - Indicates the current status of the flow.
--
-- 'httpStatus', 'updateFlowResponse_httpStatus' - The response's http status code.
newUpdateFlowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlowResponse
newUpdateFlowResponse pHttpStatus_ =
  UpdateFlowResponse'
    { flowStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates the current status of the flow.
updateFlowResponse_flowStatus :: Lens.Lens' UpdateFlowResponse (Prelude.Maybe FlowStatus)
updateFlowResponse_flowStatus = Lens.lens (\UpdateFlowResponse' {flowStatus} -> flowStatus) (\s@UpdateFlowResponse' {} a -> s {flowStatus = a} :: UpdateFlowResponse)

-- | The response's http status code.
updateFlowResponse_httpStatus :: Lens.Lens' UpdateFlowResponse Prelude.Int
updateFlowResponse_httpStatus = Lens.lens (\UpdateFlowResponse' {httpStatus} -> httpStatus) (\s@UpdateFlowResponse' {} a -> s {httpStatus = a} :: UpdateFlowResponse)

instance Prelude.NFData UpdateFlowResponse where
  rnf UpdateFlowResponse' {..} =
    Prelude.rnf flowStatus
      `Prelude.seq` Prelude.rnf httpStatus
