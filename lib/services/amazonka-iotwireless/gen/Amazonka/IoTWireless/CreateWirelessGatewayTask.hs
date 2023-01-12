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
-- Module      : Amazonka.IoTWireless.CreateWirelessGatewayTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a task for a wireless gateway.
module Amazonka.IoTWireless.CreateWirelessGatewayTask
  ( -- * Creating a Request
    CreateWirelessGatewayTask (..),
    newCreateWirelessGatewayTask,

    -- * Request Lenses
    createWirelessGatewayTask_id,
    createWirelessGatewayTask_wirelessGatewayTaskDefinitionId,

    -- * Destructuring the Response
    CreateWirelessGatewayTaskResponse (..),
    newCreateWirelessGatewayTaskResponse,

    -- * Response Lenses
    createWirelessGatewayTaskResponse_status,
    createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    createWirelessGatewayTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWirelessGatewayTask' smart constructor.
data CreateWirelessGatewayTask = CreateWirelessGatewayTask'
  { -- | The ID of the resource to update.
    id :: Prelude.Text,
    -- | The ID of the WirelessGatewayTaskDefinition.
    wirelessGatewayTaskDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGatewayTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createWirelessGatewayTask_id' - The ID of the resource to update.
--
-- 'wirelessGatewayTaskDefinitionId', 'createWirelessGatewayTask_wirelessGatewayTaskDefinitionId' - The ID of the WirelessGatewayTaskDefinition.
newCreateWirelessGatewayTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'wirelessGatewayTaskDefinitionId'
  Prelude.Text ->
  CreateWirelessGatewayTask
newCreateWirelessGatewayTask
  pId_
  pWirelessGatewayTaskDefinitionId_ =
    CreateWirelessGatewayTask'
      { id = pId_,
        wirelessGatewayTaskDefinitionId =
          pWirelessGatewayTaskDefinitionId_
      }

-- | The ID of the resource to update.
createWirelessGatewayTask_id :: Lens.Lens' CreateWirelessGatewayTask Prelude.Text
createWirelessGatewayTask_id = Lens.lens (\CreateWirelessGatewayTask' {id} -> id) (\s@CreateWirelessGatewayTask' {} a -> s {id = a} :: CreateWirelessGatewayTask)

-- | The ID of the WirelessGatewayTaskDefinition.
createWirelessGatewayTask_wirelessGatewayTaskDefinitionId :: Lens.Lens' CreateWirelessGatewayTask Prelude.Text
createWirelessGatewayTask_wirelessGatewayTaskDefinitionId = Lens.lens (\CreateWirelessGatewayTask' {wirelessGatewayTaskDefinitionId} -> wirelessGatewayTaskDefinitionId) (\s@CreateWirelessGatewayTask' {} a -> s {wirelessGatewayTaskDefinitionId = a} :: CreateWirelessGatewayTask)

instance Core.AWSRequest CreateWirelessGatewayTask where
  type
    AWSResponse CreateWirelessGatewayTask =
      CreateWirelessGatewayTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWirelessGatewayTaskResponse'
            Prelude.<$> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "WirelessGatewayTaskDefinitionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWirelessGatewayTask where
  hashWithSalt _salt CreateWirelessGatewayTask' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` wirelessGatewayTaskDefinitionId

instance Prelude.NFData CreateWirelessGatewayTask where
  rnf CreateWirelessGatewayTask' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf wirelessGatewayTaskDefinitionId

instance Data.ToHeaders CreateWirelessGatewayTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateWirelessGatewayTask where
  toJSON CreateWirelessGatewayTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "WirelessGatewayTaskDefinitionId"
                  Data..= wirelessGatewayTaskDefinitionId
              )
          ]
      )

instance Data.ToPath CreateWirelessGatewayTask where
  toPath CreateWirelessGatewayTask' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Data.toBS id, "/tasks"]

instance Data.ToQuery CreateWirelessGatewayTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWirelessGatewayTaskResponse' smart constructor.
data CreateWirelessGatewayTaskResponse = CreateWirelessGatewayTaskResponse'
  { -- | The status of the request.
    status :: Prelude.Maybe WirelessGatewayTaskStatus,
    -- | The ID of the WirelessGatewayTaskDefinition.
    wirelessGatewayTaskDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWirelessGatewayTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createWirelessGatewayTaskResponse_status' - The status of the request.
--
-- 'wirelessGatewayTaskDefinitionId', 'createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId' - The ID of the WirelessGatewayTaskDefinition.
--
-- 'httpStatus', 'createWirelessGatewayTaskResponse_httpStatus' - The response's http status code.
newCreateWirelessGatewayTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWirelessGatewayTaskResponse
newCreateWirelessGatewayTaskResponse pHttpStatus_ =
  CreateWirelessGatewayTaskResponse'
    { status =
        Prelude.Nothing,
      wirelessGatewayTaskDefinitionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the request.
createWirelessGatewayTaskResponse_status :: Lens.Lens' CreateWirelessGatewayTaskResponse (Prelude.Maybe WirelessGatewayTaskStatus)
createWirelessGatewayTaskResponse_status = Lens.lens (\CreateWirelessGatewayTaskResponse' {status} -> status) (\s@CreateWirelessGatewayTaskResponse' {} a -> s {status = a} :: CreateWirelessGatewayTaskResponse)

-- | The ID of the WirelessGatewayTaskDefinition.
createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId :: Lens.Lens' CreateWirelessGatewayTaskResponse (Prelude.Maybe Prelude.Text)
createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId = Lens.lens (\CreateWirelessGatewayTaskResponse' {wirelessGatewayTaskDefinitionId} -> wirelessGatewayTaskDefinitionId) (\s@CreateWirelessGatewayTaskResponse' {} a -> s {wirelessGatewayTaskDefinitionId = a} :: CreateWirelessGatewayTaskResponse)

-- | The response's http status code.
createWirelessGatewayTaskResponse_httpStatus :: Lens.Lens' CreateWirelessGatewayTaskResponse Prelude.Int
createWirelessGatewayTaskResponse_httpStatus = Lens.lens (\CreateWirelessGatewayTaskResponse' {httpStatus} -> httpStatus) (\s@CreateWirelessGatewayTaskResponse' {} a -> s {httpStatus = a} :: CreateWirelessGatewayTaskResponse)

instance
  Prelude.NFData
    CreateWirelessGatewayTaskResponse
  where
  rnf CreateWirelessGatewayTaskResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf wirelessGatewayTaskDefinitionId
      `Prelude.seq` Prelude.rnf httpStatus
