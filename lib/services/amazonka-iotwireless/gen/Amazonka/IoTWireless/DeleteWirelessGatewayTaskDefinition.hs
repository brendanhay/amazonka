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
-- Module      : Amazonka.IoTWireless.DeleteWirelessGatewayTaskDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a wireless gateway task definition. Deleting this task
-- definition does not affect tasks that are currently in progress.
module Amazonka.IoTWireless.DeleteWirelessGatewayTaskDefinition
  ( -- * Creating a Request
    DeleteWirelessGatewayTaskDefinition (..),
    newDeleteWirelessGatewayTaskDefinition,

    -- * Request Lenses
    deleteWirelessGatewayTaskDefinition_id,

    -- * Destructuring the Response
    DeleteWirelessGatewayTaskDefinitionResponse (..),
    newDeleteWirelessGatewayTaskDefinitionResponse,

    -- * Response Lenses
    deleteWirelessGatewayTaskDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWirelessGatewayTaskDefinition' smart constructor.
data DeleteWirelessGatewayTaskDefinition = DeleteWirelessGatewayTaskDefinition'
  { -- | The ID of the resource to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessGatewayTaskDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWirelessGatewayTaskDefinition_id' - The ID of the resource to delete.
newDeleteWirelessGatewayTaskDefinition ::
  -- | 'id'
  Prelude.Text ->
  DeleteWirelessGatewayTaskDefinition
newDeleteWirelessGatewayTaskDefinition pId_ =
  DeleteWirelessGatewayTaskDefinition' {id = pId_}

-- | The ID of the resource to delete.
deleteWirelessGatewayTaskDefinition_id :: Lens.Lens' DeleteWirelessGatewayTaskDefinition Prelude.Text
deleteWirelessGatewayTaskDefinition_id = Lens.lens (\DeleteWirelessGatewayTaskDefinition' {id} -> id) (\s@DeleteWirelessGatewayTaskDefinition' {} a -> s {id = a} :: DeleteWirelessGatewayTaskDefinition)

instance
  Core.AWSRequest
    DeleteWirelessGatewayTaskDefinition
  where
  type
    AWSResponse DeleteWirelessGatewayTaskDefinition =
      DeleteWirelessGatewayTaskDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWirelessGatewayTaskDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteWirelessGatewayTaskDefinition
  where
  hashWithSalt
    _salt
    DeleteWirelessGatewayTaskDefinition' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteWirelessGatewayTaskDefinition
  where
  rnf DeleteWirelessGatewayTaskDefinition' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    DeleteWirelessGatewayTaskDefinition
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteWirelessGatewayTaskDefinition
  where
  toPath DeleteWirelessGatewayTaskDefinition' {..} =
    Prelude.mconcat
      ["/wireless-gateway-task-definitions/", Data.toBS id]

instance
  Data.ToQuery
    DeleteWirelessGatewayTaskDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWirelessGatewayTaskDefinitionResponse' smart constructor.
data DeleteWirelessGatewayTaskDefinitionResponse = DeleteWirelessGatewayTaskDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessGatewayTaskDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWirelessGatewayTaskDefinitionResponse_httpStatus' - The response's http status code.
newDeleteWirelessGatewayTaskDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWirelessGatewayTaskDefinitionResponse
newDeleteWirelessGatewayTaskDefinitionResponse
  pHttpStatus_ =
    DeleteWirelessGatewayTaskDefinitionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteWirelessGatewayTaskDefinitionResponse_httpStatus :: Lens.Lens' DeleteWirelessGatewayTaskDefinitionResponse Prelude.Int
deleteWirelessGatewayTaskDefinitionResponse_httpStatus = Lens.lens (\DeleteWirelessGatewayTaskDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteWirelessGatewayTaskDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteWirelessGatewayTaskDefinitionResponse)

instance
  Prelude.NFData
    DeleteWirelessGatewayTaskDefinitionResponse
  where
  rnf DeleteWirelessGatewayTaskDefinitionResponse' {..} =
    Prelude.rnf httpStatus
