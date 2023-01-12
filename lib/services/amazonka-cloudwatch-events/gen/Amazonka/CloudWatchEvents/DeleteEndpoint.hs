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
-- Module      : Amazonka.CloudWatchEvents.DeleteEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing global endpoint. For more information about global
-- endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide.
module Amazonka.CloudWatchEvents.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_name,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,

    -- * Response Lenses
    deleteEndpointResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The name of the endpoint you want to delete. For example,
    -- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@..
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEndpoint_name' - The name of the endpoint you want to delete. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@..
newDeleteEndpoint ::
  -- | 'name'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pName_ =
  DeleteEndpoint' {name = pName_}

-- | The name of the endpoint you want to delete. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@..
deleteEndpoint_name :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_name = Lens.lens (\DeleteEndpoint' {name} -> name) (\s@DeleteEndpoint' {} a -> s {name = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEndpoint where
  hashWithSalt _salt DeleteEndpoint' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEndpoint where
  rnf DeleteEndpoint' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DeleteEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEndpointResponse_httpStatus' - The response's http status code.
newDeleteEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEndpointResponse
newDeleteEndpointResponse pHttpStatus_ =
  DeleteEndpointResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteEndpointResponse_httpStatus :: Lens.Lens' DeleteEndpointResponse Prelude.Int
deleteEndpointResponse_httpStatus = Lens.lens (\DeleteEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointResponse' {} a -> s {httpStatus = a} :: DeleteEndpointResponse)

instance Prelude.NFData DeleteEndpointResponse where
  rnf DeleteEndpointResponse' {..} =
    Prelude.rnf httpStatus
