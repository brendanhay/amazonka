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
-- Module      : Amazonka.DMS.DeleteEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified endpoint.
--
-- All tasks associated with the endpoint must be deleted before you can
-- delete the endpoint.
module Amazonka.DMS.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_endpointArn,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,

    -- * Response Lenses
    deleteEndpointResponse_endpoint,
    deleteEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Text
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
-- 'endpointArn', 'deleteEndpoint_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newDeleteEndpoint ::
  -- | 'endpointArn'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pEndpointArn_ =
  DeleteEndpoint' {endpointArn = pEndpointArn_}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
deleteEndpoint_endpointArn :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_endpointArn = Lens.lens (\DeleteEndpoint' {endpointArn} -> endpointArn) (\s@DeleteEndpoint' {} a -> s {endpointArn = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Prelude.<$> (x Data..?> "Endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEndpoint where
  hashWithSalt _salt DeleteEndpoint' {..} =
    _salt `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData DeleteEndpoint where
  rnf DeleteEndpoint' {..} = Prelude.rnf endpointArn

instance Data.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DeleteEndpoint" ::
                          Prelude.ByteString
                      ),
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
          [Prelude.Just ("EndpointArn" Data..= endpointArn)]
      )

instance Data.ToPath DeleteEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The endpoint that was deleted.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'deleteEndpointResponse_endpoint' - The endpoint that was deleted.
--
-- 'httpStatus', 'deleteEndpointResponse_httpStatus' - The response's http status code.
newDeleteEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEndpointResponse
newDeleteEndpointResponse pHttpStatus_ =
  DeleteEndpointResponse'
    { endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint that was deleted.
deleteEndpointResponse_endpoint :: Lens.Lens' DeleteEndpointResponse (Prelude.Maybe Endpoint)
deleteEndpointResponse_endpoint = Lens.lens (\DeleteEndpointResponse' {endpoint} -> endpoint) (\s@DeleteEndpointResponse' {} a -> s {endpoint = a} :: DeleteEndpointResponse)

-- | The response's http status code.
deleteEndpointResponse_httpStatus :: Lens.Lens' DeleteEndpointResponse Prelude.Int
deleteEndpointResponse_httpStatus = Lens.lens (\DeleteEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointResponse' {} a -> s {httpStatus = a} :: DeleteEndpointResponse)

instance Prelude.NFData DeleteEndpointResponse where
  rnf DeleteEndpointResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf httpStatus
