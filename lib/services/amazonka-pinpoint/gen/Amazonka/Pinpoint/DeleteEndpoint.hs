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
-- Module      : Amazonka.Pinpoint.DeleteEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint from an application.
module Amazonka.Pinpoint.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_applicationId,
    deleteEndpoint_endpointId,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,

    -- * Response Lenses
    deleteEndpointResponse_httpStatus,
    deleteEndpointResponse_endpointResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the endpoint.
    endpointId :: Prelude.Text
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
-- 'applicationId', 'deleteEndpoint_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointId', 'deleteEndpoint_endpointId' - The unique identifier for the endpoint.
newDeleteEndpoint ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pApplicationId_ pEndpointId_ =
  DeleteEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteEndpoint_applicationId :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_applicationId = Lens.lens (\DeleteEndpoint' {applicationId} -> applicationId) (\s@DeleteEndpoint' {} a -> s {applicationId = a} :: DeleteEndpoint)

-- | The unique identifier for the endpoint.
deleteEndpoint_endpointId :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_endpointId = Lens.lens (\DeleteEndpoint' {endpointId} -> endpointId) (\s@DeleteEndpoint' {} a -> s {endpointId = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteEndpoint where
  hashWithSalt _salt DeleteEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData DeleteEndpoint where
  rnf DeleteEndpoint' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf endpointId

instance Data.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEndpoint where
  toPath DeleteEndpoint' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/endpoints/",
        Data.toBS endpointId
      ]

instance Data.ToQuery DeleteEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    endpointResponse :: EndpointResponse
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
--
-- 'endpointResponse', 'deleteEndpointResponse_endpointResponse' - Undocumented member.
newDeleteEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'endpointResponse'
  EndpointResponse ->
  DeleteEndpointResponse
newDeleteEndpointResponse
  pHttpStatus_
  pEndpointResponse_ =
    DeleteEndpointResponse'
      { httpStatus = pHttpStatus_,
        endpointResponse = pEndpointResponse_
      }

-- | The response's http status code.
deleteEndpointResponse_httpStatus :: Lens.Lens' DeleteEndpointResponse Prelude.Int
deleteEndpointResponse_httpStatus = Lens.lens (\DeleteEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointResponse' {} a -> s {httpStatus = a} :: DeleteEndpointResponse)

-- | Undocumented member.
deleteEndpointResponse_endpointResponse :: Lens.Lens' DeleteEndpointResponse EndpointResponse
deleteEndpointResponse_endpointResponse = Lens.lens (\DeleteEndpointResponse' {endpointResponse} -> endpointResponse) (\s@DeleteEndpointResponse' {} a -> s {endpointResponse = a} :: DeleteEndpointResponse)

instance Prelude.NFData DeleteEndpointResponse where
  rnf DeleteEndpointResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointResponse
