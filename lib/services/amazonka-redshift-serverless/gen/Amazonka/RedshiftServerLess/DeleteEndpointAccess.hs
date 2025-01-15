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
-- Module      : Amazonka.RedshiftServerLess.DeleteEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift Serverless managed VPC endpoint.
module Amazonka.RedshiftServerLess.DeleteEndpointAccess
  ( -- * Creating a Request
    DeleteEndpointAccess (..),
    newDeleteEndpointAccess,

    -- * Request Lenses
    deleteEndpointAccess_endpointName,

    -- * Destructuring the Response
    DeleteEndpointAccessResponse (..),
    newDeleteEndpointAccessResponse,

    -- * Response Lenses
    deleteEndpointAccessResponse_endpoint,
    deleteEndpointAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEndpointAccess' smart constructor.
data DeleteEndpointAccess = DeleteEndpointAccess'
  { -- | The name of the VPC endpoint to delete.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'deleteEndpointAccess_endpointName' - The name of the VPC endpoint to delete.
newDeleteEndpointAccess ::
  -- | 'endpointName'
  Prelude.Text ->
  DeleteEndpointAccess
newDeleteEndpointAccess pEndpointName_ =
  DeleteEndpointAccess'
    { endpointName =
        pEndpointName_
    }

-- | The name of the VPC endpoint to delete.
deleteEndpointAccess_endpointName :: Lens.Lens' DeleteEndpointAccess Prelude.Text
deleteEndpointAccess_endpointName = Lens.lens (\DeleteEndpointAccess' {endpointName} -> endpointName) (\s@DeleteEndpointAccess' {} a -> s {endpointName = a} :: DeleteEndpointAccess)

instance Core.AWSRequest DeleteEndpointAccess where
  type
    AWSResponse DeleteEndpointAccess =
      DeleteEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointAccessResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEndpointAccess where
  hashWithSalt _salt DeleteEndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData DeleteEndpointAccess where
  rnf DeleteEndpointAccess' {..} =
    Prelude.rnf endpointName

instance Data.ToHeaders DeleteEndpointAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.DeleteEndpointAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEndpointAccess where
  toJSON DeleteEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("endpointName" Data..= endpointName)]
      )

instance Data.ToPath DeleteEndpointAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointAccessResponse' smart constructor.
data DeleteEndpointAccessResponse = DeleteEndpointAccessResponse'
  { -- | The deleted VPC endpoint.
    endpoint :: Prelude.Maybe EndpointAccess,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'deleteEndpointAccessResponse_endpoint' - The deleted VPC endpoint.
--
-- 'httpStatus', 'deleteEndpointAccessResponse_httpStatus' - The response's http status code.
newDeleteEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEndpointAccessResponse
newDeleteEndpointAccessResponse pHttpStatus_ =
  DeleteEndpointAccessResponse'
    { endpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted VPC endpoint.
deleteEndpointAccessResponse_endpoint :: Lens.Lens' DeleteEndpointAccessResponse (Prelude.Maybe EndpointAccess)
deleteEndpointAccessResponse_endpoint = Lens.lens (\DeleteEndpointAccessResponse' {endpoint} -> endpoint) (\s@DeleteEndpointAccessResponse' {} a -> s {endpoint = a} :: DeleteEndpointAccessResponse)

-- | The response's http status code.
deleteEndpointAccessResponse_httpStatus :: Lens.Lens' DeleteEndpointAccessResponse Prelude.Int
deleteEndpointAccessResponse_httpStatus = Lens.lens (\DeleteEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointAccessResponse' {} a -> s {httpStatus = a} :: DeleteEndpointAccessResponse)

instance Prelude.NFData DeleteEndpointAccessResponse where
  rnf DeleteEndpointAccessResponse' {..} =
    Prelude.rnf endpoint `Prelude.seq`
      Prelude.rnf httpStatus
