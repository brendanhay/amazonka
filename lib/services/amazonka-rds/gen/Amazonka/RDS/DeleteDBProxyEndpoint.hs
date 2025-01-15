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
-- Module      : Amazonka.RDS.DeleteDBProxyEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @DBProxyEndpoint@. Doing so removes the ability to access the
-- DB proxy using the endpoint that you defined. The endpoint that you
-- delete might have provided capabilities such as read\/write or read-only
-- operations, or using a different VPC than the DB proxy\'s default VPC.
module Amazonka.RDS.DeleteDBProxyEndpoint
  ( -- * Creating a Request
    DeleteDBProxyEndpoint (..),
    newDeleteDBProxyEndpoint,

    -- * Request Lenses
    deleteDBProxyEndpoint_dbProxyEndpointName,

    -- * Destructuring the Response
    DeleteDBProxyEndpointResponse (..),
    newDeleteDBProxyEndpointResponse,

    -- * Response Lenses
    deleteDBProxyEndpointResponse_dbProxyEndpoint,
    deleteDBProxyEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBProxyEndpoint' smart constructor.
data DeleteDBProxyEndpoint = DeleteDBProxyEndpoint'
  { -- | The name of the DB proxy endpoint to delete.
    dbProxyEndpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBProxyEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpointName', 'deleteDBProxyEndpoint_dbProxyEndpointName' - The name of the DB proxy endpoint to delete.
newDeleteDBProxyEndpoint ::
  -- | 'dbProxyEndpointName'
  Prelude.Text ->
  DeleteDBProxyEndpoint
newDeleteDBProxyEndpoint pDBProxyEndpointName_ =
  DeleteDBProxyEndpoint'
    { dbProxyEndpointName =
        pDBProxyEndpointName_
    }

-- | The name of the DB proxy endpoint to delete.
deleteDBProxyEndpoint_dbProxyEndpointName :: Lens.Lens' DeleteDBProxyEndpoint Prelude.Text
deleteDBProxyEndpoint_dbProxyEndpointName = Lens.lens (\DeleteDBProxyEndpoint' {dbProxyEndpointName} -> dbProxyEndpointName) (\s@DeleteDBProxyEndpoint' {} a -> s {dbProxyEndpointName = a} :: DeleteDBProxyEndpoint)

instance Core.AWSRequest DeleteDBProxyEndpoint where
  type
    AWSResponse DeleteDBProxyEndpoint =
      DeleteDBProxyEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBProxyEndpointResult"
      ( \s h x ->
          DeleteDBProxyEndpointResponse'
            Prelude.<$> (x Data..@? "DBProxyEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBProxyEndpoint where
  hashWithSalt _salt DeleteDBProxyEndpoint' {..} =
    _salt `Prelude.hashWithSalt` dbProxyEndpointName

instance Prelude.NFData DeleteDBProxyEndpoint where
  rnf DeleteDBProxyEndpoint' {..} =
    Prelude.rnf dbProxyEndpointName

instance Data.ToHeaders DeleteDBProxyEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBProxyEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBProxyEndpoint where
  toQuery DeleteDBProxyEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBProxyEndpoint" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBProxyEndpointName" Data.=: dbProxyEndpointName
      ]

-- | /See:/ 'newDeleteDBProxyEndpointResponse' smart constructor.
data DeleteDBProxyEndpointResponse = DeleteDBProxyEndpointResponse'
  { -- | The data structure representing the details of the DB proxy endpoint
    -- that you delete.
    dbProxyEndpoint :: Prelude.Maybe DBProxyEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBProxyEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpoint', 'deleteDBProxyEndpointResponse_dbProxyEndpoint' - The data structure representing the details of the DB proxy endpoint
-- that you delete.
--
-- 'httpStatus', 'deleteDBProxyEndpointResponse_httpStatus' - The response's http status code.
newDeleteDBProxyEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBProxyEndpointResponse
newDeleteDBProxyEndpointResponse pHttpStatus_ =
  DeleteDBProxyEndpointResponse'
    { dbProxyEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data structure representing the details of the DB proxy endpoint
-- that you delete.
deleteDBProxyEndpointResponse_dbProxyEndpoint :: Lens.Lens' DeleteDBProxyEndpointResponse (Prelude.Maybe DBProxyEndpoint)
deleteDBProxyEndpointResponse_dbProxyEndpoint = Lens.lens (\DeleteDBProxyEndpointResponse' {dbProxyEndpoint} -> dbProxyEndpoint) (\s@DeleteDBProxyEndpointResponse' {} a -> s {dbProxyEndpoint = a} :: DeleteDBProxyEndpointResponse)

-- | The response's http status code.
deleteDBProxyEndpointResponse_httpStatus :: Lens.Lens' DeleteDBProxyEndpointResponse Prelude.Int
deleteDBProxyEndpointResponse_httpStatus = Lens.lens (\DeleteDBProxyEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteDBProxyEndpointResponse' {} a -> s {httpStatus = a} :: DeleteDBProxyEndpointResponse)

instance Prelude.NFData DeleteDBProxyEndpointResponse where
  rnf DeleteDBProxyEndpointResponse' {..} =
    Prelude.rnf dbProxyEndpoint `Prelude.seq`
      Prelude.rnf httpStatus
