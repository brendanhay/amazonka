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
-- Module      : Amazonka.RDS.DeleteDBProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing DB proxy.
module Amazonka.RDS.DeleteDBProxy
  ( -- * Creating a Request
    DeleteDBProxy (..),
    newDeleteDBProxy,

    -- * Request Lenses
    deleteDBProxy_dbProxyName,

    -- * Destructuring the Response
    DeleteDBProxyResponse (..),
    newDeleteDBProxyResponse,

    -- * Response Lenses
    deleteDBProxyResponse_dbProxy,
    deleteDBProxyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBProxy' smart constructor.
data DeleteDBProxy = DeleteDBProxy'
  { -- | The name of the DB proxy to delete.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyName', 'deleteDBProxy_dbProxyName' - The name of the DB proxy to delete.
newDeleteDBProxy ::
  -- | 'dbProxyName'
  Prelude.Text ->
  DeleteDBProxy
newDeleteDBProxy pDBProxyName_ =
  DeleteDBProxy' {dbProxyName = pDBProxyName_}

-- | The name of the DB proxy to delete.
deleteDBProxy_dbProxyName :: Lens.Lens' DeleteDBProxy Prelude.Text
deleteDBProxy_dbProxyName = Lens.lens (\DeleteDBProxy' {dbProxyName} -> dbProxyName) (\s@DeleteDBProxy' {} a -> s {dbProxyName = a} :: DeleteDBProxy)

instance Core.AWSRequest DeleteDBProxy where
  type
    AWSResponse DeleteDBProxy =
      DeleteDBProxyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBProxyResult"
      ( \s h x ->
          DeleteDBProxyResponse'
            Prelude.<$> (x Data..@? "DBProxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBProxy where
  hashWithSalt _salt DeleteDBProxy' {..} =
    _salt `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData DeleteDBProxy where
  rnf DeleteDBProxy' {..} = Prelude.rnf dbProxyName

instance Data.ToHeaders DeleteDBProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBProxy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBProxy where
  toQuery DeleteDBProxy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBProxy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBProxyName" Data.=: dbProxyName
      ]

-- | /See:/ 'newDeleteDBProxyResponse' smart constructor.
data DeleteDBProxyResponse = DeleteDBProxyResponse'
  { -- | The data structure representing the details of the DB proxy that you
    -- delete.
    dbProxy :: Prelude.Maybe DBProxy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxy', 'deleteDBProxyResponse_dbProxy' - The data structure representing the details of the DB proxy that you
-- delete.
--
-- 'httpStatus', 'deleteDBProxyResponse_httpStatus' - The response's http status code.
newDeleteDBProxyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBProxyResponse
newDeleteDBProxyResponse pHttpStatus_ =
  DeleteDBProxyResponse'
    { dbProxy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data structure representing the details of the DB proxy that you
-- delete.
deleteDBProxyResponse_dbProxy :: Lens.Lens' DeleteDBProxyResponse (Prelude.Maybe DBProxy)
deleteDBProxyResponse_dbProxy = Lens.lens (\DeleteDBProxyResponse' {dbProxy} -> dbProxy) (\s@DeleteDBProxyResponse' {} a -> s {dbProxy = a} :: DeleteDBProxyResponse)

-- | The response's http status code.
deleteDBProxyResponse_httpStatus :: Lens.Lens' DeleteDBProxyResponse Prelude.Int
deleteDBProxyResponse_httpStatus = Lens.lens (\DeleteDBProxyResponse' {httpStatus} -> httpStatus) (\s@DeleteDBProxyResponse' {} a -> s {httpStatus = a} :: DeleteDBProxyResponse)

instance Prelude.NFData DeleteDBProxyResponse where
  rnf DeleteDBProxyResponse' {..} =
    Prelude.rnf dbProxy
      `Prelude.seq` Prelude.rnf httpStatus
