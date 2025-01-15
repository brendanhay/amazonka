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
-- Module      : Amazonka.Glue.GetConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a connection definition from the Data Catalog.
module Amazonka.Glue.GetConnection
  ( -- * Creating a Request
    GetConnection (..),
    newGetConnection,

    -- * Request Lenses
    getConnection_catalogId,
    getConnection_hidePassword,
    getConnection_name,

    -- * Destructuring the Response
    GetConnectionResponse (..),
    newGetConnectionResponse,

    -- * Response Lenses
    getConnectionResponse_connection,
    getConnectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnection' smart constructor.
data GetConnection = GetConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Allows you to retrieve the connection metadata without returning the
    -- password. For instance, the Glue console uses this flag to retrieve the
    -- connection, and does not display the password. Set this parameter when
    -- the caller might not have permission to use the KMS key to decrypt the
    -- password, but it does have permission to access the rest of the
    -- connection properties.
    hidePassword :: Prelude.Maybe Prelude.Bool,
    -- | The name of the connection definition to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'hidePassword', 'getConnection_hidePassword' - Allows you to retrieve the connection metadata without returning the
-- password. For instance, the Glue console uses this flag to retrieve the
-- connection, and does not display the password. Set this parameter when
-- the caller might not have permission to use the KMS key to decrypt the
-- password, but it does have permission to access the rest of the
-- connection properties.
--
-- 'name', 'getConnection_name' - The name of the connection definition to retrieve.
newGetConnection ::
  -- | 'name'
  Prelude.Text ->
  GetConnection
newGetConnection pName_ =
  GetConnection'
    { catalogId = Prelude.Nothing,
      hidePassword = Prelude.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the Amazon Web Services account ID is used by default.
getConnection_catalogId :: Lens.Lens' GetConnection (Prelude.Maybe Prelude.Text)
getConnection_catalogId = Lens.lens (\GetConnection' {catalogId} -> catalogId) (\s@GetConnection' {} a -> s {catalogId = a} :: GetConnection)

-- | Allows you to retrieve the connection metadata without returning the
-- password. For instance, the Glue console uses this flag to retrieve the
-- connection, and does not display the password. Set this parameter when
-- the caller might not have permission to use the KMS key to decrypt the
-- password, but it does have permission to access the rest of the
-- connection properties.
getConnection_hidePassword :: Lens.Lens' GetConnection (Prelude.Maybe Prelude.Bool)
getConnection_hidePassword = Lens.lens (\GetConnection' {hidePassword} -> hidePassword) (\s@GetConnection' {} a -> s {hidePassword = a} :: GetConnection)

-- | The name of the connection definition to retrieve.
getConnection_name :: Lens.Lens' GetConnection Prelude.Text
getConnection_name = Lens.lens (\GetConnection' {name} -> name) (\s@GetConnection' {} a -> s {name = a} :: GetConnection)

instance Core.AWSRequest GetConnection where
  type
    AWSResponse GetConnection =
      GetConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Prelude.<$> (x Data..?> "Connection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnection where
  hashWithSalt _salt GetConnection' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` hidePassword
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetConnection where
  rnf GetConnection' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf hidePassword `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders GetConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetConnection where
  toJSON GetConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("HidePassword" Data..=) Prelude.<$> hidePassword,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { -- | The requested connection definition.
    connection :: Prelude.Maybe Connection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connection', 'getConnectionResponse_connection' - The requested connection definition.
--
-- 'httpStatus', 'getConnectionResponse_httpStatus' - The response's http status code.
newGetConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { connection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested connection definition.
getConnectionResponse_connection :: Lens.Lens' GetConnectionResponse (Prelude.Maybe Connection)
getConnectionResponse_connection = Lens.lens (\GetConnectionResponse' {connection} -> connection) (\s@GetConnectionResponse' {} a -> s {connection = a} :: GetConnectionResponse)

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Prelude.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Prelude.NFData GetConnectionResponse where
  rnf GetConnectionResponse' {..} =
    Prelude.rnf connection `Prelude.seq`
      Prelude.rnf httpStatus
