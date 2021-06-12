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
-- Module      : Network.AWS.Glue.GetConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a connection definition from the Data Catalog.
module Network.AWS.Glue.GetConnection
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnection' smart constructor.
data GetConnection = GetConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | Allows you to retrieve the connection metadata without returning the
    -- password. For instance, the AWS Glue console uses this flag to retrieve
    -- the connection, and does not display the password. Set this parameter
    -- when the caller might not have permission to use the AWS KMS key to
    -- decrypt the password, but it does have permission to access the rest of
    -- the connection properties.
    hidePassword :: Core.Maybe Core.Bool,
    -- | The name of the connection definition to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
--
-- 'hidePassword', 'getConnection_hidePassword' - Allows you to retrieve the connection metadata without returning the
-- password. For instance, the AWS Glue console uses this flag to retrieve
-- the connection, and does not display the password. Set this parameter
-- when the caller might not have permission to use the AWS KMS key to
-- decrypt the password, but it does have permission to access the rest of
-- the connection properties.
--
-- 'name', 'getConnection_name' - The name of the connection definition to retrieve.
newGetConnection ::
  -- | 'name'
  Core.Text ->
  GetConnection
newGetConnection pName_ =
  GetConnection'
    { catalogId = Core.Nothing,
      hidePassword = Core.Nothing,
      name = pName_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
getConnection_catalogId :: Lens.Lens' GetConnection (Core.Maybe Core.Text)
getConnection_catalogId = Lens.lens (\GetConnection' {catalogId} -> catalogId) (\s@GetConnection' {} a -> s {catalogId = a} :: GetConnection)

-- | Allows you to retrieve the connection metadata without returning the
-- password. For instance, the AWS Glue console uses this flag to retrieve
-- the connection, and does not display the password. Set this parameter
-- when the caller might not have permission to use the AWS KMS key to
-- decrypt the password, but it does have permission to access the rest of
-- the connection properties.
getConnection_hidePassword :: Lens.Lens' GetConnection (Core.Maybe Core.Bool)
getConnection_hidePassword = Lens.lens (\GetConnection' {hidePassword} -> hidePassword) (\s@GetConnection' {} a -> s {hidePassword = a} :: GetConnection)

-- | The name of the connection definition to retrieve.
getConnection_name :: Lens.Lens' GetConnection Core.Text
getConnection_name = Lens.lens (\GetConnection' {name} -> name) (\s@GetConnection' {} a -> s {name = a} :: GetConnection)

instance Core.AWSRequest GetConnection where
  type
    AWSResponse GetConnection =
      GetConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionResponse'
            Core.<$> (x Core..?> "Connection")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnection

instance Core.NFData GetConnection

instance Core.ToHeaders GetConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConnection where
  toJSON GetConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            ("HidePassword" Core..=) Core.<$> hidePassword,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetConnection where
  toPath = Core.const "/"

instance Core.ToQuery GetConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { -- | The requested connection definition.
    connection :: Core.Maybe Connection,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetConnectionResponse
newGetConnectionResponse pHttpStatus_ =
  GetConnectionResponse'
    { connection = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested connection definition.
getConnectionResponse_connection :: Lens.Lens' GetConnectionResponse (Core.Maybe Connection)
getConnectionResponse_connection = Lens.lens (\GetConnectionResponse' {connection} -> connection) (\s@GetConnectionResponse' {} a -> s {connection = a} :: GetConnectionResponse)

-- | The response's http status code.
getConnectionResponse_httpStatus :: Lens.Lens' GetConnectionResponse Core.Int
getConnectionResponse_httpStatus = Lens.lens (\GetConnectionResponse' {httpStatus} -> httpStatus) (\s@GetConnectionResponse' {} a -> s {httpStatus = a} :: GetConnectionResponse)

instance Core.NFData GetConnectionResponse
