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
-- Module      : Network.AWS.Glue.CreateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection definition in the Data Catalog.
module Network.AWS.Glue.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_catalogId,
    createConnection_connectionInput,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The ID of the Data Catalog in which to create the connection. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | A @ConnectionInput@ object defining the connection to create.
    connectionInput :: ConnectionInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createConnection_catalogId' - The ID of the Data Catalog in which to create the connection. If none is
-- provided, the AWS account ID is used by default.
--
-- 'connectionInput', 'createConnection_connectionInput' - A @ConnectionInput@ object defining the connection to create.
newCreateConnection ::
  -- | 'connectionInput'
  ConnectionInput ->
  CreateConnection
newCreateConnection pConnectionInput_ =
  CreateConnection'
    { catalogId = Core.Nothing,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which to create the connection. If none is
-- provided, the AWS account ID is used by default.
createConnection_catalogId :: Lens.Lens' CreateConnection (Core.Maybe Core.Text)
createConnection_catalogId = Lens.lens (\CreateConnection' {catalogId} -> catalogId) (\s@CreateConnection' {} a -> s {catalogId = a} :: CreateConnection)

-- | A @ConnectionInput@ object defining the connection to create.
createConnection_connectionInput :: Lens.Lens' CreateConnection ConnectionInput
createConnection_connectionInput = Lens.lens (\CreateConnection' {connectionInput} -> connectionInput) (\s@CreateConnection' {} a -> s {connectionInput = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type
    AWSResponse CreateConnection =
      CreateConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConnectionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateConnection

instance Core.NFData CreateConnection

instance Core.ToHeaders CreateConnection where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateConnection" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just
              ("ConnectionInput" Core..= connectionInput)
          ]
      )

instance Core.ToPath CreateConnection where
  toPath = Core.const "/"

instance Core.ToQuery CreateConnection where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ =
  CreateConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Core.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

instance Core.NFData CreateConnectionResponse
