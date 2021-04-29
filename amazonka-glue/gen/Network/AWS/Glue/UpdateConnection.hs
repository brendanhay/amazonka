{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.UpdateConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connection definition in the Data Catalog.
module Network.AWS.Glue.UpdateConnection
  ( -- * Creating a Request
    UpdateConnection (..),
    newUpdateConnection,

    -- * Request Lenses
    updateConnection_catalogId,
    updateConnection_name,
    updateConnection_connectionInput,

    -- * Destructuring the Response
    UpdateConnectionResponse (..),
    newUpdateConnectionResponse,

    -- * Response Lenses
    updateConnectionResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConnection' smart constructor.
data UpdateConnection = UpdateConnection'
  { -- | The ID of the Data Catalog in which the connection resides. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection definition to update.
    name :: Prelude.Text,
    -- | A @ConnectionInput@ object that redefines the connection in question.
    connectionInput :: ConnectionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateConnection_catalogId' - The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
--
-- 'name', 'updateConnection_name' - The name of the connection definition to update.
--
-- 'connectionInput', 'updateConnection_connectionInput' - A @ConnectionInput@ object that redefines the connection in question.
newUpdateConnection ::
  -- | 'name'
  Prelude.Text ->
  -- | 'connectionInput'
  ConnectionInput ->
  UpdateConnection
newUpdateConnection pName_ pConnectionInput_ =
  UpdateConnection'
    { catalogId = Prelude.Nothing,
      name = pName_,
      connectionInput = pConnectionInput_
    }

-- | The ID of the Data Catalog in which the connection resides. If none is
-- provided, the AWS account ID is used by default.
updateConnection_catalogId :: Lens.Lens' UpdateConnection (Prelude.Maybe Prelude.Text)
updateConnection_catalogId = Lens.lens (\UpdateConnection' {catalogId} -> catalogId) (\s@UpdateConnection' {} a -> s {catalogId = a} :: UpdateConnection)

-- | The name of the connection definition to update.
updateConnection_name :: Lens.Lens' UpdateConnection Prelude.Text
updateConnection_name = Lens.lens (\UpdateConnection' {name} -> name) (\s@UpdateConnection' {} a -> s {name = a} :: UpdateConnection)

-- | A @ConnectionInput@ object that redefines the connection in question.
updateConnection_connectionInput :: Lens.Lens' UpdateConnection ConnectionInput
updateConnection_connectionInput = Lens.lens (\UpdateConnection' {connectionInput} -> connectionInput) (\s@UpdateConnection' {} a -> s {connectionInput = a} :: UpdateConnection)

instance Prelude.AWSRequest UpdateConnection where
  type Rs UpdateConnection = UpdateConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnection

instance Prelude.NFData UpdateConnection

instance Prelude.ToHeaders UpdateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateConnection" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateConnection where
  toJSON UpdateConnection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("ConnectionInput" Prelude..= connectionInput)
          ]
      )

instance Prelude.ToPath UpdateConnection where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectionResponse' smart constructor.
data UpdateConnectionResponse = UpdateConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectionResponse_httpStatus' - The response's http status code.
newUpdateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectionResponse
newUpdateConnectionResponse pHttpStatus_ =
  UpdateConnectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConnectionResponse_httpStatus :: Lens.Lens' UpdateConnectionResponse Prelude.Int
updateConnectionResponse_httpStatus = Lens.lens (\UpdateConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectionResponse' {} a -> s {httpStatus = a} :: UpdateConnectionResponse)

instance Prelude.NFData UpdateConnectionResponse
