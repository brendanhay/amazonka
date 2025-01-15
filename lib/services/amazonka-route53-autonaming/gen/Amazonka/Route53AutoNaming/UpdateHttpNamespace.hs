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
-- Module      : Amazonka.Route53AutoNaming.UpdateHttpNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an HTTP namespace.
module Amazonka.Route53AutoNaming.UpdateHttpNamespace
  ( -- * Creating a Request
    UpdateHttpNamespace (..),
    newUpdateHttpNamespace,

    -- * Request Lenses
    updateHttpNamespace_updaterRequestId,
    updateHttpNamespace_id,
    updateHttpNamespace_namespace,

    -- * Destructuring the Response
    UpdateHttpNamespaceResponse (..),
    newUpdateHttpNamespaceResponse,

    -- * Response Lenses
    updateHttpNamespaceResponse_operationId,
    updateHttpNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newUpdateHttpNamespace' smart constructor.
data UpdateHttpNamespace = UpdateHttpNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @UpdateHttpNamespace@ requests to be retried without the risk of running
    -- the operation twice. @UpdaterRequestId@ can be any unique string (for
    -- example, a date\/timestamp).
    updaterRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the namespace that you want to update.
    id :: Prelude.Text,
    -- | Updated properties for the the HTTP namespace.
    namespace :: HttpNamespaceChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHttpNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updaterRequestId', 'updateHttpNamespace_updaterRequestId' - A unique string that identifies the request and that allows failed
-- @UpdateHttpNamespace@ requests to be retried without the risk of running
-- the operation twice. @UpdaterRequestId@ can be any unique string (for
-- example, a date\/timestamp).
--
-- 'id', 'updateHttpNamespace_id' - The ID of the namespace that you want to update.
--
-- 'namespace', 'updateHttpNamespace_namespace' - Updated properties for the the HTTP namespace.
newUpdateHttpNamespace ::
  -- | 'id'
  Prelude.Text ->
  -- | 'namespace'
  HttpNamespaceChange ->
  UpdateHttpNamespace
newUpdateHttpNamespace pId_ pNamespace_ =
  UpdateHttpNamespace'
    { updaterRequestId =
        Prelude.Nothing,
      id = pId_,
      namespace = pNamespace_
    }

-- | A unique string that identifies the request and that allows failed
-- @UpdateHttpNamespace@ requests to be retried without the risk of running
-- the operation twice. @UpdaterRequestId@ can be any unique string (for
-- example, a date\/timestamp).
updateHttpNamespace_updaterRequestId :: Lens.Lens' UpdateHttpNamespace (Prelude.Maybe Prelude.Text)
updateHttpNamespace_updaterRequestId = Lens.lens (\UpdateHttpNamespace' {updaterRequestId} -> updaterRequestId) (\s@UpdateHttpNamespace' {} a -> s {updaterRequestId = a} :: UpdateHttpNamespace)

-- | The ID of the namespace that you want to update.
updateHttpNamespace_id :: Lens.Lens' UpdateHttpNamespace Prelude.Text
updateHttpNamespace_id = Lens.lens (\UpdateHttpNamespace' {id} -> id) (\s@UpdateHttpNamespace' {} a -> s {id = a} :: UpdateHttpNamespace)

-- | Updated properties for the the HTTP namespace.
updateHttpNamespace_namespace :: Lens.Lens' UpdateHttpNamespace HttpNamespaceChange
updateHttpNamespace_namespace = Lens.lens (\UpdateHttpNamespace' {namespace} -> namespace) (\s@UpdateHttpNamespace' {} a -> s {namespace = a} :: UpdateHttpNamespace)

instance Core.AWSRequest UpdateHttpNamespace where
  type
    AWSResponse UpdateHttpNamespace =
      UpdateHttpNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateHttpNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateHttpNamespace where
  hashWithSalt _salt UpdateHttpNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` updaterRequestId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData UpdateHttpNamespace where
  rnf UpdateHttpNamespace' {..} =
    Prelude.rnf updaterRequestId `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf namespace

instance Data.ToHeaders UpdateHttpNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.UpdateHttpNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateHttpNamespace where
  toJSON UpdateHttpNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdaterRequestId" Data..=)
              Prelude.<$> updaterRequestId,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Namespace" Data..= namespace)
          ]
      )

instance Data.ToPath UpdateHttpNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateHttpNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHttpNamespaceResponse' smart constructor.
data UpdateHttpNamespaceResponse = UpdateHttpNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHttpNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateHttpNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'updateHttpNamespaceResponse_httpStatus' - The response's http status code.
newUpdateHttpNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateHttpNamespaceResponse
newUpdateHttpNamespaceResponse pHttpStatus_ =
  UpdateHttpNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
updateHttpNamespaceResponse_operationId :: Lens.Lens' UpdateHttpNamespaceResponse (Prelude.Maybe Prelude.Text)
updateHttpNamespaceResponse_operationId = Lens.lens (\UpdateHttpNamespaceResponse' {operationId} -> operationId) (\s@UpdateHttpNamespaceResponse' {} a -> s {operationId = a} :: UpdateHttpNamespaceResponse)

-- | The response's http status code.
updateHttpNamespaceResponse_httpStatus :: Lens.Lens' UpdateHttpNamespaceResponse Prelude.Int
updateHttpNamespaceResponse_httpStatus = Lens.lens (\UpdateHttpNamespaceResponse' {httpStatus} -> httpStatus) (\s@UpdateHttpNamespaceResponse' {} a -> s {httpStatus = a} :: UpdateHttpNamespaceResponse)

instance Prelude.NFData UpdateHttpNamespaceResponse where
  rnf UpdateHttpNamespaceResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
