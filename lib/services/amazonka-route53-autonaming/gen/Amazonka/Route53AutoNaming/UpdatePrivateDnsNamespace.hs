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
-- Module      : Amazonka.Route53AutoNaming.UpdatePrivateDnsNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a private DNS namespace.
module Amazonka.Route53AutoNaming.UpdatePrivateDnsNamespace
  ( -- * Creating a Request
    UpdatePrivateDnsNamespace (..),
    newUpdatePrivateDnsNamespace,

    -- * Request Lenses
    updatePrivateDnsNamespace_updaterRequestId,
    updatePrivateDnsNamespace_id,
    updatePrivateDnsNamespace_namespace,

    -- * Destructuring the Response
    UpdatePrivateDnsNamespaceResponse (..),
    newUpdatePrivateDnsNamespaceResponse,

    -- * Response Lenses
    updatePrivateDnsNamespaceResponse_operationId,
    updatePrivateDnsNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newUpdatePrivateDnsNamespace' smart constructor.
data UpdatePrivateDnsNamespace = UpdatePrivateDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @UpdatePrivateDnsNamespace@ requests to be retried without the risk of
    -- running the operation twice. @UpdaterRequestId@ can be any unique string
    -- (for example, a date\/timestamp).
    updaterRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the namespace that you want to update.
    id :: Prelude.Text,
    -- | Updated properties for the private DNS namespace.
    namespace :: PrivateDnsNamespaceChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrivateDnsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updaterRequestId', 'updatePrivateDnsNamespace_updaterRequestId' - A unique string that identifies the request and that allows failed
-- @UpdatePrivateDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @UpdaterRequestId@ can be any unique string
-- (for example, a date\/timestamp).
--
-- 'id', 'updatePrivateDnsNamespace_id' - The ID of the namespace that you want to update.
--
-- 'namespace', 'updatePrivateDnsNamespace_namespace' - Updated properties for the private DNS namespace.
newUpdatePrivateDnsNamespace ::
  -- | 'id'
  Prelude.Text ->
  -- | 'namespace'
  PrivateDnsNamespaceChange ->
  UpdatePrivateDnsNamespace
newUpdatePrivateDnsNamespace pId_ pNamespace_ =
  UpdatePrivateDnsNamespace'
    { updaterRequestId =
        Prelude.Nothing,
      id = pId_,
      namespace = pNamespace_
    }

-- | A unique string that identifies the request and that allows failed
-- @UpdatePrivateDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @UpdaterRequestId@ can be any unique string
-- (for example, a date\/timestamp).
updatePrivateDnsNamespace_updaterRequestId :: Lens.Lens' UpdatePrivateDnsNamespace (Prelude.Maybe Prelude.Text)
updatePrivateDnsNamespace_updaterRequestId = Lens.lens (\UpdatePrivateDnsNamespace' {updaterRequestId} -> updaterRequestId) (\s@UpdatePrivateDnsNamespace' {} a -> s {updaterRequestId = a} :: UpdatePrivateDnsNamespace)

-- | The ID of the namespace that you want to update.
updatePrivateDnsNamespace_id :: Lens.Lens' UpdatePrivateDnsNamespace Prelude.Text
updatePrivateDnsNamespace_id = Lens.lens (\UpdatePrivateDnsNamespace' {id} -> id) (\s@UpdatePrivateDnsNamespace' {} a -> s {id = a} :: UpdatePrivateDnsNamespace)

-- | Updated properties for the private DNS namespace.
updatePrivateDnsNamespace_namespace :: Lens.Lens' UpdatePrivateDnsNamespace PrivateDnsNamespaceChange
updatePrivateDnsNamespace_namespace = Lens.lens (\UpdatePrivateDnsNamespace' {namespace} -> namespace) (\s@UpdatePrivateDnsNamespace' {} a -> s {namespace = a} :: UpdatePrivateDnsNamespace)

instance Core.AWSRequest UpdatePrivateDnsNamespace where
  type
    AWSResponse UpdatePrivateDnsNamespace =
      UpdatePrivateDnsNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePrivateDnsNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePrivateDnsNamespace where
  hashWithSalt _salt UpdatePrivateDnsNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` updaterRequestId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData UpdatePrivateDnsNamespace where
  rnf UpdatePrivateDnsNamespace' {..} =
    Prelude.rnf updaterRequestId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders UpdatePrivateDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.UpdatePrivateDnsNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePrivateDnsNamespace where
  toJSON UpdatePrivateDnsNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdaterRequestId" Data..=)
              Prelude.<$> updaterRequestId,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Namespace" Data..= namespace)
          ]
      )

instance Data.ToPath UpdatePrivateDnsNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePrivateDnsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePrivateDnsNamespaceResponse' smart constructor.
data UpdatePrivateDnsNamespaceResponse = UpdatePrivateDnsNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrivateDnsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updatePrivateDnsNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'updatePrivateDnsNamespaceResponse_httpStatus' - The response's http status code.
newUpdatePrivateDnsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePrivateDnsNamespaceResponse
newUpdatePrivateDnsNamespaceResponse pHttpStatus_ =
  UpdatePrivateDnsNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
updatePrivateDnsNamespaceResponse_operationId :: Lens.Lens' UpdatePrivateDnsNamespaceResponse (Prelude.Maybe Prelude.Text)
updatePrivateDnsNamespaceResponse_operationId = Lens.lens (\UpdatePrivateDnsNamespaceResponse' {operationId} -> operationId) (\s@UpdatePrivateDnsNamespaceResponse' {} a -> s {operationId = a} :: UpdatePrivateDnsNamespaceResponse)

-- | The response's http status code.
updatePrivateDnsNamespaceResponse_httpStatus :: Lens.Lens' UpdatePrivateDnsNamespaceResponse Prelude.Int
updatePrivateDnsNamespaceResponse_httpStatus = Lens.lens (\UpdatePrivateDnsNamespaceResponse' {httpStatus} -> httpStatus) (\s@UpdatePrivateDnsNamespaceResponse' {} a -> s {httpStatus = a} :: UpdatePrivateDnsNamespaceResponse)

instance
  Prelude.NFData
    UpdatePrivateDnsNamespaceResponse
  where
  rnf UpdatePrivateDnsNamespaceResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
