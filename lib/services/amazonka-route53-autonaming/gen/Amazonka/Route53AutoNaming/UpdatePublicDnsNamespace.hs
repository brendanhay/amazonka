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
-- Module      : Amazonka.Route53AutoNaming.UpdatePublicDnsNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a public DNS namespace.
module Amazonka.Route53AutoNaming.UpdatePublicDnsNamespace
  ( -- * Creating a Request
    UpdatePublicDnsNamespace (..),
    newUpdatePublicDnsNamespace,

    -- * Request Lenses
    updatePublicDnsNamespace_updaterRequestId,
    updatePublicDnsNamespace_id,
    updatePublicDnsNamespace_namespace,

    -- * Destructuring the Response
    UpdatePublicDnsNamespaceResponse (..),
    newUpdatePublicDnsNamespaceResponse,

    -- * Response Lenses
    updatePublicDnsNamespaceResponse_operationId,
    updatePublicDnsNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newUpdatePublicDnsNamespace' smart constructor.
data UpdatePublicDnsNamespace = UpdatePublicDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @UpdatePublicDnsNamespace@ requests to be retried without the risk of
    -- running the operation twice. @UpdaterRequestId@ can be any unique string
    -- (for example, a date\/timestamp).
    updaterRequestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the namespace being updated.
    id :: Prelude.Text,
    -- | Updated properties for the public DNS namespace.
    namespace :: PublicDnsNamespaceChange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePublicDnsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updaterRequestId', 'updatePublicDnsNamespace_updaterRequestId' - A unique string that identifies the request and that allows failed
-- @UpdatePublicDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @UpdaterRequestId@ can be any unique string
-- (for example, a date\/timestamp).
--
-- 'id', 'updatePublicDnsNamespace_id' - The ID of the namespace being updated.
--
-- 'namespace', 'updatePublicDnsNamespace_namespace' - Updated properties for the public DNS namespace.
newUpdatePublicDnsNamespace ::
  -- | 'id'
  Prelude.Text ->
  -- | 'namespace'
  PublicDnsNamespaceChange ->
  UpdatePublicDnsNamespace
newUpdatePublicDnsNamespace pId_ pNamespace_ =
  UpdatePublicDnsNamespace'
    { updaterRequestId =
        Prelude.Nothing,
      id = pId_,
      namespace = pNamespace_
    }

-- | A unique string that identifies the request and that allows failed
-- @UpdatePublicDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @UpdaterRequestId@ can be any unique string
-- (for example, a date\/timestamp).
updatePublicDnsNamespace_updaterRequestId :: Lens.Lens' UpdatePublicDnsNamespace (Prelude.Maybe Prelude.Text)
updatePublicDnsNamespace_updaterRequestId = Lens.lens (\UpdatePublicDnsNamespace' {updaterRequestId} -> updaterRequestId) (\s@UpdatePublicDnsNamespace' {} a -> s {updaterRequestId = a} :: UpdatePublicDnsNamespace)

-- | The ID of the namespace being updated.
updatePublicDnsNamespace_id :: Lens.Lens' UpdatePublicDnsNamespace Prelude.Text
updatePublicDnsNamespace_id = Lens.lens (\UpdatePublicDnsNamespace' {id} -> id) (\s@UpdatePublicDnsNamespace' {} a -> s {id = a} :: UpdatePublicDnsNamespace)

-- | Updated properties for the public DNS namespace.
updatePublicDnsNamespace_namespace :: Lens.Lens' UpdatePublicDnsNamespace PublicDnsNamespaceChange
updatePublicDnsNamespace_namespace = Lens.lens (\UpdatePublicDnsNamespace' {namespace} -> namespace) (\s@UpdatePublicDnsNamespace' {} a -> s {namespace = a} :: UpdatePublicDnsNamespace)

instance Core.AWSRequest UpdatePublicDnsNamespace where
  type
    AWSResponse UpdatePublicDnsNamespace =
      UpdatePublicDnsNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePublicDnsNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePublicDnsNamespace where
  hashWithSalt _salt UpdatePublicDnsNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` updaterRequestId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData UpdatePublicDnsNamespace where
  rnf UpdatePublicDnsNamespace' {..} =
    Prelude.rnf updaterRequestId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders UpdatePublicDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.UpdatePublicDnsNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePublicDnsNamespace where
  toJSON UpdatePublicDnsNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdaterRequestId" Data..=)
              Prelude.<$> updaterRequestId,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Namespace" Data..= namespace)
          ]
      )

instance Data.ToPath UpdatePublicDnsNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePublicDnsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePublicDnsNamespaceResponse' smart constructor.
data UpdatePublicDnsNamespaceResponse = UpdatePublicDnsNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePublicDnsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updatePublicDnsNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'updatePublicDnsNamespaceResponse_httpStatus' - The response's http status code.
newUpdatePublicDnsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePublicDnsNamespaceResponse
newUpdatePublicDnsNamespaceResponse pHttpStatus_ =
  UpdatePublicDnsNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
updatePublicDnsNamespaceResponse_operationId :: Lens.Lens' UpdatePublicDnsNamespaceResponse (Prelude.Maybe Prelude.Text)
updatePublicDnsNamespaceResponse_operationId = Lens.lens (\UpdatePublicDnsNamespaceResponse' {operationId} -> operationId) (\s@UpdatePublicDnsNamespaceResponse' {} a -> s {operationId = a} :: UpdatePublicDnsNamespaceResponse)

-- | The response's http status code.
updatePublicDnsNamespaceResponse_httpStatus :: Lens.Lens' UpdatePublicDnsNamespaceResponse Prelude.Int
updatePublicDnsNamespaceResponse_httpStatus = Lens.lens (\UpdatePublicDnsNamespaceResponse' {httpStatus} -> httpStatus) (\s@UpdatePublicDnsNamespaceResponse' {} a -> s {httpStatus = a} :: UpdatePublicDnsNamespaceResponse)

instance
  Prelude.NFData
    UpdatePublicDnsNamespaceResponse
  where
  rnf UpdatePublicDnsNamespaceResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
