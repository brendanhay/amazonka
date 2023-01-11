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
-- Module      : Amazonka.QuickSight.DeleteNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace and the users and groups that are associated with
-- the namespace. This is an asynchronous process. Assets including
-- dashboards, analyses, datasets and data sources are not deleted. To
-- delete these assets, you use the API operations for the relevant asset.
module Amazonka.QuickSight.DeleteNamespace
  ( -- * Creating a Request
    DeleteNamespace (..),
    newDeleteNamespace,

    -- * Request Lenses
    deleteNamespace_awsAccountId,
    deleteNamespace_namespace,

    -- * Destructuring the Response
    DeleteNamespaceResponse (..),
    newDeleteNamespaceResponse,

    -- * Response Lenses
    deleteNamespaceResponse_requestId,
    deleteNamespaceResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNamespace' smart constructor.
data DeleteNamespace = DeleteNamespace'
  { -- | The ID for the Amazon Web Services account that you want to delete the
    -- Amazon QuickSight namespace from.
    awsAccountId :: Prelude.Text,
    -- | The namespace that you want to delete.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteNamespace_awsAccountId' - The ID for the Amazon Web Services account that you want to delete the
-- Amazon QuickSight namespace from.
--
-- 'namespace', 'deleteNamespace_namespace' - The namespace that you want to delete.
newDeleteNamespace ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DeleteNamespace
newDeleteNamespace pAwsAccountId_ pNamespace_ =
  DeleteNamespace'
    { awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | The ID for the Amazon Web Services account that you want to delete the
-- Amazon QuickSight namespace from.
deleteNamespace_awsAccountId :: Lens.Lens' DeleteNamespace Prelude.Text
deleteNamespace_awsAccountId = Lens.lens (\DeleteNamespace' {awsAccountId} -> awsAccountId) (\s@DeleteNamespace' {} a -> s {awsAccountId = a} :: DeleteNamespace)

-- | The namespace that you want to delete.
deleteNamespace_namespace :: Lens.Lens' DeleteNamespace Prelude.Text
deleteNamespace_namespace = Lens.lens (\DeleteNamespace' {namespace} -> namespace) (\s@DeleteNamespace' {} a -> s {namespace = a} :: DeleteNamespace)

instance Core.AWSRequest DeleteNamespace where
  type
    AWSResponse DeleteNamespace =
      DeleteNamespaceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNamespace where
  hashWithSalt _salt DeleteNamespace' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DeleteNamespace where
  rnf DeleteNamespace' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DeleteNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteNamespace where
  toPath DeleteNamespace' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace
      ]

instance Data.ToQuery DeleteNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteNamespaceResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteNamespaceResponse_status' - The HTTP status of the request.
newDeleteNamespaceResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteNamespaceResponse
newDeleteNamespaceResponse pStatus_ =
  DeleteNamespaceResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteNamespaceResponse_requestId :: Lens.Lens' DeleteNamespaceResponse (Prelude.Maybe Prelude.Text)
deleteNamespaceResponse_requestId = Lens.lens (\DeleteNamespaceResponse' {requestId} -> requestId) (\s@DeleteNamespaceResponse' {} a -> s {requestId = a} :: DeleteNamespaceResponse)

-- | The HTTP status of the request.
deleteNamespaceResponse_status :: Lens.Lens' DeleteNamespaceResponse Prelude.Int
deleteNamespaceResponse_status = Lens.lens (\DeleteNamespaceResponse' {status} -> status) (\s@DeleteNamespaceResponse' {} a -> s {status = a} :: DeleteNamespaceResponse)

instance Prelude.NFData DeleteNamespaceResponse where
  rnf DeleteNamespaceResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
