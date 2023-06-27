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
-- Module      : Amazonka.RedshiftServerLess.DeleteNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from Amazon Redshift Serverless. Before you delete
-- the namespace, you can create a final snapshot that has all of the data
-- within the namespace.
module Amazonka.RedshiftServerLess.DeleteNamespace
  ( -- * Creating a Request
    DeleteNamespace (..),
    newDeleteNamespace,

    -- * Request Lenses
    deleteNamespace_finalSnapshotName,
    deleteNamespace_finalSnapshotRetentionPeriod,
    deleteNamespace_namespaceName,

    -- * Destructuring the Response
    DeleteNamespaceResponse (..),
    newDeleteNamespaceResponse,

    -- * Response Lenses
    deleteNamespaceResponse_httpStatus,
    deleteNamespaceResponse_namespace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNamespace' smart constructor.
data DeleteNamespace = DeleteNamespace'
  { -- | The name of the snapshot to be created before the namespace is deleted.
    finalSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | How long to retain the final snapshot.
    finalSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the namespace to delete.
    namespaceName :: Prelude.Text
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
-- 'finalSnapshotName', 'deleteNamespace_finalSnapshotName' - The name of the snapshot to be created before the namespace is deleted.
--
-- 'finalSnapshotRetentionPeriod', 'deleteNamespace_finalSnapshotRetentionPeriod' - How long to retain the final snapshot.
--
-- 'namespaceName', 'deleteNamespace_namespaceName' - The name of the namespace to delete.
newDeleteNamespace ::
  -- | 'namespaceName'
  Prelude.Text ->
  DeleteNamespace
newDeleteNamespace pNamespaceName_ =
  DeleteNamespace'
    { finalSnapshotName =
        Prelude.Nothing,
      finalSnapshotRetentionPeriod = Prelude.Nothing,
      namespaceName = pNamespaceName_
    }

-- | The name of the snapshot to be created before the namespace is deleted.
deleteNamespace_finalSnapshotName :: Lens.Lens' DeleteNamespace (Prelude.Maybe Prelude.Text)
deleteNamespace_finalSnapshotName = Lens.lens (\DeleteNamespace' {finalSnapshotName} -> finalSnapshotName) (\s@DeleteNamespace' {} a -> s {finalSnapshotName = a} :: DeleteNamespace)

-- | How long to retain the final snapshot.
deleteNamespace_finalSnapshotRetentionPeriod :: Lens.Lens' DeleteNamespace (Prelude.Maybe Prelude.Int)
deleteNamespace_finalSnapshotRetentionPeriod = Lens.lens (\DeleteNamespace' {finalSnapshotRetentionPeriod} -> finalSnapshotRetentionPeriod) (\s@DeleteNamespace' {} a -> s {finalSnapshotRetentionPeriod = a} :: DeleteNamespace)

-- | The name of the namespace to delete.
deleteNamespace_namespaceName :: Lens.Lens' DeleteNamespace Prelude.Text
deleteNamespace_namespaceName = Lens.lens (\DeleteNamespace' {namespaceName} -> namespaceName) (\s@DeleteNamespace' {} a -> s {namespaceName = a} :: DeleteNamespace)

instance Core.AWSRequest DeleteNamespace where
  type
    AWSResponse DeleteNamespace =
      DeleteNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "namespace")
      )

instance Prelude.Hashable DeleteNamespace where
  hashWithSalt _salt DeleteNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` finalSnapshotName
      `Prelude.hashWithSalt` finalSnapshotRetentionPeriod
      `Prelude.hashWithSalt` namespaceName

instance Prelude.NFData DeleteNamespace where
  rnf DeleteNamespace' {..} =
    Prelude.rnf finalSnapshotName
      `Prelude.seq` Prelude.rnf finalSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf namespaceName

instance Data.ToHeaders DeleteNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.DeleteNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNamespace where
  toJSON DeleteNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("finalSnapshotName" Data..=)
              Prelude.<$> finalSnapshotName,
            ("finalSnapshotRetentionPeriod" Data..=)
              Prelude.<$> finalSnapshotRetentionPeriod,
            Prelude.Just
              ("namespaceName" Data..= namespaceName)
          ]
      )

instance Data.ToPath DeleteNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The deleted namespace object.
    namespace :: Namespace
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNamespaceResponse_httpStatus' - The response's http status code.
--
-- 'namespace', 'deleteNamespaceResponse_namespace' - The deleted namespace object.
newDeleteNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'namespace'
  Namespace ->
  DeleteNamespaceResponse
newDeleteNamespaceResponse pHttpStatus_ pNamespace_ =
  DeleteNamespaceResponse'
    { httpStatus = pHttpStatus_,
      namespace = pNamespace_
    }

-- | The response's http status code.
deleteNamespaceResponse_httpStatus :: Lens.Lens' DeleteNamespaceResponse Prelude.Int
deleteNamespaceResponse_httpStatus = Lens.lens (\DeleteNamespaceResponse' {httpStatus} -> httpStatus) (\s@DeleteNamespaceResponse' {} a -> s {httpStatus = a} :: DeleteNamespaceResponse)

-- | The deleted namespace object.
deleteNamespaceResponse_namespace :: Lens.Lens' DeleteNamespaceResponse Namespace
deleteNamespaceResponse_namespace = Lens.lens (\DeleteNamespaceResponse' {namespace} -> namespace) (\s@DeleteNamespaceResponse' {} a -> s {namespace = a} :: DeleteNamespaceResponse)

instance Prelude.NFData DeleteNamespaceResponse where
  rnf DeleteNamespaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespace
