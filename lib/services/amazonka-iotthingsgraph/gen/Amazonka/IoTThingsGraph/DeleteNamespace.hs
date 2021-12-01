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
-- Module      : Amazonka.IoTThingsGraph.DeleteNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified namespace. This action deletes all of the entities
-- in the namespace. Delete the systems and flows that use entities in the
-- namespace before performing this action.
module Amazonka.IoTThingsGraph.DeleteNamespace
  ( -- * Creating a Request
    DeleteNamespace (..),
    newDeleteNamespace,

    -- * Destructuring the Response
    DeleteNamespaceResponse (..),
    newDeleteNamespaceResponse,

    -- * Response Lenses
    deleteNamespaceResponse_namespaceArn,
    deleteNamespaceResponse_namespaceName,
    deleteNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNamespace' smart constructor.
data DeleteNamespace = DeleteNamespace'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNamespace ::
  DeleteNamespace
newDeleteNamespace = DeleteNamespace'

instance Core.AWSRequest DeleteNamespace where
  type
    AWSResponse DeleteNamespace =
      DeleteNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNamespaceResponse'
            Prelude.<$> (x Core..?> "namespaceArn")
            Prelude.<*> (x Core..?> "namespaceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNamespace where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance Prelude.NFData DeleteNamespace where
  rnf _ = ()

instance Core.ToHeaders DeleteNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeleteNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteNamespace where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DeleteNamespace where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { -- | The ARN of the namespace to be deleted.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace to be deleted.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'namespaceArn', 'deleteNamespaceResponse_namespaceArn' - The ARN of the namespace to be deleted.
--
-- 'namespaceName', 'deleteNamespaceResponse_namespaceName' - The name of the namespace to be deleted.
--
-- 'httpStatus', 'deleteNamespaceResponse_httpStatus' - The response's http status code.
newDeleteNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNamespaceResponse
newDeleteNamespaceResponse pHttpStatus_ =
  DeleteNamespaceResponse'
    { namespaceArn =
        Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the namespace to be deleted.
deleteNamespaceResponse_namespaceArn :: Lens.Lens' DeleteNamespaceResponse (Prelude.Maybe Prelude.Text)
deleteNamespaceResponse_namespaceArn = Lens.lens (\DeleteNamespaceResponse' {namespaceArn} -> namespaceArn) (\s@DeleteNamespaceResponse' {} a -> s {namespaceArn = a} :: DeleteNamespaceResponse)

-- | The name of the namespace to be deleted.
deleteNamespaceResponse_namespaceName :: Lens.Lens' DeleteNamespaceResponse (Prelude.Maybe Prelude.Text)
deleteNamespaceResponse_namespaceName = Lens.lens (\DeleteNamespaceResponse' {namespaceName} -> namespaceName) (\s@DeleteNamespaceResponse' {} a -> s {namespaceName = a} :: DeleteNamespaceResponse)

-- | The response's http status code.
deleteNamespaceResponse_httpStatus :: Lens.Lens' DeleteNamespaceResponse Prelude.Int
deleteNamespaceResponse_httpStatus = Lens.lens (\DeleteNamespaceResponse' {httpStatus} -> httpStatus) (\s@DeleteNamespaceResponse' {} a -> s {httpStatus = a} :: DeleteNamespaceResponse)

instance Prelude.NFData DeleteNamespaceResponse where
  rnf DeleteNamespaceResponse' {..} =
    Prelude.rnf namespaceArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespaceName
