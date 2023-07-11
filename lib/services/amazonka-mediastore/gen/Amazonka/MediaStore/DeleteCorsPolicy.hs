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
-- Module      : Amazonka.MediaStore.DeleteCorsPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cross-origin resource sharing (CORS) configuration
-- information that is set for the container.
--
-- To use this operation, you must have permission to perform the
-- @MediaStore:DeleteCorsPolicy@ action. The container owner has this
-- permission by default and can grant this permission to others.
module Amazonka.MediaStore.DeleteCorsPolicy
  ( -- * Creating a Request
    DeleteCorsPolicy (..),
    newDeleteCorsPolicy,

    -- * Request Lenses
    deleteCorsPolicy_containerName,

    -- * Destructuring the Response
    DeleteCorsPolicyResponse (..),
    newDeleteCorsPolicyResponse,

    -- * Response Lenses
    deleteCorsPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCorsPolicy' smart constructor.
data DeleteCorsPolicy = DeleteCorsPolicy'
  { -- | The name of the container to remove the policy from.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCorsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'deleteCorsPolicy_containerName' - The name of the container to remove the policy from.
newDeleteCorsPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  DeleteCorsPolicy
newDeleteCorsPolicy pContainerName_ =
  DeleteCorsPolicy' {containerName = pContainerName_}

-- | The name of the container to remove the policy from.
deleteCorsPolicy_containerName :: Lens.Lens' DeleteCorsPolicy Prelude.Text
deleteCorsPolicy_containerName = Lens.lens (\DeleteCorsPolicy' {containerName} -> containerName) (\s@DeleteCorsPolicy' {} a -> s {containerName = a} :: DeleteCorsPolicy)

instance Core.AWSRequest DeleteCorsPolicy where
  type
    AWSResponse DeleteCorsPolicy =
      DeleteCorsPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCorsPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCorsPolicy where
  hashWithSalt _salt DeleteCorsPolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData DeleteCorsPolicy where
  rnf DeleteCorsPolicy' {..} = Prelude.rnf containerName

instance Data.ToHeaders DeleteCorsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.DeleteCorsPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCorsPolicy where
  toJSON DeleteCorsPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath DeleteCorsPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCorsPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCorsPolicyResponse' smart constructor.
data DeleteCorsPolicyResponse = DeleteCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCorsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCorsPolicyResponse_httpStatus' - The response's http status code.
newDeleteCorsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCorsPolicyResponse
newDeleteCorsPolicyResponse pHttpStatus_ =
  DeleteCorsPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCorsPolicyResponse_httpStatus :: Lens.Lens' DeleteCorsPolicyResponse Prelude.Int
deleteCorsPolicyResponse_httpStatus = Lens.lens (\DeleteCorsPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteCorsPolicyResponse' {} a -> s {httpStatus = a} :: DeleteCorsPolicyResponse)

instance Prelude.NFData DeleteCorsPolicyResponse where
  rnf DeleteCorsPolicyResponse' {..} =
    Prelude.rnf httpStatus
