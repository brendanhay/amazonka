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
-- Module      : Amazonka.MediaStore.DeleteContainerPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access policy that is associated with the specified
-- container.
module Amazonka.MediaStore.DeleteContainerPolicy
  ( -- * Creating a Request
    DeleteContainerPolicy (..),
    newDeleteContainerPolicy,

    -- * Request Lenses
    deleteContainerPolicy_containerName,

    -- * Destructuring the Response
    DeleteContainerPolicyResponse (..),
    newDeleteContainerPolicyResponse,

    -- * Response Lenses
    deleteContainerPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContainerPolicy' smart constructor.
data DeleteContainerPolicy = DeleteContainerPolicy'
  { -- | The name of the container that holds the policy.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'deleteContainerPolicy_containerName' - The name of the container that holds the policy.
newDeleteContainerPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  DeleteContainerPolicy
newDeleteContainerPolicy pContainerName_ =
  DeleteContainerPolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container that holds the policy.
deleteContainerPolicy_containerName :: Lens.Lens' DeleteContainerPolicy Prelude.Text
deleteContainerPolicy_containerName = Lens.lens (\DeleteContainerPolicy' {containerName} -> containerName) (\s@DeleteContainerPolicy' {} a -> s {containerName = a} :: DeleteContainerPolicy)

instance Core.AWSRequest DeleteContainerPolicy where
  type
    AWSResponse DeleteContainerPolicy =
      DeleteContainerPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContainerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContainerPolicy where
  hashWithSalt _salt DeleteContainerPolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData DeleteContainerPolicy where
  rnf DeleteContainerPolicy' {..} =
    Prelude.rnf containerName

instance Data.ToHeaders DeleteContainerPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.DeleteContainerPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteContainerPolicy where
  toJSON DeleteContainerPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath DeleteContainerPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteContainerPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContainerPolicyResponse' smart constructor.
data DeleteContainerPolicyResponse = DeleteContainerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContainerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContainerPolicyResponse_httpStatus' - The response's http status code.
newDeleteContainerPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContainerPolicyResponse
newDeleteContainerPolicyResponse pHttpStatus_ =
  DeleteContainerPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContainerPolicyResponse_httpStatus :: Lens.Lens' DeleteContainerPolicyResponse Prelude.Int
deleteContainerPolicyResponse_httpStatus = Lens.lens (\DeleteContainerPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteContainerPolicyResponse' {} a -> s {httpStatus = a} :: DeleteContainerPolicyResponse)

instance Prelude.NFData DeleteContainerPolicyResponse where
  rnf DeleteContainerPolicyResponse' {..} =
    Prelude.rnf httpStatus
