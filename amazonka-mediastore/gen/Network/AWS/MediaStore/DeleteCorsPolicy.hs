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
-- Module      : Network.AWS.MediaStore.DeleteCorsPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the cross-origin resource sharing (CORS) configuration
-- information that is set for the container.
--
-- To use this operation, you must have permission to perform the
-- @MediaStore:DeleteCorsPolicy@ action. The container owner has this
-- permission by default and can grant this permission to others.
module Network.AWS.MediaStore.DeleteCorsPolicy
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCorsPolicy' smart constructor.
data DeleteCorsPolicy = DeleteCorsPolicy'
  { -- | The name of the container to remove the policy from.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCorsPolicy where
  type Rs DeleteCorsPolicy = DeleteCorsPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCorsPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCorsPolicy

instance Prelude.NFData DeleteCorsPolicy

instance Prelude.ToHeaders DeleteCorsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.DeleteCorsPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCorsPolicy where
  toJSON DeleteCorsPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Prelude..= containerName)
          ]
      )

instance Prelude.ToPath DeleteCorsPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCorsPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCorsPolicyResponse' smart constructor.
data DeleteCorsPolicyResponse = DeleteCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCorsPolicyResponse
