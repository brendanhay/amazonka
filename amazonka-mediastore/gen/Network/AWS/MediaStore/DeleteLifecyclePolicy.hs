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
-- Module      : Network.AWS.MediaStore.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an object lifecycle policy from a container. It takes up to 20
-- minutes for the change to take effect.
module Network.AWS.MediaStore.DeleteLifecyclePolicy
  ( -- * Creating a Request
    DeleteLifecyclePolicy (..),
    newDeleteLifecyclePolicy,

    -- * Request Lenses
    deleteLifecyclePolicy_containerName,

    -- * Destructuring the Response
    DeleteLifecyclePolicyResponse (..),
    newDeleteLifecyclePolicyResponse,

    -- * Response Lenses
    deleteLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The name of the container that holds the object lifecycle policy.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'deleteLifecyclePolicy_containerName' - The name of the container that holds the object lifecycle policy.
newDeleteLifecyclePolicy ::
  -- | 'containerName'
  Core.Text ->
  DeleteLifecyclePolicy
newDeleteLifecyclePolicy pContainerName_ =
  DeleteLifecyclePolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container that holds the object lifecycle policy.
deleteLifecyclePolicy_containerName :: Lens.Lens' DeleteLifecyclePolicy Core.Text
deleteLifecyclePolicy_containerName = Lens.lens (\DeleteLifecyclePolicy' {containerName} -> containerName) (\s@DeleteLifecyclePolicy' {} a -> s {containerName = a} :: DeleteLifecyclePolicy)

instance Core.AWSRequest DeleteLifecyclePolicy where
  type
    AWSResponse DeleteLifecyclePolicy =
      DeleteLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLifecyclePolicy

instance Core.NFData DeleteLifecyclePolicy

instance Core.ToHeaders DeleteLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.DeleteLifecyclePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.ToPath DeleteLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLifecyclePolicyResponse_httpStatus' - The response's http status code.
newDeleteLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLifecyclePolicyResponse
newDeleteLifecyclePolicyResponse pHttpStatus_ =
  DeleteLifecyclePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLifecyclePolicyResponse_httpStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Core.Int
deleteLifecyclePolicyResponse_httpStatus = Lens.lens (\DeleteLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: DeleteLifecyclePolicyResponse)

instance Core.NFData DeleteLifecyclePolicyResponse
