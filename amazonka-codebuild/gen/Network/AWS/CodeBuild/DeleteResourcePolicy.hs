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
-- Module      : Network.AWS.CodeBuild.DeleteResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The ARN of the resource that is associated with the resource policy.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteResourcePolicy_resourceArn' - The ARN of the resource that is associated with the resource policy.
newDeleteResourcePolicy ::
  -- | 'resourceArn'
  Core.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pResourceArn_ =
  DeleteResourcePolicy' {resourceArn = pResourceArn_}

-- | The ARN of the resource that is associated with the resource policy.
deleteResourcePolicy_resourceArn :: Lens.Lens' DeleteResourcePolicy Core.Text
deleteResourcePolicy_resourceArn = Lens.lens (\DeleteResourcePolicy' {resourceArn} -> resourceArn) (\s@DeleteResourcePolicy' {} a -> s {resourceArn = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteResourcePolicy

instance Core.NFData DeleteResourcePolicy

instance Core.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.DeleteResourcePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceArn" Core..= resourceArn)]
      )

instance Core.ToPath DeleteResourcePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResourcePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Core.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Core.NFData DeleteResourcePolicyResponse
