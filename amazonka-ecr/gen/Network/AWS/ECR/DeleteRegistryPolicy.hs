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
-- Module      : Network.AWS.ECR.DeleteRegistryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the registry permissions policy.
module Network.AWS.ECR.DeleteRegistryPolicy
  ( -- * Creating a Request
    DeleteRegistryPolicy (..),
    newDeleteRegistryPolicy,

    -- * Destructuring the Response
    DeleteRegistryPolicyResponse (..),
    newDeleteRegistryPolicyResponse,

    -- * Response Lenses
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRegistryPolicy' smart constructor.
data DeleteRegistryPolicy = DeleteRegistryPolicy'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegistryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRegistryPolicy ::
  DeleteRegistryPolicy
newDeleteRegistryPolicy = DeleteRegistryPolicy'

instance Core.AWSRequest DeleteRegistryPolicy where
  type
    AWSResponse DeleteRegistryPolicy =
      DeleteRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegistryPolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "policyText")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRegistryPolicy

instance Core.NFData DeleteRegistryPolicy

instance Core.ToHeaders DeleteRegistryPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRegistryPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRegistryPolicy where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DeleteRegistryPolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRegistryPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRegistryPolicyResponse' smart constructor.
data DeleteRegistryPolicyResponse = DeleteRegistryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The contents of the registry permissions policy that was deleted.
    policyText :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRegistryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'policyText', 'deleteRegistryPolicyResponse_policyText' - The contents of the registry permissions policy that was deleted.
--
-- 'httpStatus', 'deleteRegistryPolicyResponse_httpStatus' - The response's http status code.
newDeleteRegistryPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRegistryPolicyResponse
newDeleteRegistryPolicyResponse pHttpStatus_ =
  DeleteRegistryPolicyResponse'
    { registryId =
        Core.Nothing,
      policyText = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
deleteRegistryPolicyResponse_registryId :: Lens.Lens' DeleteRegistryPolicyResponse (Core.Maybe Core.Text)
deleteRegistryPolicyResponse_registryId = Lens.lens (\DeleteRegistryPolicyResponse' {registryId} -> registryId) (\s@DeleteRegistryPolicyResponse' {} a -> s {registryId = a} :: DeleteRegistryPolicyResponse)

-- | The contents of the registry permissions policy that was deleted.
deleteRegistryPolicyResponse_policyText :: Lens.Lens' DeleteRegistryPolicyResponse (Core.Maybe Core.Text)
deleteRegistryPolicyResponse_policyText = Lens.lens (\DeleteRegistryPolicyResponse' {policyText} -> policyText) (\s@DeleteRegistryPolicyResponse' {} a -> s {policyText = a} :: DeleteRegistryPolicyResponse)

-- | The response's http status code.
deleteRegistryPolicyResponse_httpStatus :: Lens.Lens' DeleteRegistryPolicyResponse Core.Int
deleteRegistryPolicyResponse_httpStatus = Lens.lens (\DeleteRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteRegistryPolicyResponse' {} a -> s {httpStatus = a} :: DeleteRegistryPolicyResponse)

instance Core.NFData DeleteRegistryPolicyResponse
