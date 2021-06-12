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
-- Module      : Network.AWS.ECR.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle policy associated with the specified repository.
module Network.AWS.ECR.DeleteLifecyclePolicy
  ( -- * Creating a Request
    DeleteLifecyclePolicy (..),
    newDeleteLifecyclePolicy,

    -- * Request Lenses
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,

    -- * Destructuring the Response
    DeleteLifecyclePolicyResponse (..),
    newDeleteLifecyclePolicyResponse,

    -- * Response Lenses
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository.
    repositoryName :: Core.Text
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
-- 'registryId', 'deleteLifecyclePolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'deleteLifecyclePolicy_repositoryName' - The name of the repository.
newDeleteLifecyclePolicy ::
  -- | 'repositoryName'
  Core.Text ->
  DeleteLifecyclePolicy
newDeleteLifecyclePolicy pRepositoryName_ =
  DeleteLifecyclePolicy'
    { registryId = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
deleteLifecyclePolicy_registryId :: Lens.Lens' DeleteLifecyclePolicy (Core.Maybe Core.Text)
deleteLifecyclePolicy_registryId = Lens.lens (\DeleteLifecyclePolicy' {registryId} -> registryId) (\s@DeleteLifecyclePolicy' {} a -> s {registryId = a} :: DeleteLifecyclePolicy)

-- | The name of the repository.
deleteLifecyclePolicy_repositoryName :: Lens.Lens' DeleteLifecyclePolicy Core.Text
deleteLifecyclePolicy_repositoryName = Lens.lens (\DeleteLifecyclePolicy' {repositoryName} -> repositoryName) (\s@DeleteLifecyclePolicy' {} a -> s {repositoryName = a} :: DeleteLifecyclePolicy)

instance Core.AWSRequest DeleteLifecyclePolicy where
  type
    AWSResponse DeleteLifecyclePolicy =
      DeleteLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "lifecyclePolicyText")
            Core.<*> (x Core..?> "lastEvaluatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLifecyclePolicy

instance Core.NFData DeleteLifecyclePolicy

instance Core.ToHeaders DeleteLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy" ::
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
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath DeleteLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Core.Maybe Core.Text,
    -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
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
-- 'registryId', 'deleteLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'deleteLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'lifecyclePolicyText', 'deleteLifecyclePolicyResponse_lifecyclePolicyText' - The JSON lifecycle policy text.
--
-- 'lastEvaluatedAt', 'deleteLifecyclePolicyResponse_lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- 'httpStatus', 'deleteLifecyclePolicyResponse_httpStatus' - The response's http status code.
newDeleteLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLifecyclePolicyResponse
newDeleteLifecyclePolicyResponse pHttpStatus_ =
  DeleteLifecyclePolicyResponse'
    { registryId =
        Core.Nothing,
      repositoryName = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      lastEvaluatedAt = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
deleteLifecyclePolicyResponse_registryId :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.Text)
deleteLifecyclePolicyResponse_registryId = Lens.lens (\DeleteLifecyclePolicyResponse' {registryId} -> registryId) (\s@DeleteLifecyclePolicyResponse' {} a -> s {registryId = a} :: DeleteLifecyclePolicyResponse)

-- | The repository name associated with the request.
deleteLifecyclePolicyResponse_repositoryName :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.Text)
deleteLifecyclePolicyResponse_repositoryName = Lens.lens (\DeleteLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@DeleteLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: DeleteLifecyclePolicyResponse)

-- | The JSON lifecycle policy text.
deleteLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.Text)
deleteLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\DeleteLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@DeleteLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: DeleteLifecyclePolicyResponse)

-- | The time stamp of the last time that the lifecycle policy was run.
deleteLifecyclePolicyResponse_lastEvaluatedAt :: Lens.Lens' DeleteLifecyclePolicyResponse (Core.Maybe Core.UTCTime)
deleteLifecyclePolicyResponse_lastEvaluatedAt = Lens.lens (\DeleteLifecyclePolicyResponse' {lastEvaluatedAt} -> lastEvaluatedAt) (\s@DeleteLifecyclePolicyResponse' {} a -> s {lastEvaluatedAt = a} :: DeleteLifecyclePolicyResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
deleteLifecyclePolicyResponse_httpStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Core.Int
deleteLifecyclePolicyResponse_httpStatus = Lens.lens (\DeleteLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: DeleteLifecyclePolicyResponse)

instance Core.NFData DeleteLifecyclePolicyResponse
