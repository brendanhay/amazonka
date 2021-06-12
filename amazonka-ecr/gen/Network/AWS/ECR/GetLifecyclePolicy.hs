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
-- Module      : Network.AWS.ECR.GetLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the lifecycle policy for the specified repository.
module Network.AWS.ECR.GetLifecyclePolicy
  ( -- * Creating a Request
    GetLifecyclePolicy (..),
    newGetLifecyclePolicy,

    -- * Request Lenses
    getLifecyclePolicy_registryId,
    getLifecyclePolicy_repositoryName,

    -- * Destructuring the Response
    GetLifecyclePolicyResponse (..),
    newGetLifecyclePolicyResponse,

    -- * Response Lenses
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_lastEvaluatedAt,
    getLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getLifecyclePolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'getLifecyclePolicy_repositoryName' - The name of the repository.
newGetLifecyclePolicy ::
  -- | 'repositoryName'
  Core.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pRepositoryName_ =
  GetLifecyclePolicy'
    { registryId = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
getLifecyclePolicy_registryId :: Lens.Lens' GetLifecyclePolicy (Core.Maybe Core.Text)
getLifecyclePolicy_registryId = Lens.lens (\GetLifecyclePolicy' {registryId} -> registryId) (\s@GetLifecyclePolicy' {} a -> s {registryId = a} :: GetLifecyclePolicy)

-- | The name of the repository.
getLifecyclePolicy_repositoryName :: Lens.Lens' GetLifecyclePolicy Core.Text
getLifecyclePolicy_repositoryName = Lens.lens (\GetLifecyclePolicy' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicy' {} a -> s {repositoryName = a} :: GetLifecyclePolicy)

instance Core.AWSRequest GetLifecyclePolicy where
  type
    AWSResponse GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "lifecyclePolicyText")
            Core.<*> (x Core..?> "lastEvaluatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLifecyclePolicy

instance Core.NFData GetLifecyclePolicy

instance Core.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
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
-- Create a value of 'GetLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'getLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'lifecyclePolicyText', 'getLifecyclePolicyResponse_lifecyclePolicyText' - The JSON lifecycle policy text.
--
-- 'lastEvaluatedAt', 'getLifecyclePolicyResponse_lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- 'httpStatus', 'getLifecyclePolicyResponse_httpStatus' - The response's http status code.
newGetLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse pHttpStatus_ =
  GetLifecyclePolicyResponse'
    { registryId =
        Core.Nothing,
      repositoryName = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      lastEvaluatedAt = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
getLifecyclePolicyResponse_registryId :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.Text)
getLifecyclePolicyResponse_registryId = Lens.lens (\GetLifecyclePolicyResponse' {registryId} -> registryId) (\s@GetLifecyclePolicyResponse' {} a -> s {registryId = a} :: GetLifecyclePolicyResponse)

-- | The repository name associated with the request.
getLifecyclePolicyResponse_repositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.Text)
getLifecyclePolicyResponse_repositoryName = Lens.lens (\GetLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: GetLifecyclePolicyResponse)

-- | The JSON lifecycle policy text.
getLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.Text)
getLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\GetLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@GetLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyResponse)

-- | The time stamp of the last time that the lifecycle policy was run.
getLifecyclePolicyResponse_lastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Core.Maybe Core.UTCTime)
getLifecyclePolicyResponse_lastEvaluatedAt = Lens.lens (\GetLifecyclePolicyResponse' {lastEvaluatedAt} -> lastEvaluatedAt) (\s@GetLifecyclePolicyResponse' {} a -> s {lastEvaluatedAt = a} :: GetLifecyclePolicyResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

instance Core.NFData GetLifecyclePolicyResponse
