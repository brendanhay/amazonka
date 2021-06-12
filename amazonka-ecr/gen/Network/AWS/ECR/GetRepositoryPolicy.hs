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
-- Module      : Network.AWS.ECR.GetRepositoryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the repository policy for the specified repository.
module Network.AWS.ECR.GetRepositoryPolicy
  ( -- * Creating a Request
    GetRepositoryPolicy (..),
    newGetRepositoryPolicy,

    -- * Request Lenses
    getRepositoryPolicy_registryId,
    getRepositoryPolicy_repositoryName,

    -- * Destructuring the Response
    GetRepositoryPolicyResponse (..),
    newGetRepositoryPolicyResponse,

    -- * Response Lenses
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository with the policy to retrieve.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRepositoryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getRepositoryPolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'getRepositoryPolicy_repositoryName' - The name of the repository with the policy to retrieve.
newGetRepositoryPolicy ::
  -- | 'repositoryName'
  Core.Text ->
  GetRepositoryPolicy
newGetRepositoryPolicy pRepositoryName_ =
  GetRepositoryPolicy'
    { registryId = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
getRepositoryPolicy_registryId :: Lens.Lens' GetRepositoryPolicy (Core.Maybe Core.Text)
getRepositoryPolicy_registryId = Lens.lens (\GetRepositoryPolicy' {registryId} -> registryId) (\s@GetRepositoryPolicy' {} a -> s {registryId = a} :: GetRepositoryPolicy)

-- | The name of the repository with the policy to retrieve.
getRepositoryPolicy_repositoryName :: Lens.Lens' GetRepositoryPolicy Core.Text
getRepositoryPolicy_repositoryName = Lens.lens (\GetRepositoryPolicy' {repositoryName} -> repositoryName) (\s@GetRepositoryPolicy' {} a -> s {repositoryName = a} :: GetRepositoryPolicy)

instance Core.AWSRequest GetRepositoryPolicy where
  type
    AWSResponse GetRepositoryPolicy =
      GetRepositoryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryPolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "policyText")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRepositoryPolicy

instance Core.NFData GetRepositoryPolicy

instance Core.ToHeaders GetRepositoryPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRepositoryPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRepositoryPolicy where
  toJSON GetRepositoryPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetRepositoryPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetRepositoryPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The JSON repository policy text associated with the repository.
    policyText :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRepositoryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getRepositoryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'policyText', 'getRepositoryPolicyResponse_policyText' - The JSON repository policy text associated with the repository.
--
-- 'repositoryName', 'getRepositoryPolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'getRepositoryPolicyResponse_httpStatus' - The response's http status code.
newGetRepositoryPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRepositoryPolicyResponse
newGetRepositoryPolicyResponse pHttpStatus_ =
  GetRepositoryPolicyResponse'
    { registryId =
        Core.Nothing,
      policyText = Core.Nothing,
      repositoryName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
getRepositoryPolicyResponse_registryId :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Core.Text)
getRepositoryPolicyResponse_registryId = Lens.lens (\GetRepositoryPolicyResponse' {registryId} -> registryId) (\s@GetRepositoryPolicyResponse' {} a -> s {registryId = a} :: GetRepositoryPolicyResponse)

-- | The JSON repository policy text associated with the repository.
getRepositoryPolicyResponse_policyText :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Core.Text)
getRepositoryPolicyResponse_policyText = Lens.lens (\GetRepositoryPolicyResponse' {policyText} -> policyText) (\s@GetRepositoryPolicyResponse' {} a -> s {policyText = a} :: GetRepositoryPolicyResponse)

-- | The repository name associated with the request.
getRepositoryPolicyResponse_repositoryName :: Lens.Lens' GetRepositoryPolicyResponse (Core.Maybe Core.Text)
getRepositoryPolicyResponse_repositoryName = Lens.lens (\GetRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@GetRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: GetRepositoryPolicyResponse)

-- | The response's http status code.
getRepositoryPolicyResponse_httpStatus :: Lens.Lens' GetRepositoryPolicyResponse Core.Int
getRepositoryPolicyResponse_httpStatus = Lens.lens (\GetRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: GetRepositoryPolicyResponse)

instance Core.NFData GetRepositoryPolicyResponse
