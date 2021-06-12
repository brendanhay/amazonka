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
-- Module      : Network.AWS.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the lifecycle policy for the specified repository.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle Policy Template>.
module Network.AWS.ECR.PutLifecyclePolicy
  ( -- * Creating a Request
    PutLifecyclePolicy (..),
    newPutLifecyclePolicy,

    -- * Request Lenses
    putLifecyclePolicy_registryId,
    putLifecyclePolicy_repositoryName,
    putLifecyclePolicy_lifecyclePolicyText,

    -- * Destructuring the Response
    PutLifecyclePolicyResponse (..),
    newPutLifecyclePolicyResponse,

    -- * Response Lenses
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do  not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository to receive the policy.
    repositoryName :: Core.Text,
    -- | The JSON repository policy text to apply to the repository.
    lifecyclePolicyText :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putLifecyclePolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do  not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'putLifecyclePolicy_repositoryName' - The name of the repository to receive the policy.
--
-- 'lifecyclePolicyText', 'putLifecyclePolicy_lifecyclePolicyText' - The JSON repository policy text to apply to the repository.
newPutLifecyclePolicy ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'lifecyclePolicyText'
  Core.Text ->
  PutLifecyclePolicy
newPutLifecyclePolicy
  pRepositoryName_
  pLifecyclePolicyText_ =
    PutLifecyclePolicy'
      { registryId = Core.Nothing,
        repositoryName = pRepositoryName_,
        lifecyclePolicyText = pLifecyclePolicyText_
      }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do  not specify a registry, the default registry is
-- assumed.
putLifecyclePolicy_registryId :: Lens.Lens' PutLifecyclePolicy (Core.Maybe Core.Text)
putLifecyclePolicy_registryId = Lens.lens (\PutLifecyclePolicy' {registryId} -> registryId) (\s@PutLifecyclePolicy' {} a -> s {registryId = a} :: PutLifecyclePolicy)

-- | The name of the repository to receive the policy.
putLifecyclePolicy_repositoryName :: Lens.Lens' PutLifecyclePolicy Core.Text
putLifecyclePolicy_repositoryName = Lens.lens (\PutLifecyclePolicy' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicy' {} a -> s {repositoryName = a} :: PutLifecyclePolicy)

-- | The JSON repository policy text to apply to the repository.
putLifecyclePolicy_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Core.Text
putLifecyclePolicy_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicy' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicy' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicy)

instance Core.AWSRequest PutLifecyclePolicy where
  type
    AWSResponse PutLifecyclePolicy =
      PutLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "lifecyclePolicyText")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutLifecyclePolicy

instance Core.NFData PutLifecyclePolicy

instance Core.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("lifecyclePolicyText" Core..= lifecyclePolicyText)
          ]
      )

instance Core.ToPath PutLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'putLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'lifecyclePolicyText', 'putLifecyclePolicyResponse_lifecyclePolicyText' - The JSON repository policy text.
--
-- 'httpStatus', 'putLifecyclePolicyResponse_httpStatus' - The response's http status code.
newPutLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutLifecyclePolicyResponse
newPutLifecyclePolicyResponse pHttpStatus_ =
  PutLifecyclePolicyResponse'
    { registryId =
        Core.Nothing,
      repositoryName = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
putLifecyclePolicyResponse_registryId :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Core.Text)
putLifecyclePolicyResponse_registryId = Lens.lens (\PutLifecyclePolicyResponse' {registryId} -> registryId) (\s@PutLifecyclePolicyResponse' {} a -> s {registryId = a} :: PutLifecyclePolicyResponse)

-- | The repository name associated with the request.
putLifecyclePolicyResponse_repositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Core.Text)
putLifecyclePolicyResponse_repositoryName = Lens.lens (\PutLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: PutLifecyclePolicyResponse)

-- | The JSON repository policy text.
putLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Core.Maybe Core.Text)
putLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicyResponse)

-- | The response's http status code.
putLifecyclePolicyResponse_httpStatus :: Lens.Lens' PutLifecyclePolicyResponse Core.Int
putLifecyclePolicyResponse_httpStatus = Lens.lens (\PutLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@PutLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: PutLifecyclePolicyResponse)

instance Core.NFData PutLifecyclePolicyResponse
