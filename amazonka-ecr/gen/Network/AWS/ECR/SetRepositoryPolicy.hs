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
-- Module      : Network.AWS.ECR.SetRepositoryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a repository policy to the specified repository to control
-- access permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policies.html Amazon ECR Repository Policies>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.SetRepositoryPolicy
  ( -- * Creating a Request
    SetRepositoryPolicy (..),
    newSetRepositoryPolicy,

    -- * Request Lenses
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_force,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,

    -- * Destructuring the Response
    SetRepositoryPolicyResponse (..),
    newSetRepositoryPolicyResponse,

    -- * Response Lenses
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | If the policy you are attempting to set on a repository policy would
    -- prevent you from setting another policy in the future, you must force
    -- the SetRepositoryPolicy operation. This is intended to prevent
    -- accidental repository lock outs.
    force :: Core.Maybe Core.Bool,
    -- | The name of the repository to receive the policy.
    repositoryName :: Core.Text,
    -- | The JSON repository policy text to apply to the repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetRepositoryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'setRepositoryPolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'force', 'setRepositoryPolicy_force' - If the policy you are attempting to set on a repository policy would
-- prevent you from setting another policy in the future, you must force
-- the SetRepositoryPolicy operation. This is intended to prevent
-- accidental repository lock outs.
--
-- 'repositoryName', 'setRepositoryPolicy_repositoryName' - The name of the repository to receive the policy.
--
-- 'policyText', 'setRepositoryPolicy_policyText' - The JSON repository policy text to apply to the repository. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies>
-- in the /Amazon Elastic Container Registry User Guide/.
newSetRepositoryPolicy ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'policyText'
  Core.Text ->
  SetRepositoryPolicy
newSetRepositoryPolicy pRepositoryName_ pPolicyText_ =
  SetRepositoryPolicy'
    { registryId = Core.Nothing,
      force = Core.Nothing,
      repositoryName = pRepositoryName_,
      policyText = pPolicyText_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
setRepositoryPolicy_registryId :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Core.Text)
setRepositoryPolicy_registryId = Lens.lens (\SetRepositoryPolicy' {registryId} -> registryId) (\s@SetRepositoryPolicy' {} a -> s {registryId = a} :: SetRepositoryPolicy)

-- | If the policy you are attempting to set on a repository policy would
-- prevent you from setting another policy in the future, you must force
-- the SetRepositoryPolicy operation. This is intended to prevent
-- accidental repository lock outs.
setRepositoryPolicy_force :: Lens.Lens' SetRepositoryPolicy (Core.Maybe Core.Bool)
setRepositoryPolicy_force = Lens.lens (\SetRepositoryPolicy' {force} -> force) (\s@SetRepositoryPolicy' {} a -> s {force = a} :: SetRepositoryPolicy)

-- | The name of the repository to receive the policy.
setRepositoryPolicy_repositoryName :: Lens.Lens' SetRepositoryPolicy Core.Text
setRepositoryPolicy_repositoryName = Lens.lens (\SetRepositoryPolicy' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicy' {} a -> s {repositoryName = a} :: SetRepositoryPolicy)

-- | The JSON repository policy text to apply to the repository. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies>
-- in the /Amazon Elastic Container Registry User Guide/.
setRepositoryPolicy_policyText :: Lens.Lens' SetRepositoryPolicy Core.Text
setRepositoryPolicy_policyText = Lens.lens (\SetRepositoryPolicy' {policyText} -> policyText) (\s@SetRepositoryPolicy' {} a -> s {policyText = a} :: SetRepositoryPolicy)

instance Core.AWSRequest SetRepositoryPolicy where
  type
    AWSResponse SetRepositoryPolicy =
      SetRepositoryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRepositoryPolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "policyText")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetRepositoryPolicy

instance Core.NFData SetRepositoryPolicy

instance Core.ToHeaders SetRepositoryPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetRepositoryPolicy where
  toJSON SetRepositoryPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            ("force" Core..=) Core.<$> force,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("policyText" Core..= policyText)
          ]
      )

instance Core.ToPath SetRepositoryPolicy where
  toPath = Core.const "/"

instance Core.ToQuery SetRepositoryPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The JSON repository policy text applied to the repository.
    policyText :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetRepositoryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'setRepositoryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'policyText', 'setRepositoryPolicyResponse_policyText' - The JSON repository policy text applied to the repository.
--
-- 'repositoryName', 'setRepositoryPolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'setRepositoryPolicyResponse_httpStatus' - The response's http status code.
newSetRepositoryPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetRepositoryPolicyResponse
newSetRepositoryPolicyResponse pHttpStatus_ =
  SetRepositoryPolicyResponse'
    { registryId =
        Core.Nothing,
      policyText = Core.Nothing,
      repositoryName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
setRepositoryPolicyResponse_registryId :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Core.Text)
setRepositoryPolicyResponse_registryId = Lens.lens (\SetRepositoryPolicyResponse' {registryId} -> registryId) (\s@SetRepositoryPolicyResponse' {} a -> s {registryId = a} :: SetRepositoryPolicyResponse)

-- | The JSON repository policy text applied to the repository.
setRepositoryPolicyResponse_policyText :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Core.Text)
setRepositoryPolicyResponse_policyText = Lens.lens (\SetRepositoryPolicyResponse' {policyText} -> policyText) (\s@SetRepositoryPolicyResponse' {} a -> s {policyText = a} :: SetRepositoryPolicyResponse)

-- | The repository name associated with the request.
setRepositoryPolicyResponse_repositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Core.Maybe Core.Text)
setRepositoryPolicyResponse_repositoryName = Lens.lens (\SetRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: SetRepositoryPolicyResponse)

-- | The response's http status code.
setRepositoryPolicyResponse_httpStatus :: Lens.Lens' SetRepositoryPolicyResponse Core.Int
setRepositoryPolicyResponse_httpStatus = Lens.lens (\SetRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@SetRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: SetRepositoryPolicyResponse)

instance Core.NFData SetRepositoryPolicyResponse
