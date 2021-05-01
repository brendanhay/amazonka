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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | If the policy you are attempting to set on a repository policy would
    -- prevent you from setting another policy in the future, you must force
    -- the SetRepositoryPolicy operation. This is intended to prevent
    -- accidental repository lock outs.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The name of the repository to receive the policy.
    repositoryName :: Prelude.Text,
    -- | The JSON repository policy text to apply to the repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'policyText'
  Prelude.Text ->
  SetRepositoryPolicy
newSetRepositoryPolicy pRepositoryName_ pPolicyText_ =
  SetRepositoryPolicy'
    { registryId = Prelude.Nothing,
      force = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      policyText = pPolicyText_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
setRepositoryPolicy_registryId :: Lens.Lens' SetRepositoryPolicy (Prelude.Maybe Prelude.Text)
setRepositoryPolicy_registryId = Lens.lens (\SetRepositoryPolicy' {registryId} -> registryId) (\s@SetRepositoryPolicy' {} a -> s {registryId = a} :: SetRepositoryPolicy)

-- | If the policy you are attempting to set on a repository policy would
-- prevent you from setting another policy in the future, you must force
-- the SetRepositoryPolicy operation. This is intended to prevent
-- accidental repository lock outs.
setRepositoryPolicy_force :: Lens.Lens' SetRepositoryPolicy (Prelude.Maybe Prelude.Bool)
setRepositoryPolicy_force = Lens.lens (\SetRepositoryPolicy' {force} -> force) (\s@SetRepositoryPolicy' {} a -> s {force = a} :: SetRepositoryPolicy)

-- | The name of the repository to receive the policy.
setRepositoryPolicy_repositoryName :: Lens.Lens' SetRepositoryPolicy Prelude.Text
setRepositoryPolicy_repositoryName = Lens.lens (\SetRepositoryPolicy' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicy' {} a -> s {repositoryName = a} :: SetRepositoryPolicy)

-- | The JSON repository policy text to apply to the repository. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR Repository Policies>
-- in the /Amazon Elastic Container Registry User Guide/.
setRepositoryPolicy_policyText :: Lens.Lens' SetRepositoryPolicy Prelude.Text
setRepositoryPolicy_policyText = Lens.lens (\SetRepositoryPolicy' {policyText} -> policyText) (\s@SetRepositoryPolicy' {} a -> s {policyText = a} :: SetRepositoryPolicy)

instance Prelude.AWSRequest SetRepositoryPolicy where
  type
    Rs SetRepositoryPolicy =
      SetRepositoryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRepositoryPolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "policyText")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetRepositoryPolicy

instance Prelude.NFData SetRepositoryPolicy

instance Prelude.ToHeaders SetRepositoryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetRepositoryPolicy where
  toJSON SetRepositoryPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            ("force" Prelude..=) Prelude.<$> force,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("policyText" Prelude..= policyText)
          ]
      )

instance Prelude.ToPath SetRepositoryPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetRepositoryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON repository policy text applied to the repository.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  SetRepositoryPolicyResponse
newSetRepositoryPolicyResponse pHttpStatus_ =
  SetRepositoryPolicyResponse'
    { registryId =
        Prelude.Nothing,
      policyText = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
setRepositoryPolicyResponse_registryId :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_registryId = Lens.lens (\SetRepositoryPolicyResponse' {registryId} -> registryId) (\s@SetRepositoryPolicyResponse' {} a -> s {registryId = a} :: SetRepositoryPolicyResponse)

-- | The JSON repository policy text applied to the repository.
setRepositoryPolicyResponse_policyText :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_policyText = Lens.lens (\SetRepositoryPolicyResponse' {policyText} -> policyText) (\s@SetRepositoryPolicyResponse' {} a -> s {policyText = a} :: SetRepositoryPolicyResponse)

-- | The repository name associated with the request.
setRepositoryPolicyResponse_repositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_repositoryName = Lens.lens (\SetRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: SetRepositoryPolicyResponse)

-- | The response's http status code.
setRepositoryPolicyResponse_httpStatus :: Lens.Lens' SetRepositoryPolicyResponse Prelude.Int
setRepositoryPolicyResponse_httpStatus = Lens.lens (\SetRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@SetRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: SetRepositoryPolicyResponse)

instance Prelude.NFData SetRepositoryPolicyResponse
