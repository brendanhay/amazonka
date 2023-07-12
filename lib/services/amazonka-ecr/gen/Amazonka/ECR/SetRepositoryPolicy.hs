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
-- Module      : Amazonka.ECR.SetRepositoryPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a repository policy to the specified repository to control
-- access permissions. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policies.html Amazon ECR Repository policies>
-- in the /Amazon Elastic Container Registry User Guide/.
module Amazonka.ECR.SetRepositoryPolicy
  ( -- * Creating a Request
    SetRepositoryPolicy (..),
    newSetRepositoryPolicy,

    -- * Request Lenses
    setRepositoryPolicy_force,
    setRepositoryPolicy_registryId,
    setRepositoryPolicy_repositoryName,
    setRepositoryPolicy_policyText,

    -- * Destructuring the Response
    SetRepositoryPolicyResponse (..),
    newSetRepositoryPolicyResponse,

    -- * Response Lenses
    setRepositoryPolicyResponse_policyText,
    setRepositoryPolicyResponse_registryId,
    setRepositoryPolicyResponse_repositoryName,
    setRepositoryPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetRepositoryPolicy' smart constructor.
data SetRepositoryPolicy = SetRepositoryPolicy'
  { -- | If the policy you are attempting to set on a repository policy would
    -- prevent you from setting another policy in the future, you must force
    -- the SetRepositoryPolicy operation. This is intended to prevent
    -- accidental repository lock outs.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to receive the policy.
    repositoryName :: Prelude.Text,
    -- | The JSON repository policy text to apply to the repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR repository policies>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetRepositoryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'setRepositoryPolicy_force' - If the policy you are attempting to set on a repository policy would
-- prevent you from setting another policy in the future, you must force
-- the SetRepositoryPolicy operation. This is intended to prevent
-- accidental repository lock outs.
--
-- 'registryId', 'setRepositoryPolicy_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'setRepositoryPolicy_repositoryName' - The name of the repository to receive the policy.
--
-- 'policyText', 'setRepositoryPolicy_policyText' - The JSON repository policy text to apply to the repository. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR repository policies>
-- in the /Amazon Elastic Container Registry User Guide/.
newSetRepositoryPolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'policyText'
  Prelude.Text ->
  SetRepositoryPolicy
newSetRepositoryPolicy pRepositoryName_ pPolicyText_ =
  SetRepositoryPolicy'
    { force = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      policyText = pPolicyText_
    }

-- | If the policy you are attempting to set on a repository policy would
-- prevent you from setting another policy in the future, you must force
-- the SetRepositoryPolicy operation. This is intended to prevent
-- accidental repository lock outs.
setRepositoryPolicy_force :: Lens.Lens' SetRepositoryPolicy (Prelude.Maybe Prelude.Bool)
setRepositoryPolicy_force = Lens.lens (\SetRepositoryPolicy' {force} -> force) (\s@SetRepositoryPolicy' {} a -> s {force = a} :: SetRepositoryPolicy)

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
setRepositoryPolicy_registryId :: Lens.Lens' SetRepositoryPolicy (Prelude.Maybe Prelude.Text)
setRepositoryPolicy_registryId = Lens.lens (\SetRepositoryPolicy' {registryId} -> registryId) (\s@SetRepositoryPolicy' {} a -> s {registryId = a} :: SetRepositoryPolicy)

-- | The name of the repository to receive the policy.
setRepositoryPolicy_repositoryName :: Lens.Lens' SetRepositoryPolicy Prelude.Text
setRepositoryPolicy_repositoryName = Lens.lens (\SetRepositoryPolicy' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicy' {} a -> s {repositoryName = a} :: SetRepositoryPolicy)

-- | The JSON repository policy text to apply to the repository. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/repository-policy-examples.html Amazon ECR repository policies>
-- in the /Amazon Elastic Container Registry User Guide/.
setRepositoryPolicy_policyText :: Lens.Lens' SetRepositoryPolicy Prelude.Text
setRepositoryPolicy_policyText = Lens.lens (\SetRepositoryPolicy' {policyText} -> policyText) (\s@SetRepositoryPolicy' {} a -> s {policyText = a} :: SetRepositoryPolicy)

instance Core.AWSRequest SetRepositoryPolicy where
  type
    AWSResponse SetRepositoryPolicy =
      SetRepositoryPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetRepositoryPolicyResponse'
            Prelude.<$> (x Data..?> "policyText")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetRepositoryPolicy where
  hashWithSalt _salt SetRepositoryPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` policyText

instance Prelude.NFData SetRepositoryPolicy where
  rnf SetRepositoryPolicy' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf policyText

instance Data.ToHeaders SetRepositoryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.SetRepositoryPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetRepositoryPolicy where
  toJSON SetRepositoryPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("force" Data..=) Prelude.<$> force,
            ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just ("policyText" Data..= policyText)
          ]
      )

instance Data.ToPath SetRepositoryPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery SetRepositoryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetRepositoryPolicyResponse' smart constructor.
data SetRepositoryPolicyResponse = SetRepositoryPolicyResponse'
  { -- | The JSON repository policy text applied to the repository.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetRepositoryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'setRepositoryPolicyResponse_policyText' - The JSON repository policy text applied to the repository.
--
-- 'registryId', 'setRepositoryPolicyResponse_registryId' - The registry ID associated with the request.
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
    { policyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON repository policy text applied to the repository.
setRepositoryPolicyResponse_policyText :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_policyText = Lens.lens (\SetRepositoryPolicyResponse' {policyText} -> policyText) (\s@SetRepositoryPolicyResponse' {} a -> s {policyText = a} :: SetRepositoryPolicyResponse)

-- | The registry ID associated with the request.
setRepositoryPolicyResponse_registryId :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_registryId = Lens.lens (\SetRepositoryPolicyResponse' {registryId} -> registryId) (\s@SetRepositoryPolicyResponse' {} a -> s {registryId = a} :: SetRepositoryPolicyResponse)

-- | The repository name associated with the request.
setRepositoryPolicyResponse_repositoryName :: Lens.Lens' SetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
setRepositoryPolicyResponse_repositoryName = Lens.lens (\SetRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@SetRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: SetRepositoryPolicyResponse)

-- | The response's http status code.
setRepositoryPolicyResponse_httpStatus :: Lens.Lens' SetRepositoryPolicyResponse Prelude.Int
setRepositoryPolicyResponse_httpStatus = Lens.lens (\SetRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@SetRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: SetRepositoryPolicyResponse)

instance Prelude.NFData SetRepositoryPolicyResponse where
  rnf SetRepositoryPolicyResponse' {..} =
    Prelude.rnf policyText
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf httpStatus
