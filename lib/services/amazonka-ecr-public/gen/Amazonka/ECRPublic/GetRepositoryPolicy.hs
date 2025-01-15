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
-- Module      : Amazonka.ECRPublic.GetRepositoryPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the repository policy for the specified repository.
module Amazonka.ECRPublic.GetRepositoryPolicy
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
    getRepositoryPolicyResponse_policyText,
    getRepositoryPolicyResponse_registryId,
    getRepositoryPolicyResponse_repositoryName,
    getRepositoryPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRepositoryPolicy' smart constructor.
data GetRepositoryPolicy = GetRepositoryPolicy'
  { -- | The AWS account ID associated with the public registry that contains the
    -- repository. If you do not specify a registry, the default public
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository with the policy to retrieve.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getRepositoryPolicy_registryId' - The AWS account ID associated with the public registry that contains the
-- repository. If you do not specify a registry, the default public
-- registry is assumed.
--
-- 'repositoryName', 'getRepositoryPolicy_repositoryName' - The name of the repository with the policy to retrieve.
newGetRepositoryPolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  GetRepositoryPolicy
newGetRepositoryPolicy pRepositoryName_ =
  GetRepositoryPolicy'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the public registry that contains the
-- repository. If you do not specify a registry, the default public
-- registry is assumed.
getRepositoryPolicy_registryId :: Lens.Lens' GetRepositoryPolicy (Prelude.Maybe Prelude.Text)
getRepositoryPolicy_registryId = Lens.lens (\GetRepositoryPolicy' {registryId} -> registryId) (\s@GetRepositoryPolicy' {} a -> s {registryId = a} :: GetRepositoryPolicy)

-- | The name of the repository with the policy to retrieve.
getRepositoryPolicy_repositoryName :: Lens.Lens' GetRepositoryPolicy Prelude.Text
getRepositoryPolicy_repositoryName = Lens.lens (\GetRepositoryPolicy' {repositoryName} -> repositoryName) (\s@GetRepositoryPolicy' {} a -> s {repositoryName = a} :: GetRepositoryPolicy)

instance Core.AWSRequest GetRepositoryPolicy where
  type
    AWSResponse GetRepositoryPolicy =
      GetRepositoryPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryPolicyResponse'
            Prelude.<$> (x Data..?> "policyText")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRepositoryPolicy where
  hashWithSalt _salt GetRepositoryPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData GetRepositoryPolicy where
  rnf GetRepositoryPolicy' {..} =
    Prelude.rnf registryId `Prelude.seq`
      Prelude.rnf repositoryName

instance Data.ToHeaders GetRepositoryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.GetRepositoryPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRepositoryPolicy where
  toJSON GetRepositoryPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath GetRepositoryPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRepositoryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRepositoryPolicyResponse' smart constructor.
data GetRepositoryPolicyResponse = GetRepositoryPolicyResponse'
  { -- | The repository policy text associated with the repository. The policy
    -- text will be in JSON format.
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
-- Create a value of 'GetRepositoryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'getRepositoryPolicyResponse_policyText' - The repository policy text associated with the repository. The policy
-- text will be in JSON format.
--
-- 'registryId', 'getRepositoryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'getRepositoryPolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'getRepositoryPolicyResponse_httpStatus' - The response's http status code.
newGetRepositoryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRepositoryPolicyResponse
newGetRepositoryPolicyResponse pHttpStatus_ =
  GetRepositoryPolicyResponse'
    { policyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository policy text associated with the repository. The policy
-- text will be in JSON format.
getRepositoryPolicyResponse_policyText :: Lens.Lens' GetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
getRepositoryPolicyResponse_policyText = Lens.lens (\GetRepositoryPolicyResponse' {policyText} -> policyText) (\s@GetRepositoryPolicyResponse' {} a -> s {policyText = a} :: GetRepositoryPolicyResponse)

-- | The registry ID associated with the request.
getRepositoryPolicyResponse_registryId :: Lens.Lens' GetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
getRepositoryPolicyResponse_registryId = Lens.lens (\GetRepositoryPolicyResponse' {registryId} -> registryId) (\s@GetRepositoryPolicyResponse' {} a -> s {registryId = a} :: GetRepositoryPolicyResponse)

-- | The repository name associated with the request.
getRepositoryPolicyResponse_repositoryName :: Lens.Lens' GetRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
getRepositoryPolicyResponse_repositoryName = Lens.lens (\GetRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@GetRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: GetRepositoryPolicyResponse)

-- | The response's http status code.
getRepositoryPolicyResponse_httpStatus :: Lens.Lens' GetRepositoryPolicyResponse Prelude.Int
getRepositoryPolicyResponse_httpStatus = Lens.lens (\GetRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: GetRepositoryPolicyResponse)

instance Prelude.NFData GetRepositoryPolicyResponse where
  rnf GetRepositoryPolicyResponse' {..} =
    Prelude.rnf policyText `Prelude.seq`
      Prelude.rnf registryId `Prelude.seq`
        Prelude.rnf repositoryName `Prelude.seq`
          Prelude.rnf httpStatus
