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
-- Module      : Amazonka.ECR.GetLifecyclePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the lifecycle policy for the specified repository.
module Amazonka.ECR.GetLifecyclePolicy
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
    getLifecyclePolicyResponse_lastEvaluatedAt,
    getLifecyclePolicyResponse_repositoryName,
    getLifecyclePolicyResponse_registryId,
    getLifecyclePolicyResponse_lifecyclePolicyText,
    getLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getLifecyclePolicy_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'getLifecyclePolicy_repositoryName' - The name of the repository.
newGetLifecyclePolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pRepositoryName_ =
  GetLifecyclePolicy'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
getLifecyclePolicy_registryId :: Lens.Lens' GetLifecyclePolicy (Prelude.Maybe Prelude.Text)
getLifecyclePolicy_registryId = Lens.lens (\GetLifecyclePolicy' {registryId} -> registryId) (\s@GetLifecyclePolicy' {} a -> s {registryId = a} :: GetLifecyclePolicy)

-- | The name of the repository.
getLifecyclePolicy_repositoryName :: Lens.Lens' GetLifecyclePolicy Prelude.Text
getLifecyclePolicy_repositoryName = Lens.lens (\GetLifecyclePolicy' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicy' {} a -> s {repositoryName = a} :: GetLifecyclePolicy)

instance Core.AWSRequest GetLifecyclePolicy where
  type
    AWSResponse GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Prelude.<$> (x Data..?> "lastEvaluatedAt")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "lifecyclePolicyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLifecyclePolicy where
  hashWithSalt _salt GetLifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData GetLifecyclePolicy where
  rnf GetLifecyclePolicy' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath GetLifecyclePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Prelude.Maybe Data.POSIX,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedAt', 'getLifecyclePolicyResponse_lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- 'repositoryName', 'getLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'registryId', 'getLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'lifecyclePolicyText', 'getLifecyclePolicyResponse_lifecyclePolicyText' - The JSON lifecycle policy text.
--
-- 'httpStatus', 'getLifecyclePolicyResponse_httpStatus' - The response's http status code.
newGetLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse pHttpStatus_ =
  GetLifecyclePolicyResponse'
    { lastEvaluatedAt =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      registryId = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time stamp of the last time that the lifecycle policy was run.
getLifecyclePolicyResponse_lastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.UTCTime)
getLifecyclePolicyResponse_lastEvaluatedAt = Lens.lens (\GetLifecyclePolicyResponse' {lastEvaluatedAt} -> lastEvaluatedAt) (\s@GetLifecyclePolicyResponse' {} a -> s {lastEvaluatedAt = a} :: GetLifecyclePolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The repository name associated with the request.
getLifecyclePolicyResponse_repositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_repositoryName = Lens.lens (\GetLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: GetLifecyclePolicyResponse)

-- | The registry ID associated with the request.
getLifecyclePolicyResponse_registryId :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_registryId = Lens.lens (\GetLifecyclePolicyResponse' {registryId} -> registryId) (\s@GetLifecyclePolicyResponse' {} a -> s {registryId = a} :: GetLifecyclePolicyResponse)

-- | The JSON lifecycle policy text.
getLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\GetLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@GetLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyResponse)

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Prelude.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

instance Prelude.NFData GetLifecyclePolicyResponse where
  rnf GetLifecyclePolicyResponse' {..} =
    Prelude.rnf lastEvaluatedAt
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf lifecyclePolicyText
      `Prelude.seq` Prelude.rnf httpStatus
