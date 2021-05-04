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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pRepositoryName_ =
  GetLifecyclePolicy'
    { registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
getLifecyclePolicy_registryId :: Lens.Lens' GetLifecyclePolicy (Prelude.Maybe Prelude.Text)
getLifecyclePolicy_registryId = Lens.lens (\GetLifecyclePolicy' {registryId} -> registryId) (\s@GetLifecyclePolicy' {} a -> s {registryId = a} :: GetLifecyclePolicy)

-- | The name of the repository.
getLifecyclePolicy_repositoryName :: Lens.Lens' GetLifecyclePolicy Prelude.Text
getLifecyclePolicy_repositoryName = Lens.lens (\GetLifecyclePolicy' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicy' {} a -> s {repositoryName = a} :: GetLifecyclePolicy)

instance Prelude.AWSRequest GetLifecyclePolicy where
  type
    Rs GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (x Prelude..?> "lifecyclePolicyText")
            Prelude.<*> (x Prelude..?> "lastEvaluatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLifecyclePolicy

instance Prelude.NFData GetLifecyclePolicy

instance Prelude.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath GetLifecyclePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse pHttpStatus_ =
  GetLifecyclePolicyResponse'
    { registryId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      lastEvaluatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
getLifecyclePolicyResponse_registryId :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_registryId = Lens.lens (\GetLifecyclePolicyResponse' {registryId} -> registryId) (\s@GetLifecyclePolicyResponse' {} a -> s {registryId = a} :: GetLifecyclePolicyResponse)

-- | The repository name associated with the request.
getLifecyclePolicyResponse_repositoryName :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_repositoryName = Lens.lens (\GetLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: GetLifecyclePolicyResponse)

-- | The JSON lifecycle policy text.
getLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\GetLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@GetLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyResponse)

-- | The time stamp of the last time that the lifecycle policy was run.
getLifecyclePolicyResponse_lastEvaluatedAt :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe Prelude.UTCTime)
getLifecyclePolicyResponse_lastEvaluatedAt = Lens.lens (\GetLifecyclePolicyResponse' {lastEvaluatedAt} -> lastEvaluatedAt) (\s@GetLifecyclePolicyResponse' {} a -> s {lastEvaluatedAt = a} :: GetLifecyclePolicyResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Prelude.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

instance Prelude.NFData GetLifecyclePolicyResponse
