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
-- Module      : Amazonka.ECR.PutLifecyclePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the lifecycle policy for the specified repository.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/LifecyclePolicies.html Lifecycle policy template>.
module Amazonka.ECR.PutLifecyclePolicy
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
    putLifecyclePolicyResponse_repositoryName,
    putLifecyclePolicyResponse_registryId,
    putLifecyclePolicyResponse_lifecyclePolicyText,
    putLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository. If you do  not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to receive the policy.
    repositoryName :: Prelude.Text,
    -- | The JSON repository policy text to apply to the repository.
    lifecyclePolicyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putLifecyclePolicy_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do  not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'putLifecyclePolicy_repositoryName' - The name of the repository to receive the policy.
--
-- 'lifecyclePolicyText', 'putLifecyclePolicy_lifecyclePolicyText' - The JSON repository policy text to apply to the repository.
newPutLifecyclePolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'lifecyclePolicyText'
  Prelude.Text ->
  PutLifecyclePolicy
newPutLifecyclePolicy
  pRepositoryName_
  pLifecyclePolicyText_ =
    PutLifecyclePolicy'
      { registryId = Prelude.Nothing,
        repositoryName = pRepositoryName_,
        lifecyclePolicyText = pLifecyclePolicyText_
      }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do  not specify a registry, the default
-- registry is assumed.
putLifecyclePolicy_registryId :: Lens.Lens' PutLifecyclePolicy (Prelude.Maybe Prelude.Text)
putLifecyclePolicy_registryId = Lens.lens (\PutLifecyclePolicy' {registryId} -> registryId) (\s@PutLifecyclePolicy' {} a -> s {registryId = a} :: PutLifecyclePolicy)

-- | The name of the repository to receive the policy.
putLifecyclePolicy_repositoryName :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_repositoryName = Lens.lens (\PutLifecyclePolicy' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicy' {} a -> s {repositoryName = a} :: PutLifecyclePolicy)

-- | The JSON repository policy text to apply to the repository.
putLifecyclePolicy_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicy' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicy' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicy)

instance Core.AWSRequest PutLifecyclePolicy where
  type
    AWSResponse PutLifecyclePolicy =
      PutLifecyclePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Prelude.<$> (x Core..?> "repositoryName")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (x Core..?> "lifecyclePolicyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLifecyclePolicy where
  hashWithSalt _salt PutLifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` lifecyclePolicyText

instance Prelude.NFData PutLifecyclePolicy where
  rnf PutLifecyclePolicy' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf lifecyclePolicyText

instance Core.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ("lifecyclePolicyText" Core..= lifecyclePolicyText)
          ]
      )

instance Core.ToPath PutLifecyclePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'putLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'registryId', 'putLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'lifecyclePolicyText', 'putLifecyclePolicyResponse_lifecyclePolicyText' - The JSON repository policy text.
--
-- 'httpStatus', 'putLifecyclePolicyResponse_httpStatus' - The response's http status code.
newPutLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLifecyclePolicyResponse
newPutLifecyclePolicyResponse pHttpStatus_ =
  PutLifecyclePolicyResponse'
    { repositoryName =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository name associated with the request.
putLifecyclePolicyResponse_repositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_repositoryName = Lens.lens (\PutLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: PutLifecyclePolicyResponse)

-- | The registry ID associated with the request.
putLifecyclePolicyResponse_registryId :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_registryId = Lens.lens (\PutLifecyclePolicyResponse' {registryId} -> registryId) (\s@PutLifecyclePolicyResponse' {} a -> s {registryId = a} :: PutLifecyclePolicyResponse)

-- | The JSON repository policy text.
putLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicyResponse)

-- | The response's http status code.
putLifecyclePolicyResponse_httpStatus :: Lens.Lens' PutLifecyclePolicyResponse Prelude.Int
putLifecyclePolicyResponse_httpStatus = Lens.lens (\PutLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@PutLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: PutLifecyclePolicyResponse)

instance Prelude.NFData PutLifecyclePolicyResponse where
  rnf PutLifecyclePolicyResponse' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf lifecyclePolicyText
      `Prelude.seq` Prelude.rnf httpStatus
