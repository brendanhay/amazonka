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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do  not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to receive the policy.
    repositoryName :: Prelude.Text,
    -- | The JSON repository policy text to apply to the repository.
    lifecyclePolicyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do  not specify a registry, the default registry is
-- assumed.
putLifecyclePolicy_registryId :: Lens.Lens' PutLifecyclePolicy (Prelude.Maybe Prelude.Text)
putLifecyclePolicy_registryId = Lens.lens (\PutLifecyclePolicy' {registryId} -> registryId) (\s@PutLifecyclePolicy' {} a -> s {registryId = a} :: PutLifecyclePolicy)

-- | The name of the repository to receive the policy.
putLifecyclePolicy_repositoryName :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_repositoryName = Lens.lens (\PutLifecyclePolicy' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicy' {} a -> s {repositoryName = a} :: PutLifecyclePolicy)

-- | The JSON repository policy text to apply to the repository.
putLifecyclePolicy_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicy' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicy' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicy)

instance Prelude.AWSRequest PutLifecyclePolicy where
  type
    Rs PutLifecyclePolicy =
      PutLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (x Prelude..?> "lifecyclePolicyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLifecyclePolicy

instance Prelude.NFData PutLifecyclePolicy

instance Prelude.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutLifecyclePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just
              ( "lifecyclePolicyText"
                  Prelude..= lifecyclePolicyText
              )
          ]
      )

instance Prelude.ToPath PutLifecyclePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutLifecyclePolicyResponse
newPutLifecyclePolicyResponse pHttpStatus_ =
  PutLifecyclePolicyResponse'
    { registryId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
putLifecyclePolicyResponse_registryId :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_registryId = Lens.lens (\PutLifecyclePolicyResponse' {registryId} -> registryId) (\s@PutLifecyclePolicyResponse' {} a -> s {registryId = a} :: PutLifecyclePolicyResponse)

-- | The repository name associated with the request.
putLifecyclePolicyResponse_repositoryName :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_repositoryName = Lens.lens (\PutLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@PutLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: PutLifecyclePolicyResponse)

-- | The JSON repository policy text.
putLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' PutLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
putLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\PutLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@PutLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: PutLifecyclePolicyResponse)

-- | The response's http status code.
putLifecyclePolicyResponse_httpStatus :: Lens.Lens' PutLifecyclePolicyResponse Prelude.Int
putLifecyclePolicyResponse_httpStatus = Lens.lens (\PutLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@PutLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: PutLifecyclePolicyResponse)

instance Prelude.NFData PutLifecyclePolicyResponse
