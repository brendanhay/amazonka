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
-- Module      : Network.AWS.SageMaker.GetModelPackageGroupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that manages access for a model group. For
-- information about resource policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_identity-vs-resource.html Identity-based policies and resource-based policies>
-- in the /AWS Identity and Access Management User Guide./.
module Network.AWS.SageMaker.GetModelPackageGroupPolicy
  ( -- * Creating a Request
    GetModelPackageGroupPolicy (..),
    newGetModelPackageGroupPolicy,

    -- * Request Lenses
    getModelPackageGroupPolicy_modelPackageGroupName,

    -- * Destructuring the Response
    GetModelPackageGroupPolicyResponse (..),
    newGetModelPackageGroupPolicyResponse,

    -- * Response Lenses
    getModelPackageGroupPolicyResponse_httpStatus,
    getModelPackageGroupPolicyResponse_resourcePolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newGetModelPackageGroupPolicy' smart constructor.
data GetModelPackageGroupPolicy = GetModelPackageGroupPolicy'
  { -- | The name of the model group for which to get the resource policy.
    modelPackageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetModelPackageGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'getModelPackageGroupPolicy_modelPackageGroupName' - The name of the model group for which to get the resource policy.
newGetModelPackageGroupPolicy ::
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  GetModelPackageGroupPolicy
newGetModelPackageGroupPolicy pModelPackageGroupName_ =
  GetModelPackageGroupPolicy'
    { modelPackageGroupName =
        pModelPackageGroupName_
    }

-- | The name of the model group for which to get the resource policy.
getModelPackageGroupPolicy_modelPackageGroupName :: Lens.Lens' GetModelPackageGroupPolicy Prelude.Text
getModelPackageGroupPolicy_modelPackageGroupName = Lens.lens (\GetModelPackageGroupPolicy' {modelPackageGroupName} -> modelPackageGroupName) (\s@GetModelPackageGroupPolicy' {} a -> s {modelPackageGroupName = a} :: GetModelPackageGroupPolicy)

instance
  Prelude.AWSRequest
    GetModelPackageGroupPolicy
  where
  type
    Rs GetModelPackageGroupPolicy =
      GetModelPackageGroupPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelPackageGroupPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ResourcePolicy")
      )

instance Prelude.Hashable GetModelPackageGroupPolicy

instance Prelude.NFData GetModelPackageGroupPolicy

instance Prelude.ToHeaders GetModelPackageGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.GetModelPackageGroupPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetModelPackageGroupPolicy where
  toJSON GetModelPackageGroupPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelPackageGroupName"
                  Prelude..= modelPackageGroupName
              )
          ]
      )

instance Prelude.ToPath GetModelPackageGroupPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetModelPackageGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetModelPackageGroupPolicyResponse' smart constructor.
data GetModelPackageGroupPolicyResponse = GetModelPackageGroupPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resource policy for the model group.
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetModelPackageGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getModelPackageGroupPolicyResponse_httpStatus' - The response's http status code.
--
-- 'resourcePolicy', 'getModelPackageGroupPolicyResponse_resourcePolicy' - The resource policy for the model group.
newGetModelPackageGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourcePolicy'
  Prelude.Text ->
  GetModelPackageGroupPolicyResponse
newGetModelPackageGroupPolicyResponse
  pHttpStatus_
  pResourcePolicy_ =
    GetModelPackageGroupPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        resourcePolicy = pResourcePolicy_
      }

-- | The response's http status code.
getModelPackageGroupPolicyResponse_httpStatus :: Lens.Lens' GetModelPackageGroupPolicyResponse Prelude.Int
getModelPackageGroupPolicyResponse_httpStatus = Lens.lens (\GetModelPackageGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@GetModelPackageGroupPolicyResponse' {} a -> s {httpStatus = a} :: GetModelPackageGroupPolicyResponse)

-- | The resource policy for the model group.
getModelPackageGroupPolicyResponse_resourcePolicy :: Lens.Lens' GetModelPackageGroupPolicyResponse Prelude.Text
getModelPackageGroupPolicyResponse_resourcePolicy = Lens.lens (\GetModelPackageGroupPolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@GetModelPackageGroupPolicyResponse' {} a -> s {resourcePolicy = a} :: GetModelPackageGroupPolicyResponse)

instance
  Prelude.NFData
    GetModelPackageGroupPolicyResponse
