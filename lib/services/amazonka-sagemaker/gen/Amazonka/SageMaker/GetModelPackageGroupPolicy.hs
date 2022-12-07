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
-- Module      : Amazonka.SageMaker.GetModelPackageGroupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that manages access for a model group. For
-- information about resource policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_identity-vs-resource.html Identity-based policies and resource-based policies>
-- in the /Amazon Web Services Identity and Access Management User Guide./.
module Amazonka.SageMaker.GetModelPackageGroupPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newGetModelPackageGroupPolicy' smart constructor.
data GetModelPackageGroupPolicy = GetModelPackageGroupPolicy'
  { -- | The name of the model group for which to get the resource policy.
    modelPackageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetModelPackageGroupPolicy where
  type
    AWSResponse GetModelPackageGroupPolicy =
      GetModelPackageGroupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelPackageGroupPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ResourcePolicy")
      )

instance Prelude.Hashable GetModelPackageGroupPolicy where
  hashWithSalt _salt GetModelPackageGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName

instance Prelude.NFData GetModelPackageGroupPolicy where
  rnf GetModelPackageGroupPolicy' {..} =
    Prelude.rnf modelPackageGroupName

instance Data.ToHeaders GetModelPackageGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.GetModelPackageGroupPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetModelPackageGroupPolicy where
  toJSON GetModelPackageGroupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelPackageGroupName"
                  Data..= modelPackageGroupName
              )
          ]
      )

instance Data.ToPath GetModelPackageGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetModelPackageGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetModelPackageGroupPolicyResponse' smart constructor.
data GetModelPackageGroupPolicyResponse = GetModelPackageGroupPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resource policy for the model group.
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetModelPackageGroupPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourcePolicy
