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
-- Module      : Network.AWS.SageMaker.PutModelPackageGroupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a resouce policy to control access to a model group. For
-- information about resoure policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_identity-vs-resource.html Identity-based policies and resource-based policies>
-- in the /AWS Identity and Access Management User Guide./.
module Network.AWS.SageMaker.PutModelPackageGroupPolicy
  ( -- * Creating a Request
    PutModelPackageGroupPolicy (..),
    newPutModelPackageGroupPolicy,

    -- * Request Lenses
    putModelPackageGroupPolicy_modelPackageGroupName,
    putModelPackageGroupPolicy_resourcePolicy,

    -- * Destructuring the Response
    PutModelPackageGroupPolicyResponse (..),
    newPutModelPackageGroupPolicyResponse,

    -- * Response Lenses
    putModelPackageGroupPolicyResponse_httpStatus,
    putModelPackageGroupPolicyResponse_modelPackageGroupArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newPutModelPackageGroupPolicy' smart constructor.
data PutModelPackageGroupPolicy = PutModelPackageGroupPolicy'
  { -- | The name of the model group to add a resource policy to.
    modelPackageGroupName :: Core.Text,
    -- | The resource policy for the model group.
    resourcePolicy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutModelPackageGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'putModelPackageGroupPolicy_modelPackageGroupName' - The name of the model group to add a resource policy to.
--
-- 'resourcePolicy', 'putModelPackageGroupPolicy_resourcePolicy' - The resource policy for the model group.
newPutModelPackageGroupPolicy ::
  -- | 'modelPackageGroupName'
  Core.Text ->
  -- | 'resourcePolicy'
  Core.Text ->
  PutModelPackageGroupPolicy
newPutModelPackageGroupPolicy
  pModelPackageGroupName_
  pResourcePolicy_ =
    PutModelPackageGroupPolicy'
      { modelPackageGroupName =
          pModelPackageGroupName_,
        resourcePolicy = pResourcePolicy_
      }

-- | The name of the model group to add a resource policy to.
putModelPackageGroupPolicy_modelPackageGroupName :: Lens.Lens' PutModelPackageGroupPolicy Core.Text
putModelPackageGroupPolicy_modelPackageGroupName = Lens.lens (\PutModelPackageGroupPolicy' {modelPackageGroupName} -> modelPackageGroupName) (\s@PutModelPackageGroupPolicy' {} a -> s {modelPackageGroupName = a} :: PutModelPackageGroupPolicy)

-- | The resource policy for the model group.
putModelPackageGroupPolicy_resourcePolicy :: Lens.Lens' PutModelPackageGroupPolicy Core.Text
putModelPackageGroupPolicy_resourcePolicy = Lens.lens (\PutModelPackageGroupPolicy' {resourcePolicy} -> resourcePolicy) (\s@PutModelPackageGroupPolicy' {} a -> s {resourcePolicy = a} :: PutModelPackageGroupPolicy)

instance Core.AWSRequest PutModelPackageGroupPolicy where
  type
    AWSResponse PutModelPackageGroupPolicy =
      PutModelPackageGroupPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutModelPackageGroupPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "ModelPackageGroupArn")
      )

instance Core.Hashable PutModelPackageGroupPolicy

instance Core.NFData PutModelPackageGroupPolicy

instance Core.ToHeaders PutModelPackageGroupPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.PutModelPackageGroupPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutModelPackageGroupPolicy where
  toJSON PutModelPackageGroupPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ModelPackageGroupName"
                  Core..= modelPackageGroupName
              ),
            Core.Just ("ResourcePolicy" Core..= resourcePolicy)
          ]
      )

instance Core.ToPath PutModelPackageGroupPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutModelPackageGroupPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutModelPackageGroupPolicyResponse' smart constructor.
data PutModelPackageGroupPolicyResponse = PutModelPackageGroupPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the model package group.
    modelPackageGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutModelPackageGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putModelPackageGroupPolicyResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageGroupArn', 'putModelPackageGroupPolicyResponse_modelPackageGroupArn' - The Amazon Resource Name (ARN) of the model package group.
newPutModelPackageGroupPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'modelPackageGroupArn'
  Core.Text ->
  PutModelPackageGroupPolicyResponse
newPutModelPackageGroupPolicyResponse
  pHttpStatus_
  pModelPackageGroupArn_ =
    PutModelPackageGroupPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        modelPackageGroupArn =
          pModelPackageGroupArn_
      }

-- | The response's http status code.
putModelPackageGroupPolicyResponse_httpStatus :: Lens.Lens' PutModelPackageGroupPolicyResponse Core.Int
putModelPackageGroupPolicyResponse_httpStatus = Lens.lens (\PutModelPackageGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@PutModelPackageGroupPolicyResponse' {} a -> s {httpStatus = a} :: PutModelPackageGroupPolicyResponse)

-- | The Amazon Resource Name (ARN) of the model package group.
putModelPackageGroupPolicyResponse_modelPackageGroupArn :: Lens.Lens' PutModelPackageGroupPolicyResponse Core.Text
putModelPackageGroupPolicyResponse_modelPackageGroupArn = Lens.lens (\PutModelPackageGroupPolicyResponse' {modelPackageGroupArn} -> modelPackageGroupArn) (\s@PutModelPackageGroupPolicyResponse' {} a -> s {modelPackageGroupArn = a} :: PutModelPackageGroupPolicyResponse)

instance
  Core.NFData
    PutModelPackageGroupPolicyResponse
