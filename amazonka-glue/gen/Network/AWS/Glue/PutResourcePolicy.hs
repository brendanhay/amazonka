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
-- Module      : Network.AWS.Glue.PutResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Data Catalog resource policy for access control.
module Network.AWS.Glue.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_resourceArn,
    putResourcePolicy_enableHybrid,
    putResourcePolicy_policyHashCondition,
    putResourcePolicy_policyExistsCondition,
    putResourcePolicy_policyInJson,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_policyHash,
    putResourcePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The ARN of the AWS Glue resource for the resource policy to be set. For
    -- more information about AWS Glue resource ARNs, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
    resourceArn :: Core.Maybe Core.Text,
    -- | Allows you to specify if you want to use both resource-level and
    -- account\/catalog-level resource policies. A resource-level policy is a
    -- policy attached to an individual resource such as a database or a table.
    --
    -- The default value of @NO@ indicates that resource-level policies cannot
    -- co-exist with an account-level policy. A value of @YES@ means the use of
    -- both resource-level and account\/catalog-level resource policies is
    -- allowed.
    enableHybrid :: Core.Maybe EnableHybridValues,
    -- | The hash value returned when the previous policy was set using
    -- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
    -- of a policy. Do not use this parameter if no previous policy has been
    -- set.
    policyHashCondition :: Core.Maybe Core.Text,
    -- | A value of @MUST_EXIST@ is used to update a policy. A value of
    -- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
    -- null value is used, the call will not depend on the existence of a
    -- policy.
    policyExistsCondition :: Core.Maybe ExistCondition,
    -- | Contains the policy document to set, in JSON format.
    policyInJson :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The ARN of the AWS Glue resource for the resource policy to be set. For
-- more information about AWS Glue resource ARNs, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
--
-- 'enableHybrid', 'putResourcePolicy_enableHybrid' - Allows you to specify if you want to use both resource-level and
-- account\/catalog-level resource policies. A resource-level policy is a
-- policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot
-- co-exist with an account-level policy. A value of @YES@ means the use of
-- both resource-level and account\/catalog-level resource policies is
-- allowed.
--
-- 'policyHashCondition', 'putResourcePolicy_policyHashCondition' - The hash value returned when the previous policy was set using
-- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
-- of a policy. Do not use this parameter if no previous policy has been
-- set.
--
-- 'policyExistsCondition', 'putResourcePolicy_policyExistsCondition' - A value of @MUST_EXIST@ is used to update a policy. A value of
-- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
-- null value is used, the call will not depend on the existence of a
-- policy.
--
-- 'policyInJson', 'putResourcePolicy_policyInJson' - Contains the policy document to set, in JSON format.
newPutResourcePolicy ::
  -- | 'policyInJson'
  Core.Text ->
  PutResourcePolicy
newPutResourcePolicy pPolicyInJson_ =
  PutResourcePolicy'
    { resourceArn = Core.Nothing,
      enableHybrid = Core.Nothing,
      policyHashCondition = Core.Nothing,
      policyExistsCondition = Core.Nothing,
      policyInJson = pPolicyInJson_
    }

-- | The ARN of the AWS Glue resource for the resource policy to be set. For
-- more information about AWS Glue resource ARNs, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy (Core.Maybe Core.Text)
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | Allows you to specify if you want to use both resource-level and
-- account\/catalog-level resource policies. A resource-level policy is a
-- policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot
-- co-exist with an account-level policy. A value of @YES@ means the use of
-- both resource-level and account\/catalog-level resource policies is
-- allowed.
putResourcePolicy_enableHybrid :: Lens.Lens' PutResourcePolicy (Core.Maybe EnableHybridValues)
putResourcePolicy_enableHybrid = Lens.lens (\PutResourcePolicy' {enableHybrid} -> enableHybrid) (\s@PutResourcePolicy' {} a -> s {enableHybrid = a} :: PutResourcePolicy)

-- | The hash value returned when the previous policy was set using
-- @PutResourcePolicy@. Its purpose is to prevent concurrent modifications
-- of a policy. Do not use this parameter if no previous policy has been
-- set.
putResourcePolicy_policyHashCondition :: Lens.Lens' PutResourcePolicy (Core.Maybe Core.Text)
putResourcePolicy_policyHashCondition = Lens.lens (\PutResourcePolicy' {policyHashCondition} -> policyHashCondition) (\s@PutResourcePolicy' {} a -> s {policyHashCondition = a} :: PutResourcePolicy)

-- | A value of @MUST_EXIST@ is used to update a policy. A value of
-- @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a
-- null value is used, the call will not depend on the existence of a
-- policy.
putResourcePolicy_policyExistsCondition :: Lens.Lens' PutResourcePolicy (Core.Maybe ExistCondition)
putResourcePolicy_policyExistsCondition = Lens.lens (\PutResourcePolicy' {policyExistsCondition} -> policyExistsCondition) (\s@PutResourcePolicy' {} a -> s {policyExistsCondition = a} :: PutResourcePolicy)

-- | Contains the policy document to set, in JSON format.
putResourcePolicy_policyInJson :: Lens.Lens' PutResourcePolicy Core.Text
putResourcePolicy_policyInJson = Lens.lens (\PutResourcePolicy' {policyInJson} -> policyInJson) (\s@PutResourcePolicy' {} a -> s {policyInJson = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Core.<$> (x Core..?> "PolicyHash")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutResourcePolicy

instance Core.NFData PutResourcePolicy

instance Core.ToHeaders PutResourcePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.PutResourcePolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceArn" Core..=) Core.<$> resourceArn,
            ("EnableHybrid" Core..=) Core.<$> enableHybrid,
            ("PolicyHashCondition" Core..=)
              Core.<$> policyHashCondition,
            ("PolicyExistsCondition" Core..=)
              Core.<$> policyExistsCondition,
            Core.Just ("PolicyInJson" Core..= policyInJson)
          ]
      )

instance Core.ToPath PutResourcePolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutResourcePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | A hash of the policy that has just been set. This must be included in a
    -- subsequent call that overwrites or updates this policy.
    policyHash :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyHash', 'putResourcePolicyResponse_policyHash' - A hash of the policy that has just been set. This must be included in a
-- subsequent call that overwrites or updates this policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { policyHash =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A hash of the policy that has just been set. This must be included in a
-- subsequent call that overwrites or updates this policy.
putResourcePolicyResponse_policyHash :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Core.Text)
putResourcePolicyResponse_policyHash = Lens.lens (\PutResourcePolicyResponse' {policyHash} -> policyHash) (\s@PutResourcePolicyResponse' {} a -> s {policyHash = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Core.NFData PutResourcePolicyResponse
