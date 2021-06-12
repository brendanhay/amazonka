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
-- Module      : Network.AWS.Organizations.UpdatePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing policy with a new name, description, or content. If
-- you don\'t supply any parameter, that value remains unchanged. You
-- can\'t change a policy\'s type.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.UpdatePolicy
  ( -- * Creating a Request
    UpdatePolicy (..),
    newUpdatePolicy,

    -- * Request Lenses
    updatePolicy_name,
    updatePolicy_content,
    updatePolicy_description,
    updatePolicy_policyId,

    -- * Destructuring the Response
    UpdatePolicyResponse (..),
    newUpdatePolicyResponse,

    -- * Response Lenses
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { -- | If provided, the new name for the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Core.Maybe Core.Text,
    -- | If provided, the new content for the policy. The text must be correctly
    -- formatted JSON that complies with the syntax for the policy\'s type. For
    -- more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
    -- in the /AWS Organizations User Guide./
    content :: Core.Maybe Core.Text,
    -- | If provided, the new description for the policy.
    description :: Core.Maybe Core.Text,
    -- | The unique identifier (ID) of the policy that you want to update.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updatePolicy_name' - If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'content', 'updatePolicy_content' - If provided, the new content for the policy. The text must be correctly
-- formatted JSON that complies with the syntax for the policy\'s type. For
-- more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /AWS Organizations User Guide./
--
-- 'description', 'updatePolicy_description' - If provided, the new description for the policy.
--
-- 'policyId', 'updatePolicy_policyId' - The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
newUpdatePolicy ::
  -- | 'policyId'
  Core.Text ->
  UpdatePolicy
newUpdatePolicy pPolicyId_ =
  UpdatePolicy'
    { name = Core.Nothing,
      content = Core.Nothing,
      description = Core.Nothing,
      policyId = pPolicyId_
    }

-- | If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
updatePolicy_name :: Lens.Lens' UpdatePolicy (Core.Maybe Core.Text)
updatePolicy_name = Lens.lens (\UpdatePolicy' {name} -> name) (\s@UpdatePolicy' {} a -> s {name = a} :: UpdatePolicy)

-- | If provided, the new content for the policy. The text must be correctly
-- formatted JSON that complies with the syntax for the policy\'s type. For
-- more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /AWS Organizations User Guide./
updatePolicy_content :: Lens.Lens' UpdatePolicy (Core.Maybe Core.Text)
updatePolicy_content = Lens.lens (\UpdatePolicy' {content} -> content) (\s@UpdatePolicy' {} a -> s {content = a} :: UpdatePolicy)

-- | If provided, the new description for the policy.
updatePolicy_description :: Lens.Lens' UpdatePolicy (Core.Maybe Core.Text)
updatePolicy_description = Lens.lens (\UpdatePolicy' {description} -> description) (\s@UpdatePolicy' {} a -> s {description = a} :: UpdatePolicy)

-- | The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
updatePolicy_policyId :: Lens.Lens' UpdatePolicy Core.Text
updatePolicy_policyId = Lens.lens (\UpdatePolicy' {policyId} -> policyId) (\s@UpdatePolicy' {} a -> s {policyId = a} :: UpdatePolicy)

instance Core.AWSRequest UpdatePolicy where
  type AWSResponse UpdatePolicy = UpdatePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePolicyResponse'
            Core.<$> (x Core..?> "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePolicy

instance Core.NFData UpdatePolicy

instance Core.ToHeaders UpdatePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.UpdatePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePolicy where
  toJSON UpdatePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Content" Core..=) Core.<$> content,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("PolicyId" Core..= policyId)
          ]
      )

instance Core.ToPath UpdatePolicy where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePolicyResponse' smart constructor.
data UpdatePolicyResponse = UpdatePolicyResponse'
  { -- | A structure that contains details about the updated policy, showing the
    -- requested changes.
    policy :: Core.Maybe Policy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'updatePolicyResponse_policy' - A structure that contains details about the updated policy, showing the
-- requested changes.
--
-- 'httpStatus', 'updatePolicyResponse_httpStatus' - The response's http status code.
newUpdatePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePolicyResponse
newUpdatePolicyResponse pHttpStatus_ =
  UpdatePolicyResponse'
    { policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the updated policy, showing the
-- requested changes.
updatePolicyResponse_policy :: Lens.Lens' UpdatePolicyResponse (Core.Maybe Policy)
updatePolicyResponse_policy = Lens.lens (\UpdatePolicyResponse' {policy} -> policy) (\s@UpdatePolicyResponse' {} a -> s {policy = a} :: UpdatePolicyResponse)

-- | The response's http status code.
updatePolicyResponse_httpStatus :: Lens.Lens' UpdatePolicyResponse Core.Int
updatePolicyResponse_httpStatus = Lens.lens (\UpdatePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdatePolicyResponse' {} a -> s {httpStatus = a} :: UpdatePolicyResponse)

instance Core.NFData UpdatePolicyResponse
