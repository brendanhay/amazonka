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
-- Module      : Amazonka.Organizations.UpdatePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Organizations.UpdatePolicy
  ( -- * Creating a Request
    UpdatePolicy (..),
    newUpdatePolicy,

    -- * Request Lenses
    updatePolicy_name,
    updatePolicy_description,
    updatePolicy_content,
    updatePolicy_policyId,

    -- * Destructuring the Response
    UpdatePolicyResponse (..),
    newUpdatePolicyResponse,

    -- * Response Lenses
    updatePolicyResponse_policy,
    updatePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { -- | If provided, the new name for the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text,
    -- | If provided, the new description for the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | If provided, the new content for the policy. The text must be correctly
    -- formatted JSON that complies with the syntax for the policy\'s type. For
    -- more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
    -- in the /Organizations User Guide./
    content :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the policy that you want to update.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'updatePolicy_description' - If provided, the new description for the policy.
--
-- 'content', 'updatePolicy_content' - If provided, the new content for the policy. The text must be correctly
-- formatted JSON that complies with the syntax for the policy\'s type. For
-- more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /Organizations User Guide./
--
-- 'policyId', 'updatePolicy_policyId' - The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
newUpdatePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  UpdatePolicy
newUpdatePolicy pPolicyId_ =
  UpdatePolicy'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      content = Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
updatePolicy_name :: Lens.Lens' UpdatePolicy (Prelude.Maybe Prelude.Text)
updatePolicy_name = Lens.lens (\UpdatePolicy' {name} -> name) (\s@UpdatePolicy' {} a -> s {name = a} :: UpdatePolicy)

-- | If provided, the new description for the policy.
updatePolicy_description :: Lens.Lens' UpdatePolicy (Prelude.Maybe Prelude.Text)
updatePolicy_description = Lens.lens (\UpdatePolicy' {description} -> description) (\s@UpdatePolicy' {} a -> s {description = a} :: UpdatePolicy)

-- | If provided, the new content for the policy. The text must be correctly
-- formatted JSON that complies with the syntax for the policy\'s type. For
-- more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax>
-- in the /Organizations User Guide./
updatePolicy_content :: Lens.Lens' UpdatePolicy (Prelude.Maybe Prelude.Text)
updatePolicy_content = Lens.lens (\UpdatePolicy' {content} -> content) (\s@UpdatePolicy' {} a -> s {content = a} :: UpdatePolicy)

-- | The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
updatePolicy_policyId :: Lens.Lens' UpdatePolicy Prelude.Text
updatePolicy_policyId = Lens.lens (\UpdatePolicy' {policyId} -> policyId) (\s@UpdatePolicy' {} a -> s {policyId = a} :: UpdatePolicy)

instance Core.AWSRequest UpdatePolicy where
  type AWSResponse UpdatePolicy = UpdatePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePolicy where
  hashWithSalt _salt UpdatePolicy' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData UpdatePolicy where
  rnf UpdatePolicy' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders UpdatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.UpdatePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePolicy where
  toJSON UpdatePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Description" Data..=) Prelude.<$> description,
            ("Content" Data..=) Prelude.<$> content,
            Prelude.Just ("PolicyId" Data..= policyId)
          ]
      )

instance Data.ToPath UpdatePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePolicyResponse' smart constructor.
data UpdatePolicyResponse = UpdatePolicyResponse'
  { -- | A structure that contains details about the updated policy, showing the
    -- requested changes.
    policy :: Prelude.Maybe Policy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdatePolicyResponse
newUpdatePolicyResponse pHttpStatus_ =
  UpdatePolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the updated policy, showing the
-- requested changes.
updatePolicyResponse_policy :: Lens.Lens' UpdatePolicyResponse (Prelude.Maybe Policy)
updatePolicyResponse_policy = Lens.lens (\UpdatePolicyResponse' {policy} -> policy) (\s@UpdatePolicyResponse' {} a -> s {policy = a} :: UpdatePolicyResponse)

-- | The response's http status code.
updatePolicyResponse_httpStatus :: Lens.Lens' UpdatePolicyResponse Prelude.Int
updatePolicyResponse_httpStatus = Lens.lens (\UpdatePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdatePolicyResponse' {} a -> s {httpStatus = a} :: UpdatePolicyResponse)

instance Prelude.NFData UpdatePolicyResponse where
  rnf UpdatePolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
