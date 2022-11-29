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
-- Module      : Amazonka.Organizations.CreatePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy of a specified type that you can attach to a root, an
-- organizational unit (OU), or an individual Amazon Web Services account.
--
-- For more information about policies and their use, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html Managing Organization Policies>.
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission.
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.CreatePolicy
  ( -- * Creating a Request
    CreatePolicy (..),
    newCreatePolicy,

    -- * Request Lenses
    createPolicy_tags,
    createPolicy_content,
    createPolicy_description,
    createPolicy_name,
    createPolicy_type,

    -- * Destructuring the Response
    CreatePolicyResponse (..),
    newCreatePolicyResponse,

    -- * Response Lenses
    createPolicyResponse_policy,
    createPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | A list of tags that you want to attach to the newly created policy. For
    -- each tag in the list, you must specify both a tag key and a value. You
    -- can set the value to an empty string, but you can\'t set it to @null@.
    -- For more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
    -- in the Organizations User Guide.
    --
    -- If any one of the tags is invalid or if you exceed the allowed number of
    -- tags for a policy, then the entire request fails and the policy is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The policy text content to add to the new policy. The text that you
    -- supply must adhere to the rules of the policy type you specify in the
    -- @Type@ parameter.
    content :: Prelude.Text,
    -- | An optional description to assign to the policy.
    description :: Prelude.Text,
    -- | The friendly name to assign to the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Text,
    -- | The type of policy to create. You can specify one of the following
    -- values:
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    type' :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPolicy_tags' - A list of tags that you want to attach to the newly created policy. For
-- each tag in the list, you must specify both a tag key and a value. You
-- can set the value to an empty string, but you can\'t set it to @null@.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for a policy, then the entire request fails and the policy is not
-- created.
--
-- 'content', 'createPolicy_content' - The policy text content to add to the new policy. The text that you
-- supply must adhere to the rules of the policy type you specify in the
-- @Type@ parameter.
--
-- 'description', 'createPolicy_description' - An optional description to assign to the policy.
--
-- 'name', 'createPolicy_name' - The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'type'', 'createPolicy_type' - The type of policy to create. You can specify one of the following
-- values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
newCreatePolicy ::
  -- | 'content'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  PolicyType ->
  CreatePolicy
newCreatePolicy pContent_ pDescription_ pName_ pType_ =
  CreatePolicy'
    { tags = Prelude.Nothing,
      content = pContent_,
      description = pDescription_,
      name = pName_,
      type' = pType_
    }

-- | A list of tags that you want to attach to the newly created policy. For
-- each tag in the list, you must specify both a tag key and a value. You
-- can set the value to an empty string, but you can\'t set it to @null@.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for a policy, then the entire request fails and the policy is not
-- created.
createPolicy_tags :: Lens.Lens' CreatePolicy (Prelude.Maybe [Tag])
createPolicy_tags = Lens.lens (\CreatePolicy' {tags} -> tags) (\s@CreatePolicy' {} a -> s {tags = a} :: CreatePolicy) Prelude.. Lens.mapping Lens.coerced

-- | The policy text content to add to the new policy. The text that you
-- supply must adhere to the rules of the policy type you specify in the
-- @Type@ parameter.
createPolicy_content :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_content = Lens.lens (\CreatePolicy' {content} -> content) (\s@CreatePolicy' {} a -> s {content = a} :: CreatePolicy)

-- | An optional description to assign to the policy.
createPolicy_description :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_description = Lens.lens (\CreatePolicy' {description} -> description) (\s@CreatePolicy' {} a -> s {description = a} :: CreatePolicy)

-- | The friendly name to assign to the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
createPolicy_name :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_name = Lens.lens (\CreatePolicy' {name} -> name) (\s@CreatePolicy' {} a -> s {name = a} :: CreatePolicy)

-- | The type of policy to create. You can specify one of the following
-- values:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
createPolicy_type :: Lens.Lens' CreatePolicy PolicyType
createPolicy_type = Lens.lens (\CreatePolicy' {type'} -> type') (\s@CreatePolicy' {} a -> s {type' = a} :: CreatePolicy)

instance Core.AWSRequest CreatePolicy where
  type AWSResponse CreatePolicy = CreatePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicy where
  hashWithSalt _salt CreatePolicy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreatePolicy where
  rnf CreatePolicy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Core.ToHeaders CreatePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.CreatePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePolicy where
  toJSON CreatePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Content" Core..= content),
            Prelude.Just ("Description" Core..= description),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath CreatePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | A structure that contains details about the newly created policy.
    policy :: Prelude.Maybe Policy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'createPolicyResponse_policy' - A structure that contains details about the newly created policy.
--
-- 'httpStatus', 'createPolicyResponse_httpStatus' - The response's http status code.
newCreatePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePolicyResponse
newCreatePolicyResponse pHttpStatus_ =
  CreatePolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the newly created policy.
createPolicyResponse_policy :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Policy)
createPolicyResponse_policy = Lens.lens (\CreatePolicyResponse' {policy} -> policy) (\s@CreatePolicyResponse' {} a -> s {policy = a} :: CreatePolicyResponse)

-- | The response's http status code.
createPolicyResponse_httpStatus :: Lens.Lens' CreatePolicyResponse Prelude.Int
createPolicyResponse_httpStatus = Lens.lens (\CreatePolicyResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyResponse' {} a -> s {httpStatus = a} :: CreatePolicyResponse)

instance Prelude.NFData CreatePolicyResponse where
  rnf CreatePolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
