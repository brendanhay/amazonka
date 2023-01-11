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
-- Module      : Amazonka.IAM.CreatePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your Amazon Web Services account.
--
-- This operation creates a policy version with a version identifier of
-- @v1@ and sets v1 as the policy\'s default version. For more information
-- about policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
--
-- As a best practice, you can validate your IAM policies. To learn more,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_policy-validator.html Validating IAM policies>
-- in the /IAM User Guide/.
--
-- For more information about managed policies in general, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreatePolicy
  ( -- * Creating a Request
    CreatePolicy (..),
    newCreatePolicy,

    -- * Request Lenses
    createPolicy_description,
    createPolicy_path,
    createPolicy_tags,
    createPolicy_policyName,
    createPolicy_policyDocument,

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
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { -- | A friendly description of the policy.
    --
    -- Typically used to store information about the permissions defined in the
    -- policy. For example, \"Grants access to production DynamoDB tables.\"
    --
    -- The policy description is immutable. After a value is assigned, it
    -- cannot be changed.
    description :: Prelude.Maybe Prelude.Text,
    -- | The path for the policy.
    --
    -- For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/).
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    --
    -- You cannot use an asterisk (*) in the path name.
    path :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the new IAM customer managed
    -- policy. Each tag consists of a key name and an associated value. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The friendly name of the policy.
    --
    -- IAM user, group, role, and policy names must be unique within the
    -- account. Names are not distinguished by case. For example, you cannot
    -- create resources named both \"MyResource\" and \"myresource\".
    policyName :: Prelude.Text,
    -- | The JSON policy document that you want to use as the content for the new
    -- policy.
    --
    -- You must provide policies in JSON format in IAM. However, for
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. CloudFormation always converts a YAML policy to
    -- JSON format before submitting it to IAM.
    --
    -- The maximum length of the policy document that you can pass in this
    -- operation, including whitespace, is listed below. To view the maximum
    -- character counts of a managed policy with no whitespaces, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
    --
    -- To learn more about JSON policy grammar, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_grammar.html Grammar of the IAM JSON policy language>
    -- in the /IAM User Guide/.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    policyDocument :: Prelude.Text
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
-- 'description', 'createPolicy_description' - A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the
-- policy. For example, \"Grants access to production DynamoDB tables.\"
--
-- The policy description is immutable. After a value is assigned, it
-- cannot be changed.
--
-- 'path', 'createPolicy_path' - The path for the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- You cannot use an asterisk (*) in the path name.
--
-- 'tags', 'createPolicy_tags' - A list of tags that you want to attach to the new IAM customer managed
-- policy. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'policyName', 'createPolicy_policyName' - The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
--
-- 'policyDocument', 'createPolicy_policyDocument' - The JSON policy document that you want to use as the content for the new
-- policy.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- To learn more about JSON policy grammar, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_grammar.html Grammar of the IAM JSON policy language>
-- in the /IAM User Guide/.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
newCreatePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  CreatePolicy
newCreatePolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { description = Prelude.Nothing,
      path = Prelude.Nothing,
      tags = Prelude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the
-- policy. For example, \"Grants access to production DynamoDB tables.\"
--
-- The policy description is immutable. After a value is assigned, it
-- cannot be changed.
createPolicy_description :: Lens.Lens' CreatePolicy (Prelude.Maybe Prelude.Text)
createPolicy_description = Lens.lens (\CreatePolicy' {description} -> description) (\s@CreatePolicy' {} a -> s {description = a} :: CreatePolicy)

-- | The path for the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- You cannot use an asterisk (*) in the path name.
createPolicy_path :: Lens.Lens' CreatePolicy (Prelude.Maybe Prelude.Text)
createPolicy_path = Lens.lens (\CreatePolicy' {path} -> path) (\s@CreatePolicy' {} a -> s {path = a} :: CreatePolicy)

-- | A list of tags that you want to attach to the new IAM customer managed
-- policy. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createPolicy_tags :: Lens.Lens' CreatePolicy (Prelude.Maybe [Tag])
createPolicy_tags = Lens.lens (\CreatePolicy' {tags} -> tags) (\s@CreatePolicy' {} a -> s {tags = a} :: CreatePolicy) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
createPolicy_policyName :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_policyName = Lens.lens (\CreatePolicy' {policyName} -> policyName) (\s@CreatePolicy' {} a -> s {policyName = a} :: CreatePolicy)

-- | The JSON policy document that you want to use as the content for the new
-- policy.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
--
-- To learn more about JSON policy grammar, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_grammar.html Grammar of the IAM JSON policy language>
-- in the /IAM User Guide/.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
createPolicy_policyDocument :: Lens.Lens' CreatePolicy Prelude.Text
createPolicy_policyDocument = Lens.lens (\CreatePolicy' {policyDocument} -> policyDocument) (\s@CreatePolicy' {} a -> s {policyDocument = a} :: CreatePolicy)

instance Core.AWSRequest CreatePolicy where
  type AWSResponse CreatePolicy = CreatePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreatePolicyResult"
      ( \s h x ->
          CreatePolicyResponse'
            Prelude.<$> (x Data..@? "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicy where
  hashWithSalt _salt CreatePolicy' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData CreatePolicy where
  rnf CreatePolicy' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders CreatePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePolicy where
  toQuery CreatePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Description" Data.=: description,
        "Path" Data.=: path,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "PolicyName" Data.=: policyName,
        "PolicyDocument" Data.=: policyDocument
      ]

-- | Contains the response to a successful CreatePolicy request.
--
-- /See:/ 'newCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { -- | A structure containing details about the new policy.
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
-- 'policy', 'createPolicyResponse_policy' - A structure containing details about the new policy.
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

-- | A structure containing details about the new policy.
createPolicyResponse_policy :: Lens.Lens' CreatePolicyResponse (Prelude.Maybe Policy)
createPolicyResponse_policy = Lens.lens (\CreatePolicyResponse' {policy} -> policy) (\s@CreatePolicyResponse' {} a -> s {policy = a} :: CreatePolicyResponse)

-- | The response's http status code.
createPolicyResponse_httpStatus :: Lens.Lens' CreatePolicyResponse Prelude.Int
createPolicyResponse_httpStatus = Lens.lens (\CreatePolicyResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyResponse' {} a -> s {httpStatus = a} :: CreatePolicyResponse)

instance Prelude.NFData CreatePolicyResponse where
  rnf CreatePolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
