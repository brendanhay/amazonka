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
-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the
-- specified IAM group.
--
-- A user can also have managed policies attached to it. To attach a
-- managed policy to a group, use AttachGroupPolicy. To create a new
-- managed policy, use CreatePolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- For information about the maximum number of inline policies that you can
-- embed in a group, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutGroupPolicy@. For general information about using
-- the Query API with IAM, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making query requests>
-- in the /IAM User Guide/.
module Network.AWS.IAM.PutGroupPolicy
  ( -- * Creating a Request
    PutGroupPolicy (..),
    newPutGroupPolicy,

    -- * Request Lenses
    putGroupPolicy_groupName,
    putGroupPolicy_policyName,
    putGroupPolicy_policyDocument,

    -- * Destructuring the Response
    PutGroupPolicyResponse (..),
    newPutGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutGroupPolicy' smart constructor.
data PutGroupPolicy = PutGroupPolicy'
  { -- | The name of the group to associate the policy with.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-.
    groupName :: Prelude.Text,
    -- | The name of the policy document.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyName :: Prelude.Text,
    -- | The policy document.
    --
    -- You must provide policies in JSON format in IAM. However, for AWS
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
    -- to JSON format before submitting it to IAM.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'putGroupPolicy_groupName' - The name of the group to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-.
--
-- 'policyName', 'putGroupPolicy_policyName' - The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyDocument', 'putGroupPolicy_policyDocument' - The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
-- to JSON format before submitting it to IAM.
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
newPutGroupPolicy ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutGroupPolicy
newPutGroupPolicy
  pGroupName_
  pPolicyName_
  pPolicyDocument_ =
    PutGroupPolicy'
      { groupName = pGroupName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The name of the group to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-.
putGroupPolicy_groupName :: Lens.Lens' PutGroupPolicy Prelude.Text
putGroupPolicy_groupName = Lens.lens (\PutGroupPolicy' {groupName} -> groupName) (\s@PutGroupPolicy' {} a -> s {groupName = a} :: PutGroupPolicy)

-- | The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
putGroupPolicy_policyName :: Lens.Lens' PutGroupPolicy Prelude.Text
putGroupPolicy_policyName = Lens.lens (\PutGroupPolicy' {policyName} -> policyName) (\s@PutGroupPolicy' {} a -> s {policyName = a} :: PutGroupPolicy)

-- | The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
-- to JSON format before submitting it to IAM.
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
putGroupPolicy_policyDocument :: Lens.Lens' PutGroupPolicy Prelude.Text
putGroupPolicy_policyDocument = Lens.lens (\PutGroupPolicy' {policyDocument} -> policyDocument) (\s@PutGroupPolicy' {} a -> s {policyDocument = a} :: PutGroupPolicy)

instance Prelude.AWSRequest PutGroupPolicy where
  type Rs PutGroupPolicy = PutGroupPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull PutGroupPolicyResponse'

instance Prelude.Hashable PutGroupPolicy

instance Prelude.NFData PutGroupPolicy

instance Prelude.ToHeaders PutGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutGroupPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutGroupPolicy where
  toQuery PutGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutGroupPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Prelude.=: groupName,
        "PolicyName" Prelude.=: policyName,
        "PolicyDocument" Prelude.=: policyDocument
      ]

-- | /See:/ 'newPutGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse = PutGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutGroupPolicyResponse ::
  PutGroupPolicyResponse
newPutGroupPolicyResponse = PutGroupPolicyResponse'

instance Prelude.NFData PutGroupPolicyResponse
