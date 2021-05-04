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
-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the
-- specified IAM user.
--
-- An IAM user can also have a managed policy attached to it. To attach a
-- managed policy to a user, use AttachUserPolicy. To create a new managed
-- policy, use CreatePolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- For information about the maximum number of inline policies that you can
-- embed in a user, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutUserPolicy@. For general information about using
-- the Query API with IAM, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making query requests>
-- in the /IAM User Guide/.
module Network.AWS.IAM.PutUserPolicy
  ( -- * Creating a Request
    PutUserPolicy (..),
    newPutUserPolicy,

    -- * Request Lenses
    putUserPolicy_userName,
    putUserPolicy_policyName,
    putUserPolicy_policyDocument,

    -- * Destructuring the Response
    PutUserPolicyResponse (..),
    newPutUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutUserPolicy' smart constructor.
data PutUserPolicy = PutUserPolicy'
  { -- | The name of the user to associate the policy with.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
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
-- Create a value of 'PutUserPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'putUserPolicy_userName' - The name of the user to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'putUserPolicy_policyName' - The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyDocument', 'putUserPolicy_policyDocument' - The policy document.
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
newPutUserPolicy ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutUserPolicy
newPutUserPolicy
  pUserName_
  pPolicyName_
  pPolicyDocument_ =
    PutUserPolicy'
      { userName = pUserName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The name of the user to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
putUserPolicy_userName :: Lens.Lens' PutUserPolicy Prelude.Text
putUserPolicy_userName = Lens.lens (\PutUserPolicy' {userName} -> userName) (\s@PutUserPolicy' {} a -> s {userName = a} :: PutUserPolicy)

-- | The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
putUserPolicy_policyName :: Lens.Lens' PutUserPolicy Prelude.Text
putUserPolicy_policyName = Lens.lens (\PutUserPolicy' {policyName} -> policyName) (\s@PutUserPolicy' {} a -> s {policyName = a} :: PutUserPolicy)

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
putUserPolicy_policyDocument :: Lens.Lens' PutUserPolicy Prelude.Text
putUserPolicy_policyDocument = Lens.lens (\PutUserPolicy' {policyDocument} -> policyDocument) (\s@PutUserPolicy' {} a -> s {policyDocument = a} :: PutUserPolicy)

instance Prelude.AWSRequest PutUserPolicy where
  type Rs PutUserPolicy = PutUserPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull PutUserPolicyResponse'

instance Prelude.Hashable PutUserPolicy

instance Prelude.NFData PutUserPolicy

instance Prelude.ToHeaders PutUserPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath PutUserPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutUserPolicy where
  toQuery PutUserPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("PutUserPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "PolicyName" Prelude.=: policyName,
        "PolicyDocument" Prelude.=: policyDocument
      ]

-- | /See:/ 'newPutUserPolicyResponse' smart constructor.
data PutUserPolicyResponse = PutUserPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutUserPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutUserPolicyResponse ::
  PutUserPolicyResponse
newPutUserPolicyResponse = PutUserPolicyResponse'

instance Prelude.NFData PutUserPolicyResponse
