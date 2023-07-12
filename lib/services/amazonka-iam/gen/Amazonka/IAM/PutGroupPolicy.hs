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
-- Module      : Amazonka.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.IAM.PutGroupPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- You must provide policies in JSON format in IAM. However, for
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. CloudFormation always converts a YAML policy to
    -- JSON format before submitting it to = IAM.
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
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to = IAM.
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
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to = IAM.
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

instance Core.AWSRequest PutGroupPolicy where
  type
    AWSResponse PutGroupPolicy =
      PutGroupPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull PutGroupPolicyResponse'

instance Prelude.Hashable PutGroupPolicy where
  hashWithSalt _salt PutGroupPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutGroupPolicy where
  rnf PutGroupPolicy' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders PutGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutGroupPolicy where
  toQuery PutGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutGroupPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Data.=: groupName,
        "PolicyName" Data.=: policyName,
        "PolicyDocument" Data.=: policyDocument
      ]

-- | /See:/ 'newPutGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse = PutGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutGroupPolicyResponse ::
  PutGroupPolicyResponse
newPutGroupPolicyResponse = PutGroupPolicyResponse'

instance Prelude.NFData PutGroupPolicyResponse where
  rnf _ = ()
