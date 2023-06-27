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
-- Module      : Amazonka.IAM.AttachGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM group.
--
-- You use this operation to attach a managed policy to a group. To embed
-- an inline policy in a group, use
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html PutGroupPolicy>
-- .
--
-- As a best practice, you can validate your IAM policies. To learn more,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_policy-validator.html Validating IAM policies>
-- in the /IAM User Guide/.
--
-- For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.AttachGroupPolicy
  ( -- * Creating a Request
    AttachGroupPolicy (..),
    newAttachGroupPolicy,

    -- * Request Lenses
    attachGroupPolicy_groupName,
    attachGroupPolicy_policyArn,

    -- * Destructuring the Response
    AttachGroupPolicyResponse (..),
    newAttachGroupPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachGroupPolicy' smart constructor.
data AttachGroupPolicy = AttachGroupPolicy'
  { -- | The name (friendly name, not ARN) of the group to attach the policy to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'attachGroupPolicy_groupName' - The name (friendly name, not ARN) of the group to attach the policy to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'attachGroupPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newAttachGroupPolicy ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  AttachGroupPolicy
newAttachGroupPolicy pGroupName_ pPolicyArn_ =
  AttachGroupPolicy'
    { groupName = pGroupName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
attachGroupPolicy_groupName :: Lens.Lens' AttachGroupPolicy Prelude.Text
attachGroupPolicy_groupName = Lens.lens (\AttachGroupPolicy' {groupName} -> groupName) (\s@AttachGroupPolicy' {} a -> s {groupName = a} :: AttachGroupPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
attachGroupPolicy_policyArn :: Lens.Lens' AttachGroupPolicy Prelude.Text
attachGroupPolicy_policyArn = Lens.lens (\AttachGroupPolicy' {policyArn} -> policyArn) (\s@AttachGroupPolicy' {} a -> s {policyArn = a} :: AttachGroupPolicy)

instance Core.AWSRequest AttachGroupPolicy where
  type
    AWSResponse AttachGroupPolicy =
      AttachGroupPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AttachGroupPolicyResponse'

instance Prelude.Hashable AttachGroupPolicy where
  hashWithSalt _salt AttachGroupPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData AttachGroupPolicy where
  rnf AttachGroupPolicy' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf policyArn

instance Data.ToHeaders AttachGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachGroupPolicy where
  toQuery AttachGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachGroupPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Data.=: groupName,
        "PolicyArn" Data.=: policyArn
      ]

-- | /See:/ 'newAttachGroupPolicyResponse' smart constructor.
data AttachGroupPolicyResponse = AttachGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachGroupPolicyResponse ::
  AttachGroupPolicyResponse
newAttachGroupPolicyResponse =
  AttachGroupPolicyResponse'

instance Prelude.NFData AttachGroupPolicyResponse where
  rnf _ = ()
