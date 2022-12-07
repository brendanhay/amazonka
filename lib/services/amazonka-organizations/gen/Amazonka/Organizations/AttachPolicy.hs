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
-- Module      : Amazonka.Organizations.AttachPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to a root, an organizational unit (OU), or an
-- individual account. How the policy affects accounts depends on the type
-- of policy. Refer to the /Organizations User Guide/ for information about
-- each policy type:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.AttachPolicy
  ( -- * Creating a Request
    AttachPolicy (..),
    newAttachPolicy,

    -- * Request Lenses
    attachPolicy_policyId,
    attachPolicy_targetId,

    -- * Destructuring the Response
    AttachPolicyResponse (..),
    newAttachPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The unique identifier (ID) of the policy that you want to attach to the
    -- target. You can get the ID for the policy by calling the ListPolicies
    -- operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Prelude.Text,
    -- | The unique identifier (ID) of the root, OU, or account that you want to
    -- attach the policy to. You can get the ID by calling the ListRoots,
    -- ListOrganizationalUnitsForParent, or ListAccounts operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Account__ - A string that consists of exactly 12 digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    targetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'attachPolicy_policyId' - The unique identifier (ID) of the policy that you want to attach to the
-- target. You can get the ID for the policy by calling the ListPolicies
-- operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
--
-- 'targetId', 'attachPolicy_targetId' - The unique identifier (ID) of the root, OU, or account that you want to
-- attach the policy to. You can get the ID by calling the ListRoots,
-- ListOrganizationalUnitsForParent, or ListAccounts operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
newAttachPolicy ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'targetId'
  Prelude.Text ->
  AttachPolicy
newAttachPolicy pPolicyId_ pTargetId_ =
  AttachPolicy'
    { policyId = pPolicyId_,
      targetId = pTargetId_
    }

-- | The unique identifier (ID) of the policy that you want to attach to the
-- target. You can get the ID for the policy by calling the ListPolicies
-- operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
attachPolicy_policyId :: Lens.Lens' AttachPolicy Prelude.Text
attachPolicy_policyId = Lens.lens (\AttachPolicy' {policyId} -> policyId) (\s@AttachPolicy' {} a -> s {policyId = a} :: AttachPolicy)

-- | The unique identifier (ID) of the root, OU, or account that you want to
-- attach the policy to. You can get the ID by calling the ListRoots,
-- ListOrganizationalUnitsForParent, or ListAccounts operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
attachPolicy_targetId :: Lens.Lens' AttachPolicy Prelude.Text
attachPolicy_targetId = Lens.lens (\AttachPolicy' {targetId} -> targetId) (\s@AttachPolicy' {} a -> s {targetId = a} :: AttachPolicy)

instance Core.AWSRequest AttachPolicy where
  type AWSResponse AttachPolicy = AttachPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull AttachPolicyResponse'

instance Prelude.Hashable AttachPolicy where
  hashWithSalt _salt AttachPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` targetId

instance Prelude.NFData AttachPolicy where
  rnf AttachPolicy' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf targetId

instance Data.ToHeaders AttachPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.AttachPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Data..= policyId),
            Prelude.Just ("TargetId" Data..= targetId)
          ]
      )

instance Data.ToPath AttachPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachPolicyResponse ::
  AttachPolicyResponse
newAttachPolicyResponse = AttachPolicyResponse'

instance Prelude.NFData AttachPolicyResponse where
  rnf _ = ()
