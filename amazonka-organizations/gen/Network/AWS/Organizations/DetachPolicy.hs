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
-- Module      : Network.AWS.Organizations.DetachPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from a target root, organizational unit (OU), or
-- account.
--
-- If the policy being detached is a service control policy (SCP), the
-- changes to permissions for AWS Identity and Access Management (IAM)
-- users and roles in affected accounts are immediate.
--
-- Every root, OU, and account must have at least one SCP attached. If you
-- want to replace the default @FullAWSAccess@ policy with an SCP that
-- limits the permissions that can be delegated, you must attach the
-- replacement SCP before you can remove the default SCP. This is the
-- authorization strategy of an
-- \"<https://docs.aws.amazon.com/organizations/latest/userguide/SCP_strategies.html#orgs_policies_allowlist allow list>\".
-- If you instead attach a second SCP and leave the @FullAWSAccess@ SCP
-- still attached, and specify @\"Effect\": \"Deny\"@ in the second SCP to
-- override the @\"Effect\": \"Allow\"@ in the @FullAWSAccess@ policy (or
-- any other attached SCP), you\'re using the authorization strategy of a
-- \"<https://docs.aws.amazon.com/organizations/latest/userguide/SCP_strategies.html#orgs_policies_denylist deny list>\".
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.DetachPolicy
  ( -- * Creating a Request
    DetachPolicy (..),
    newDetachPolicy,

    -- * Request Lenses
    detachPolicy_policyId,
    detachPolicy_targetId,

    -- * Destructuring the Response
    DetachPolicyResponse (..),
    newDetachPolicyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The unique identifier (ID) of the policy you want to detach. You can get
    -- the ID from the ListPolicies or ListPoliciesForTarget operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Prelude.Text,
    -- | The unique identifier (ID) of the root, OU, or account that you want to
    -- detach the policy from. You can get the ID from the ListRoots,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'detachPolicy_policyId' - The unique identifier (ID) of the policy you want to detach. You can get
-- the ID from the ListPolicies or ListPoliciesForTarget operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
--
-- 'targetId', 'detachPolicy_targetId' - The unique identifier (ID) of the root, OU, or account that you want to
-- detach the policy from. You can get the ID from the ListRoots,
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
newDetachPolicy ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'targetId'
  Prelude.Text ->
  DetachPolicy
newDetachPolicy pPolicyId_ pTargetId_ =
  DetachPolicy'
    { policyId = pPolicyId_,
      targetId = pTargetId_
    }

-- | The unique identifier (ID) of the policy you want to detach. You can get
-- the ID from the ListPolicies or ListPoliciesForTarget operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
detachPolicy_policyId :: Lens.Lens' DetachPolicy Prelude.Text
detachPolicy_policyId = Lens.lens (\DetachPolicy' {policyId} -> policyId) (\s@DetachPolicy' {} a -> s {policyId = a} :: DetachPolicy)

-- | The unique identifier (ID) of the root, OU, or account that you want to
-- detach the policy from. You can get the ID from the ListRoots,
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
detachPolicy_targetId :: Lens.Lens' DetachPolicy Prelude.Text
detachPolicy_targetId = Lens.lens (\DetachPolicy' {targetId} -> targetId) (\s@DetachPolicy' {} a -> s {targetId = a} :: DetachPolicy)

instance Prelude.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DetachPolicyResponse'

instance Prelude.Hashable DetachPolicy

instance Prelude.NFData DetachPolicy

instance Prelude.ToHeaders DetachPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.DetachPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Prelude..= policyId),
            Prelude.Just ("TargetId" Prelude..= targetId)
          ]
      )

instance Prelude.ToPath DetachPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachPolicyResponse ::
  DetachPolicyResponse
newDetachPolicyResponse = DetachPolicyResponse'

instance Prelude.NFData DetachPolicyResponse
