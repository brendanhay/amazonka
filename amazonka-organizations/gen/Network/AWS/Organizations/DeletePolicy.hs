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
-- Module      : Network.AWS.Organizations.DeletePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from your organization. Before you perform
-- this operation, you must first detach the policy from all organizational
-- units (OUs), roots, and accounts.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_policyId,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The unique identifier (ID) of the policy that you want to delete. You
    -- can get the ID from the ListPolicies or ListPoliciesForTarget
    -- operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'deletePolicy_policyId' - The unique identifier (ID) of the policy that you want to delete. You
-- can get the ID from the ListPolicies or ListPoliciesForTarget
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
newDeletePolicy ::
  -- | 'policyId'
  Core.Text ->
  DeletePolicy
newDeletePolicy pPolicyId_ =
  DeletePolicy' {policyId = pPolicyId_}

-- | The unique identifier (ID) of the policy that you want to delete. You
-- can get the ID from the ListPolicies or ListPoliciesForTarget
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
deletePolicy_policyId :: Lens.Lens' DeletePolicy Core.Text
deletePolicy_policyId = Lens.lens (\DeletePolicy' {policyId} -> policyId) (\s@DeletePolicy' {} a -> s {policyId = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeletePolicyResponse'

instance Core.Hashable DeletePolicy

instance Core.NFData DeletePolicy

instance Core.ToHeaders DeletePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DeletePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PolicyId" Core..= policyId)]
      )

instance Core.ToPath DeletePolicy where
  toPath = Core.const "/"

instance Core.ToQuery DeletePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Core.NFData DeletePolicyResponse
