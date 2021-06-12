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
-- Module      : Network.AWS.IoT.DeletePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy.
--
-- A policy cannot be deleted if it has non-default versions or it is
-- attached to any certificate.
--
-- To delete a policy, use the DeletePolicyVersion API to delete all
-- non-default versions of the policy; use the DetachPrincipalPolicy API to
-- detach the policy from any certificate; and then use the DeletePolicy
-- API to delete the policy.
--
-- When a policy is deleted using DeletePolicy, its default version is
-- deleted with it.
module Network.AWS.IoT.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_policyName,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeletePolicy operation.
--
-- /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The name of the policy to delete.
    policyName :: Core.Text
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
-- 'policyName', 'deletePolicy_policyName' - The name of the policy to delete.
newDeletePolicy ::
  -- | 'policyName'
  Core.Text ->
  DeletePolicy
newDeletePolicy pPolicyName_ =
  DeletePolicy' {policyName = pPolicyName_}

-- | The name of the policy to delete.
deletePolicy_policyName :: Lens.Lens' DeletePolicy Core.Text
deletePolicy_policyName = Lens.lens (\DeletePolicy' {policyName} -> policyName) (\s@DeletePolicy' {} a -> s {policyName = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeletePolicyResponse'

instance Core.Hashable DeletePolicy

instance Core.NFData DeletePolicy

instance Core.ToHeaders DeletePolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeletePolicy where
  toPath DeletePolicy' {..} =
    Core.mconcat ["/policies/", Core.toBS policyName]

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
