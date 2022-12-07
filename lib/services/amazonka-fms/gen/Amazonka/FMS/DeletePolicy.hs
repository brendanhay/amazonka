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
-- Module      : Amazonka.FMS.DeletePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an Firewall Manager policy.
module Amazonka.FMS.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_deleteAllPolicyResources,
    deletePolicy_policyId,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | If @True@, the request performs cleanup according to the policy type.
    --
    -- For WAF and Shield Advanced policies, the cleanup does the following:
    --
    -- -   Deletes rule groups created by Firewall Manager
    --
    -- -   Removes web ACLs from in-scope resources
    --
    -- -   Deletes web ACLs that contain no rules or rule groups
    --
    -- For security group policies, the cleanup does the following for each
    -- security group in the policy:
    --
    -- -   Disassociates the security group from in-scope resources
    --
    -- -   Deletes the security group if it was created through Firewall
    --     Manager and if it\'s no longer associated with any resources through
    --     another policy
    --
    -- After the cleanup, in-scope resources are no longer protected by web
    -- ACLs in this policy. Protection of out-of-scope resources remains
    -- unchanged. Scope is determined by tags that you create and accounts that
    -- you associate with the policy. When creating the policy, if you specify
    -- that only resources in specific accounts or with specific tags are in
    -- scope of the policy, those accounts and resources are handled by the
    -- policy. All others are out of scope. If you don\'t specify tags or
    -- accounts, all resources are in scope.
    deleteAllPolicyResources :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the policy that you want to delete. You can retrieve this ID
    -- from @PutPolicy@ and @ListPolicies@.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAllPolicyResources', 'deletePolicy_deleteAllPolicyResources' - If @True@, the request performs cleanup according to the policy type.
--
-- For WAF and Shield Advanced policies, the cleanup does the following:
--
-- -   Deletes rule groups created by Firewall Manager
--
-- -   Removes web ACLs from in-scope resources
--
-- -   Deletes web ACLs that contain no rules or rule groups
--
-- For security group policies, the cleanup does the following for each
-- security group in the policy:
--
-- -   Disassociates the security group from in-scope resources
--
-- -   Deletes the security group if it was created through Firewall
--     Manager and if it\'s no longer associated with any resources through
--     another policy
--
-- After the cleanup, in-scope resources are no longer protected by web
-- ACLs in this policy. Protection of out-of-scope resources remains
-- unchanged. Scope is determined by tags that you create and accounts that
-- you associate with the policy. When creating the policy, if you specify
-- that only resources in specific accounts or with specific tags are in
-- scope of the policy, those accounts and resources are handled by the
-- policy. All others are out of scope. If you don\'t specify tags or
-- accounts, all resources are in scope.
--
-- 'policyId', 'deletePolicy_policyId' - The ID of the policy that you want to delete. You can retrieve this ID
-- from @PutPolicy@ and @ListPolicies@.
newDeletePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pPolicyId_ =
  DeletePolicy'
    { deleteAllPolicyResources =
        Prelude.Nothing,
      policyId = pPolicyId_
    }

-- | If @True@, the request performs cleanup according to the policy type.
--
-- For WAF and Shield Advanced policies, the cleanup does the following:
--
-- -   Deletes rule groups created by Firewall Manager
--
-- -   Removes web ACLs from in-scope resources
--
-- -   Deletes web ACLs that contain no rules or rule groups
--
-- For security group policies, the cleanup does the following for each
-- security group in the policy:
--
-- -   Disassociates the security group from in-scope resources
--
-- -   Deletes the security group if it was created through Firewall
--     Manager and if it\'s no longer associated with any resources through
--     another policy
--
-- After the cleanup, in-scope resources are no longer protected by web
-- ACLs in this policy. Protection of out-of-scope resources remains
-- unchanged. Scope is determined by tags that you create and accounts that
-- you associate with the policy. When creating the policy, if you specify
-- that only resources in specific accounts or with specific tags are in
-- scope of the policy, those accounts and resources are handled by the
-- policy. All others are out of scope. If you don\'t specify tags or
-- accounts, all resources are in scope.
deletePolicy_deleteAllPolicyResources :: Lens.Lens' DeletePolicy (Prelude.Maybe Prelude.Bool)
deletePolicy_deleteAllPolicyResources = Lens.lens (\DeletePolicy' {deleteAllPolicyResources} -> deleteAllPolicyResources) (\s@DeletePolicy' {} a -> s {deleteAllPolicyResources = a} :: DeletePolicy)

-- | The ID of the policy that you want to delete. You can retrieve this ID
-- from @PutPolicy@ and @ListPolicies@.
deletePolicy_policyId :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyId = Lens.lens (\DeletePolicy' {policyId} -> policyId) (\s@DeletePolicy' {} a -> s {policyId = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy where
  hashWithSalt _salt DeletePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` deleteAllPolicyResources
      `Prelude.hashWithSalt` policyId

instance Prelude.NFData DeletePolicy where
  rnf DeletePolicy' {..} =
    Prelude.rnf deleteAllPolicyResources
      `Prelude.seq` Prelude.rnf policyId

instance Data.ToHeaders DeletePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DeletePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteAllPolicyResources" Data..=)
              Prelude.<$> deleteAllPolicyResources,
            Prelude.Just ("PolicyId" Data..= policyId)
          ]
      )

instance Data.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Prelude.NFData DeletePolicyResponse where
  rnf _ = ()
