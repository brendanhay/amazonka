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
-- Module      : Amazonka.CloudWatchLogs.DeleteAccountPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudWatch Logs account policy.
--
-- To use this operation, you must be signed on with the
-- @logs:DeleteDataProtectionPolicy@ and @logs:DeleteAccountPolicy@
-- permissions.
module Amazonka.CloudWatchLogs.DeleteAccountPolicy
  ( -- * Creating a Request
    DeleteAccountPolicy (..),
    newDeleteAccountPolicy,

    -- * Request Lenses
    deleteAccountPolicy_policyName,
    deleteAccountPolicy_policyType,

    -- * Destructuring the Response
    DeleteAccountPolicyResponse (..),
    newDeleteAccountPolicyResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountPolicy' smart constructor.
data DeleteAccountPolicy = DeleteAccountPolicy'
  { -- | The name of the policy to delete.
    policyName :: Prelude.Text,
    -- | The type of policy to delete. Currently, the only valid value is
    -- @DATA_PROTECTION_POLICY@.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'deleteAccountPolicy_policyName' - The name of the policy to delete.
--
-- 'policyType', 'deleteAccountPolicy_policyType' - The type of policy to delete. Currently, the only valid value is
-- @DATA_PROTECTION_POLICY@.
newDeleteAccountPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  DeleteAccountPolicy
newDeleteAccountPolicy pPolicyName_ pPolicyType_ =
  DeleteAccountPolicy'
    { policyName = pPolicyName_,
      policyType = pPolicyType_
    }

-- | The name of the policy to delete.
deleteAccountPolicy_policyName :: Lens.Lens' DeleteAccountPolicy Prelude.Text
deleteAccountPolicy_policyName = Lens.lens (\DeleteAccountPolicy' {policyName} -> policyName) (\s@DeleteAccountPolicy' {} a -> s {policyName = a} :: DeleteAccountPolicy)

-- | The type of policy to delete. Currently, the only valid value is
-- @DATA_PROTECTION_POLICY@.
deleteAccountPolicy_policyType :: Lens.Lens' DeleteAccountPolicy PolicyType
deleteAccountPolicy_policyType = Lens.lens (\DeleteAccountPolicy' {policyType} -> policyType) (\s@DeleteAccountPolicy' {} a -> s {policyType = a} :: DeleteAccountPolicy)

instance Core.AWSRequest DeleteAccountPolicy where
  type
    AWSResponse DeleteAccountPolicy =
      DeleteAccountPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteAccountPolicyResponse'

instance Prelude.Hashable DeleteAccountPolicy where
  hashWithSalt _salt DeleteAccountPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData DeleteAccountPolicy where
  rnf DeleteAccountPolicy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyType

instance Data.ToHeaders DeleteAccountPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DeleteAccountPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccountPolicy where
  toJSON DeleteAccountPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("policyName" Data..= policyName),
            Prelude.Just ("policyType" Data..= policyType)
          ]
      )

instance Data.ToPath DeleteAccountPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccountPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountPolicyResponse' smart constructor.
data DeleteAccountPolicyResponse = DeleteAccountPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountPolicyResponse ::
  DeleteAccountPolicyResponse
newDeleteAccountPolicyResponse =
  DeleteAccountPolicyResponse'

instance Prelude.NFData DeleteAccountPolicyResponse where
  rnf _ = ()
