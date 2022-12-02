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
-- Module      : Amazonka.IoT.DeletePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- To delete a policy, use the DeletePolicyVersion action to delete all
-- non-default versions of the policy; use the DetachPolicy action to
-- detach the policy from any certificate; and then use the DeletePolicy
-- action to delete the policy.
--
-- When a policy is deleted using DeletePolicy, its default version is
-- deleted with it.
--
-- Because of the distributed nature of Amazon Web Services, it can take up
-- to five minutes after a policy is detached before it\'s ready to be
-- deleted.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeletePolicy>
-- action.
module Amazonka.IoT.DeletePolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeletePolicy operation.
--
-- /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The name of the policy to delete.
    policyName :: Prelude.Text
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
-- 'policyName', 'deletePolicy_policyName' - The name of the policy to delete.
newDeletePolicy ::
  -- | 'policyName'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pPolicyName_ =
  DeletePolicy' {policyName = pPolicyName_}

-- | The name of the policy to delete.
deletePolicy_policyName :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyName = Lens.lens (\DeletePolicy' {policyName} -> policyName) (\s@DeletePolicy' {} a -> s {policyName = a} :: DeletePolicy)

instance Core.AWSRequest DeletePolicy where
  type AWSResponse DeletePolicy = DeletePolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy where
  hashWithSalt _salt DeletePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeletePolicy where
  rnf DeletePolicy' {..} = Prelude.rnf policyName

instance Data.ToHeaders DeletePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePolicy where
  toPath DeletePolicy' {..} =
    Prelude.mconcat
      ["/policies/", Data.toBS policyName]

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
