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
-- Module      : Amazonka.IoT.DeletePolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified policy. You cannot delete
-- the default version of a policy using this action. To delete the default
-- version of a policy, use DeletePolicy. To find out which version of a
-- policy is marked as the default version, use ListPolicyVersions.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeletePolicyVersion>
-- action.
module Amazonka.IoT.DeletePolicyVersion
  ( -- * Creating a Request
    DeletePolicyVersion (..),
    newDeletePolicyVersion,

    -- * Request Lenses
    deletePolicyVersion_policyName,
    deletePolicyVersion_policyVersionId,

    -- * Destructuring the Response
    DeletePolicyVersionResponse (..),
    newDeletePolicyVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeletePolicyVersion operation.
--
-- /See:/ 'newDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The name of the policy.
    policyName :: Prelude.Text,
    -- | The policy version ID.
    policyVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'deletePolicyVersion_policyName' - The name of the policy.
--
-- 'policyVersionId', 'deletePolicyVersion_policyVersionId' - The policy version ID.
newDeletePolicyVersion ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Text ->
  DeletePolicyVersion
newDeletePolicyVersion pPolicyName_ pPolicyVersionId_ =
  DeletePolicyVersion'
    { policyName = pPolicyName_,
      policyVersionId = pPolicyVersionId_
    }

-- | The name of the policy.
deletePolicyVersion_policyName :: Lens.Lens' DeletePolicyVersion Prelude.Text
deletePolicyVersion_policyName = Lens.lens (\DeletePolicyVersion' {policyName} -> policyName) (\s@DeletePolicyVersion' {} a -> s {policyName = a} :: DeletePolicyVersion)

-- | The policy version ID.
deletePolicyVersion_policyVersionId :: Lens.Lens' DeletePolicyVersion Prelude.Text
deletePolicyVersion_policyVersionId = Lens.lens (\DeletePolicyVersion' {policyVersionId} -> policyVersionId) (\s@DeletePolicyVersion' {} a -> s {policyVersionId = a} :: DeletePolicyVersion)

instance Core.AWSRequest DeletePolicyVersion where
  type
    AWSResponse DeletePolicyVersion =
      DeletePolicyVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeletePolicyVersionResponse'

instance Prelude.Hashable DeletePolicyVersion where
  hashWithSalt _salt DeletePolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData DeletePolicyVersion where
  rnf DeletePolicyVersion' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyVersionId

instance Data.ToHeaders DeletePolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeletePolicyVersion where
  toPath DeletePolicyVersion' {..} =
    Prelude.mconcat
      [ "/policies/",
        Data.toBS policyName,
        "/version/",
        Data.toBS policyVersionId
      ]

instance Data.ToQuery DeletePolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
newDeletePolicyVersionResponse =
  DeletePolicyVersionResponse'

instance Prelude.NFData DeletePolicyVersionResponse where
  rnf _ = ()
