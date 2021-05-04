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
-- Module      : Network.AWS.IoT.DeletePolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified policy. You cannot delete
-- the default version of a policy using this API. To delete the default
-- version of a policy, use DeletePolicy. To find out which version of a
-- policy is marked as the default version, use ListPolicyVersions.
module Network.AWS.IoT.DeletePolicyVersion
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeletePolicyVersion operation.
--
-- /See:/ 'newDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The name of the policy.
    policyName :: Prelude.Text,
    -- | The policy version ID.
    policyVersionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeletePolicyVersion where
  type
    Rs DeletePolicyVersion =
      DeletePolicyVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeletePolicyVersionResponse'

instance Prelude.Hashable DeletePolicyVersion

instance Prelude.NFData DeletePolicyVersion

instance Prelude.ToHeaders DeletePolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePolicyVersion where
  toPath DeletePolicyVersion' {..} =
    Prelude.mconcat
      [ "/policies/",
        Prelude.toBS policyName,
        "/version/",
        Prelude.toBS policyVersionId
      ]

instance Prelude.ToQuery DeletePolicyVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
newDeletePolicyVersionResponse =
  DeletePolicyVersionResponse'

instance Prelude.NFData DeletePolicyVersionResponse
