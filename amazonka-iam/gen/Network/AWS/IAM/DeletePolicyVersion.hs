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
-- Module      : Network.AWS.IAM.DeletePolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified managed policy.
--
-- You cannot delete the default version from a policy using this
-- operation. To delete the default version from a policy, use
-- DeletePolicy. To find out which version of a policy is marked as the
-- default version, use ListPolicyVersions.
--
-- For information about versions for managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeletePolicyVersion
  ( -- * Creating a Request
    DeletePolicyVersion (..),
    newDeletePolicyVersion,

    -- * Request Lenses
    deletePolicyVersion_policyArn,
    deletePolicyVersion_versionId,

    -- * Destructuring the Response
    DeletePolicyVersionResponse (..),
    newDeletePolicyVersionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { -- | The Amazon Resource Name (ARN) of the IAM policy from which you want to
    -- delete a version.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Core.Text,
    -- | The policy version to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consists of the lowercase letter \'v\' followed by one or two
    -- digits, and optionally followed by a period \'.\' and a string of
    -- letters and digits.
    --
    -- For more information about managed policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    versionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'deletePolicyVersion_policyArn' - The Amazon Resource Name (ARN) of the IAM policy from which you want to
-- delete a version.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'versionId', 'deletePolicyVersion_versionId' - The policy version to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consists of the lowercase letter \'v\' followed by one or two
-- digits, and optionally followed by a period \'.\' and a string of
-- letters and digits.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
newDeletePolicyVersion ::
  -- | 'policyArn'
  Core.Text ->
  -- | 'versionId'
  Core.Text ->
  DeletePolicyVersion
newDeletePolicyVersion pPolicyArn_ pVersionId_ =
  DeletePolicyVersion'
    { policyArn = pPolicyArn_,
      versionId = pVersionId_
    }

-- | The Amazon Resource Name (ARN) of the IAM policy from which you want to
-- delete a version.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deletePolicyVersion_policyArn :: Lens.Lens' DeletePolicyVersion Core.Text
deletePolicyVersion_policyArn = Lens.lens (\DeletePolicyVersion' {policyArn} -> policyArn) (\s@DeletePolicyVersion' {} a -> s {policyArn = a} :: DeletePolicyVersion)

-- | The policy version to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consists of the lowercase letter \'v\' followed by one or two
-- digits, and optionally followed by a period \'.\' and a string of
-- letters and digits.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
deletePolicyVersion_versionId :: Lens.Lens' DeletePolicyVersion Core.Text
deletePolicyVersion_versionId = Lens.lens (\DeletePolicyVersion' {versionId} -> versionId) (\s@DeletePolicyVersion' {} a -> s {versionId = a} :: DeletePolicyVersion)

instance Core.AWSRequest DeletePolicyVersion where
  type
    AWSResponse DeletePolicyVersion =
      DeletePolicyVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeletePolicyVersionResponse'

instance Core.Hashable DeletePolicyVersion

instance Core.NFData DeletePolicyVersion

instance Core.ToHeaders DeletePolicyVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeletePolicyVersion where
  toPath = Core.const "/"

instance Core.ToQuery DeletePolicyVersion where
  toQuery DeletePolicyVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeletePolicyVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "PolicyArn" Core.=: policyArn,
        "VersionId" Core.=: versionId
      ]

-- | /See:/ 'newDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
newDeletePolicyVersionResponse =
  DeletePolicyVersionResponse'

instance Core.NFData DeletePolicyVersionResponse
