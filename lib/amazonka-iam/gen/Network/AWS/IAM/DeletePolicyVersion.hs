{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeletePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified managed policy.
--
-- You cannot delete the default version from a policy using this API. To delete the default version from a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use 'ListPolicyVersions' .
-- For information about versions for managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeletePolicyVersion
  ( -- * Creating a request
    DeletePolicyVersion (..),
    mkDeletePolicyVersion,

    -- ** Request lenses
    dpvPolicyARN,
    dpvVersionId,

    -- * Destructuring the response
    DeletePolicyVersionResponse (..),
    mkDeletePolicyVersionResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { policyARN ::
      Lude.Text,
    versionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'versionId' - The policy version to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
mkDeletePolicyVersion ::
  -- | 'policyARN'
  Lude.Text ->
  -- | 'versionId'
  Lude.Text ->
  DeletePolicyVersion
mkDeletePolicyVersion pPolicyARN_ pVersionId_ =
  DeletePolicyVersion'
    { policyARN = pPolicyARN_,
      versionId = pVersionId_
    }

-- | The Amazon Resource Name (ARN) of the IAM policy from which you want to delete a version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPolicyARN :: Lens.Lens' DeletePolicyVersion Lude.Text
dpvPolicyARN = Lens.lens (policyARN :: DeletePolicyVersion -> Lude.Text) (\s a -> s {policyARN = a} :: DeletePolicyVersion)
{-# DEPRECATED dpvPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The policy version to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvVersionId :: Lens.Lens' DeletePolicyVersion Lude.Text
dpvVersionId = Lens.lens (versionId :: DeletePolicyVersion -> Lude.Text) (\s a -> s {versionId = a} :: DeletePolicyVersion)
{-# DEPRECATED dpvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Lude.AWSRequest DeletePolicyVersion where
  type Rs DeletePolicyVersion = DeletePolicyVersionResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeletePolicyVersionResponse'

instance Lude.ToHeaders DeletePolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePolicyVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePolicyVersion where
  toQuery DeletePolicyVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePolicyVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyArn" Lude.=: policyARN,
        "VersionId" Lude.=: versionId
      ]

-- | /See:/ 'mkDeletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse = DeletePolicyVersionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyVersionResponse' with the minimum fields required to make a request.
mkDeletePolicyVersionResponse ::
  DeletePolicyVersionResponse
mkDeletePolicyVersionResponse = DeletePolicyVersionResponse'
