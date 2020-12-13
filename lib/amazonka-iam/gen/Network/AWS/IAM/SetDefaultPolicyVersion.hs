{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version.
--
-- This operation affects all users, groups, and roles that the policy is attached to. To list the users, groups, and roles that the policy is attached to, use the 'ListEntitiesForPolicy' API.
-- For information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.SetDefaultPolicyVersion
  ( -- * Creating a request
    SetDefaultPolicyVersion (..),
    mkSetDefaultPolicyVersion,

    -- ** Request lenses
    sdpvVersionId,
    sdpvPolicyARN,

    -- * Destructuring the response
    SetDefaultPolicyVersionResponse (..),
    mkSetDefaultPolicyVersionResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { -- | The version of the policy to set as the default (operative) version.
    --
    -- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
    versionId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDefaultPolicyVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkSetDefaultPolicyVersion ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  SetDefaultPolicyVersion
mkSetDefaultPolicyVersion pVersionId_ pPolicyARN_ =
  SetDefaultPolicyVersion'
    { versionId = pVersionId_,
      policyARN = pPolicyARN_
    }

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvVersionId :: Lens.Lens' SetDefaultPolicyVersion Lude.Text
sdpvVersionId = Lens.lens (versionId :: SetDefaultPolicyVersion -> Lude.Text) (\s a -> s {versionId = a} :: SetDefaultPolicyVersion)
{-# DEPRECATED sdpvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy whose default version you want to set.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyARN :: Lens.Lens' SetDefaultPolicyVersion Lude.Text
sdpvPolicyARN = Lens.lens (policyARN :: SetDefaultPolicyVersion -> Lude.Text) (\s a -> s {policyARN = a} :: SetDefaultPolicyVersion)
{-# DEPRECATED sdpvPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest SetDefaultPolicyVersion where
  type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse
  request = Req.postQuery iamService
  response = Res.receiveNull SetDefaultPolicyVersionResponse'

instance Lude.ToHeaders SetDefaultPolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetDefaultPolicyVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery SetDefaultPolicyVersion where
  toQuery SetDefaultPolicyVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetDefaultPolicyVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "VersionId" Lude.=: versionId,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetDefaultPolicyVersionResponse' with the minimum fields required to make a request.
mkSetDefaultPolicyVersionResponse ::
  SetDefaultPolicyVersionResponse
mkSetDefaultPolicyVersionResponse =
  SetDefaultPolicyVersionResponse'
