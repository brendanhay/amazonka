{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM group.
--
-- A user can also have managed policies attached to it. To attach a managed policy to a group, use 'AttachGroupPolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For information about limits on the number of inline policies that you can embed in a group, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
module Network.AWS.IAM.PutGroupPolicy
  ( -- * Creating a request
    PutGroupPolicy (..),
    mkPutGroupPolicy,

    -- ** Request lenses
    pgpGroupName,
    pgpPolicyName,
    pgpPolicyDocument,

    -- * Destructuring the response
    PutGroupPolicyResponse (..),
    mkPutGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutGroupPolicy' smart constructor.
data PutGroupPolicy = PutGroupPolicy'
  { groupName :: Lude.Text,
    policyName :: Lude.Text,
    policyDocument :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutGroupPolicy' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-.
-- * 'policyDocument' - The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- * 'policyName' - The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkPutGroupPolicy ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  PutGroupPolicy
mkPutGroupPolicy pGroupName_ pPolicyName_ pPolicyDocument_ =
  PutGroupPolicy'
    { groupName = pGroupName_,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | The name of the group to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpGroupName :: Lens.Lens' PutGroupPolicy Lude.Text
pgpGroupName = Lens.lens (groupName :: PutGroupPolicy -> Lude.Text) (\s a -> s {groupName = a} :: PutGroupPolicy)
{-# DEPRECATED pgpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpPolicyName :: Lens.Lens' PutGroupPolicy Lude.Text
pgpPolicyName = Lens.lens (policyName :: PutGroupPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutGroupPolicy)
{-# DEPRECATED pgpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy document.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpPolicyDocument :: Lens.Lens' PutGroupPolicy Lude.Text
pgpPolicyDocument = Lens.lens (policyDocument :: PutGroupPolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: PutGroupPolicy)
{-# DEPRECATED pgpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest PutGroupPolicy where
  type Rs PutGroupPolicy = PutGroupPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull PutGroupPolicyResponse'

instance Lude.ToHeaders PutGroupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutGroupPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutGroupPolicy where
  toQuery PutGroupPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutGroupPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "GroupName" Lude.=: groupName,
        "PolicyName" Lude.=: policyName,
        "PolicyDocument" Lude.=: policyDocument
      ]

-- | /See:/ 'mkPutGroupPolicyResponse' smart constructor.
data PutGroupPolicyResponse = PutGroupPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutGroupPolicyResponse' with the minimum fields required to make a request.
mkPutGroupPolicyResponse ::
  PutGroupPolicyResponse
mkPutGroupPolicyResponse = PutGroupPolicyResponse'
