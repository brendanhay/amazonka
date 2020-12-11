{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the specified IAM user.
--
-- An IAM user can also have a managed policy attached to it. To attach a managed policy to a user, use 'AttachUserPolicy' . To create a new managed policy, use 'CreatePolicy' . For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For information about limits on the number of inline policies that you can embed in a user, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
module Network.AWS.IAM.PutUserPolicy
  ( -- * Creating a request
    PutUserPolicy (..),
    mkPutUserPolicy,

    -- ** Request lenses
    pupUserName,
    pupPolicyName,
    pupPolicyDocument,

    -- * Destructuring the response
    PutUserPolicyResponse (..),
    mkPutUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutUserPolicy' smart constructor.
data PutUserPolicy = PutUserPolicy'
  { userName :: Lude.Text,
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

-- | Creates a value of 'PutUserPolicy' with the minimum fields required to make a request.
--
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
-- * 'userName' - The name of the user to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkPutUserPolicy ::
  -- | 'userName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  PutUserPolicy
mkPutUserPolicy pUserName_ pPolicyName_ pPolicyDocument_ =
  PutUserPolicy'
    { userName = pUserName_,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | The name of the user to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupUserName :: Lens.Lens' PutUserPolicy Lude.Text
pupUserName = Lens.lens (userName :: PutUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: PutUserPolicy)
{-# DEPRECATED pupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupPolicyName :: Lens.Lens' PutUserPolicy Lude.Text
pupPolicyName = Lens.lens (policyName :: PutUserPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutUserPolicy)
{-# DEPRECATED pupPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

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
pupPolicyDocument :: Lens.Lens' PutUserPolicy Lude.Text
pupPolicyDocument = Lens.lens (policyDocument :: PutUserPolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: PutUserPolicy)
{-# DEPRECATED pupPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest PutUserPolicy where
  type Rs PutUserPolicy = PutUserPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull PutUserPolicyResponse'

instance Lude.ToHeaders PutUserPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutUserPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutUserPolicy where
  toQuery PutUserPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutUserPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "PolicyName" Lude.=: policyName,
        "PolicyDocument" Lude.=: policyDocument
      ]

-- | /See:/ 'mkPutUserPolicyResponse' smart constructor.
data PutUserPolicyResponse = PutUserPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutUserPolicyResponse' with the minimum fields required to make a request.
mkPutUserPolicyResponse ::
  PutUserPolicyResponse
mkPutUserPolicyResponse = PutUserPolicyResponse'
