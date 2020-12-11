{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetContextKeysForPrincipalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in all the IAM policies that are attached to the specified IAM entity. The entity can be an IAM user, group, or role. If you specify a user, then the request also includes all of the policies attached to groups that the user is a member of.
--
-- You can optionally include a list of one or more additional policies, specified as strings. If you want to include /only/ a list of policies by string, use 'GetContextKeysForCustomPolicy' instead.
-- __Note:__ This API discloses information about the permissions granted to other users. If you do not want users to see other user's permissions, then consider allowing them to use 'GetContextKeysForCustomPolicy' instead.
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. Context keys can be evaluated by testing against a value in an IAM policy. Use 'GetContextKeysForPrincipalPolicy' to understand what key names and values you must supply when you call 'SimulatePrincipalPolicy' .
module Network.AWS.IAM.GetContextKeysForPrincipalPolicy
  ( -- * Creating a request
    GetContextKeysForPrincipalPolicy (..),
    mkGetContextKeysForPrincipalPolicy,

    -- ** Request lenses
    gckfppPolicyInputList,
    gckfppPolicySourceARN,

    -- * Destructuring the response
    GetContextKeysForPolicyResponse (..),
    mkGetContextKeysForPolicyResponse,

    -- ** Response lenses
    gckfpContextKeyNames,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContextKeysForPrincipalPolicy' smart constructor.
data GetContextKeysForPrincipalPolicy = GetContextKeysForPrincipalPolicy'
  { policyInputList ::
      Lude.Maybe [Lude.Text],
    policySourceARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContextKeysForPrincipalPolicy' with the minimum fields required to make a request.
--
-- * 'policyInputList' - An optional list of additional policies for which you want the list of context keys that are referenced.
--
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
-- * 'policySourceARN' - The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkGetContextKeysForPrincipalPolicy ::
  -- | 'policySourceARN'
  Lude.Text ->
  GetContextKeysForPrincipalPolicy
mkGetContextKeysForPrincipalPolicy pPolicySourceARN_ =
  GetContextKeysForPrincipalPolicy'
    { policyInputList = Lude.Nothing,
      policySourceARN = pPolicySourceARN_
    }

-- | An optional list of additional policies for which you want the list of context keys that are referenced.
--
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
-- /Note:/ Consider using 'policyInputList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gckfppPolicyInputList :: Lens.Lens' GetContextKeysForPrincipalPolicy (Lude.Maybe [Lude.Text])
gckfppPolicyInputList = Lens.lens (policyInputList :: GetContextKeysForPrincipalPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {policyInputList = a} :: GetContextKeysForPrincipalPolicy)
{-# DEPRECATED gckfppPolicyInputList "Use generic-lens or generic-optics with 'policyInputList' instead." #-}

-- | The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policySourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gckfppPolicySourceARN :: Lens.Lens' GetContextKeysForPrincipalPolicy Lude.Text
gckfppPolicySourceARN = Lens.lens (policySourceARN :: GetContextKeysForPrincipalPolicy -> Lude.Text) (\s a -> s {policySourceARN = a} :: GetContextKeysForPrincipalPolicy)
{-# DEPRECATED gckfppPolicySourceARN "Use generic-lens or generic-optics with 'policySourceARN' instead." #-}

instance Lude.AWSRequest GetContextKeysForPrincipalPolicy where
  type
    Rs GetContextKeysForPrincipalPolicy =
      GetContextKeysForPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetContextKeysForPrincipalPolicyResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders GetContextKeysForPrincipalPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetContextKeysForPrincipalPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContextKeysForPrincipalPolicy where
  toQuery GetContextKeysForPrincipalPolicy' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetContextKeysForPrincipalPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyInputList"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyInputList),
        "PolicySourceArn" Lude.=: policySourceARN
      ]
