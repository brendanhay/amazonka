{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetContextKeysForCustomPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all of the context keys referenced in the input policies. The policies are supplied as a list of one or more strings. To get the context keys from policies associated with an IAM user, group, or role, use 'GetContextKeysForPrincipalPolicy' .
--
-- Context keys are variables maintained by AWS and its services that provide details about the context of an API query request. Context keys can be evaluated by testing against a value specified in an IAM policy. Use @GetContextKeysForCustomPolicy@ to understand what key names and values you must supply when you call 'SimulateCustomPolicy' . Note that all parameters are shown in unencoded form here for clarity but must be URL encoded to be included as a part of a real HTML request.
module Network.AWS.IAM.GetContextKeysForCustomPolicy
  ( -- * Creating a request
    GetContextKeysForCustomPolicy (..),
    mkGetContextKeysForCustomPolicy,

    -- ** Request lenses
    gckfcpPolicyInputList,

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

-- | /See:/ 'mkGetContextKeysForCustomPolicy' smart constructor.
newtype GetContextKeysForCustomPolicy = GetContextKeysForCustomPolicy'
  { -- | A list of policies for which you want the list of context keys referenced in those policies. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
    policyInputList :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContextKeysForCustomPolicy' with the minimum fields required to make a request.
--
-- * 'policyInputList' - A list of policies for which you want the list of context keys referenced in those policies. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
mkGetContextKeysForCustomPolicy ::
  GetContextKeysForCustomPolicy
mkGetContextKeysForCustomPolicy =
  GetContextKeysForCustomPolicy' {policyInputList = Lude.mempty}

-- | A list of policies for which you want the list of context keys referenced in those policies. Each document is specified as a string containing the complete, valid JSON text of an IAM policy.
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
gckfcpPolicyInputList :: Lens.Lens' GetContextKeysForCustomPolicy [Lude.Text]
gckfcpPolicyInputList = Lens.lens (policyInputList :: GetContextKeysForCustomPolicy -> [Lude.Text]) (\s a -> s {policyInputList = a} :: GetContextKeysForCustomPolicy)
{-# DEPRECATED gckfcpPolicyInputList "Use generic-lens or generic-optics with 'policyInputList' instead." #-}

instance Lude.AWSRequest GetContextKeysForCustomPolicy where
  type
    Rs GetContextKeysForCustomPolicy =
      GetContextKeysForPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetContextKeysForCustomPolicyResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders GetContextKeysForCustomPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetContextKeysForCustomPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContextKeysForCustomPolicy where
  toQuery GetContextKeysForCustomPolicy' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetContextKeysForCustomPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyInputList"
          Lude.=: Lude.toQueryList "member" policyInputList
      ]
