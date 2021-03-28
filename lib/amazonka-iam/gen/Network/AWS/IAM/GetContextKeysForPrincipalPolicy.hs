{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetContextKeysForPrincipalPolicy (..)
    , mkGetContextKeysForPrincipalPolicy
    -- ** Request lenses
    , gckfppPolicySourceArn
    , gckfppPolicyInputList

     -- * Destructuring the response
    , Types.GetContextKeysForPolicyResponse (..)
    , Types.mkGetContextKeysForPolicyResponse
    -- ** Response lenses
    , Types.gckfprContextKeyNames
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContextKeysForPrincipalPolicy' smart constructor.
data GetContextKeysForPrincipalPolicy = GetContextKeysForPrincipalPolicy'
  { policySourceArn :: Types.PolicySourceArn
    -- ^ The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
  , policyInputList :: Core.Maybe [Types.PolicyDocumentType]
    -- ^ An optional list of additional policies for which you want the list of context keys that are referenced.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetContextKeysForPrincipalPolicy' value with any optional fields omitted.
mkGetContextKeysForPrincipalPolicy
    :: Types.PolicySourceArn -- ^ 'policySourceArn'
    -> GetContextKeysForPrincipalPolicy
mkGetContextKeysForPrincipalPolicy policySourceArn
  = GetContextKeysForPrincipalPolicy'{policySourceArn,
                                      policyInputList = Core.Nothing}

-- | The ARN of a user, group, or role whose policies contain the context keys that you want listed. If you specify a user, the list includes context keys that are found in all policies that are attached to the user. The list also includes all groups that the user is a member of. If you pick a group or a role, then it includes only those context keys that are found in policies attached to that entity. Note that all parameters are shown in unencoded form here for clarity, but must be URL encoded to be included as a part of a real HTML request.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policySourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gckfppPolicySourceArn :: Lens.Lens' GetContextKeysForPrincipalPolicy Types.PolicySourceArn
gckfppPolicySourceArn = Lens.field @"policySourceArn"
{-# INLINEABLE gckfppPolicySourceArn #-}
{-# DEPRECATED policySourceArn "Use generic-lens or generic-optics with 'policySourceArn' instead"  #-}

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
gckfppPolicyInputList :: Lens.Lens' GetContextKeysForPrincipalPolicy (Core.Maybe [Types.PolicyDocumentType])
gckfppPolicyInputList = Lens.field @"policyInputList"
{-# INLINEABLE gckfppPolicyInputList #-}
{-# DEPRECATED policyInputList "Use generic-lens or generic-optics with 'policyInputList' instead"  #-}

instance Core.ToQuery GetContextKeysForPrincipalPolicy where
        toQuery GetContextKeysForPrincipalPolicy{..}
          = Core.toQueryPair "Action"
              ("GetContextKeysForPrincipalPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "PolicySourceArn" policySourceArn
              Core.<>
              Core.toQueryPair "PolicyInputList"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   policyInputList)

instance Core.ToHeaders GetContextKeysForPrincipalPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetContextKeysForPrincipalPolicy where
        type Rs GetContextKeysForPrincipalPolicy =
             Types.GetContextKeysForPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "GetContextKeysForPrincipalPolicyResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}
