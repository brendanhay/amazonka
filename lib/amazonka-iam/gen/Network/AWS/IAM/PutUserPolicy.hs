{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutUserPolicy (..)
    , mkPutUserPolicy
    -- ** Request lenses
    , pupUserName
    , pupPolicyName
    , pupPolicyDocument

    -- * Destructuring the response
    , PutUserPolicyResponse (..)
    , mkPutUserPolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutUserPolicy' smart constructor.
data PutUserPolicy = PutUserPolicy'
  { userName :: Types.ExistingUserNameType
    -- ^ The name of the user to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyNameType
    -- ^ The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyDocument :: Types.PolicyDocumentType
    -- ^ The policy document.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutUserPolicy' value with any optional fields omitted.
mkPutUserPolicy
    :: Types.ExistingUserNameType -- ^ 'userName'
    -> Types.PolicyNameType -- ^ 'policyName'
    -> Types.PolicyDocumentType -- ^ 'policyDocument'
    -> PutUserPolicy
mkPutUserPolicy userName policyName policyDocument
  = PutUserPolicy'{userName, policyName, policyDocument}

-- | The name of the user to associate the policy with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupUserName :: Lens.Lens' PutUserPolicy Types.ExistingUserNameType
pupUserName = Lens.field @"userName"
{-# INLINEABLE pupUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The name of the policy document.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupPolicyName :: Lens.Lens' PutUserPolicy Types.PolicyNameType
pupPolicyName = Lens.field @"policyName"
{-# INLINEABLE pupPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

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
pupPolicyDocument :: Lens.Lens' PutUserPolicy Types.PolicyDocumentType
pupPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE pupPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

instance Core.ToQuery PutUserPolicy where
        toQuery PutUserPolicy{..}
          = Core.toQueryPair "Action" ("PutUserPolicy" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "PolicyName" policyName
              Core.<> Core.toQueryPair "PolicyDocument" policyDocument

instance Core.ToHeaders PutUserPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutUserPolicy where
        type Rs PutUserPolicy = PutUserPolicyResponse
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
        parseResponse = Response.receiveNull PutUserPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutUserPolicyResponse' smart constructor.
data PutUserPolicyResponse = PutUserPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutUserPolicyResponse' value with any optional fields omitted.
mkPutUserPolicyResponse
    :: PutUserPolicyResponse
mkPutUserPolicyResponse = PutUserPolicyResponse'
