{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM user.
--
-- A user can also have managed policies attached to it. To detach a managed policy from a user, use 'DetachUserPolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteUserPolicy
    (
    -- * Creating a request
      DeleteUserPolicy (..)
    , mkDeleteUserPolicy
    -- ** Request lenses
    , dupUserName
    , dupPolicyName

    -- * Destructuring the response
    , DeleteUserPolicyResponse (..)
    , mkDeleteUserPolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserPolicy' smart constructor.
data DeleteUserPolicy = DeleteUserPolicy'
  { userName :: Types.ExistingUserNameType
    -- ^ The name (friendly name, not ARN) identifying the user that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyNameType
    -- ^ The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPolicy' value with any optional fields omitted.
mkDeleteUserPolicy
    :: Types.ExistingUserNameType -- ^ 'userName'
    -> Types.PolicyNameType -- ^ 'policyName'
    -> DeleteUserPolicy
mkDeleteUserPolicy userName policyName
  = DeleteUserPolicy'{userName, policyName}

-- | The name (friendly name, not ARN) identifying the user that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserName :: Lens.Lens' DeleteUserPolicy Types.ExistingUserNameType
dupUserName = Lens.field @"userName"
{-# INLINEABLE dupUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupPolicyName :: Lens.Lens' DeleteUserPolicy Types.PolicyNameType
dupPolicyName = Lens.field @"policyName"
{-# INLINEABLE dupPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeleteUserPolicy where
        toQuery DeleteUserPolicy{..}
          = Core.toQueryPair "Action" ("DeleteUserPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders DeleteUserPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteUserPolicy where
        type Rs DeleteUserPolicy = DeleteUserPolicyResponse
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
        parseResponse = Response.receiveNull DeleteUserPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserPolicyResponse' smart constructor.
data DeleteUserPolicyResponse = DeleteUserPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPolicyResponse' value with any optional fields omitted.
mkDeleteUserPolicyResponse
    :: DeleteUserPolicyResponse
mkDeleteUserPolicyResponse = DeleteUserPolicyResponse'
