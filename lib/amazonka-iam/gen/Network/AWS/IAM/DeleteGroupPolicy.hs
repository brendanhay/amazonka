{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM group.
--
-- A group can also have managed policies attached to it. To detach a managed policy from a group, use 'DetachGroupPolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteGroupPolicy
    (
    -- * Creating a request
      DeleteGroupPolicy (..)
    , mkDeleteGroupPolicy
    -- ** Request lenses
    , dGroupName
    , dPolicyName

    -- * Destructuring the response
    , DeleteGroupPolicyResponse (..)
    , mkDeleteGroupPolicyResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteGroupPolicy' smart constructor.
data DeleteGroupPolicy = DeleteGroupPolicy'
  { groupName :: Types.GroupName
    -- ^ The name (friendly name, not ARN) identifying the group that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , policyName :: Types.PolicyName
    -- ^ The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupPolicy' value with any optional fields omitted.
mkDeleteGroupPolicy
    :: Types.GroupName -- ^ 'groupName'
    -> Types.PolicyName -- ^ 'policyName'
    -> DeleteGroupPolicy
mkDeleteGroupPolicy groupName policyName
  = DeleteGroupPolicy'{groupName, policyName}

-- | The name (friendly name, not ARN) identifying the group that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGroupName :: Lens.Lens' DeleteGroupPolicy Types.GroupName
dGroupName = Lens.field @"groupName"
{-# INLINEABLE dGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DeleteGroupPolicy Types.PolicyName
dPolicyName = Lens.field @"policyName"
{-# INLINEABLE dPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.ToQuery DeleteGroupPolicy where
        toQuery DeleteGroupPolicy{..}
          = Core.toQueryPair "Action" ("DeleteGroupPolicy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "GroupName" groupName
              Core.<> Core.toQueryPair "PolicyName" policyName

instance Core.ToHeaders DeleteGroupPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteGroupPolicy where
        type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
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
        parseResponse = Response.receiveNull DeleteGroupPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteGroupPolicyResponse' value with any optional fields omitted.
mkDeleteGroupPolicyResponse
    :: DeleteGroupPolicyResponse
mkDeleteGroupPolicyResponse = DeleteGroupPolicyResponse'
