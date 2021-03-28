{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Amazon Identity and Access Management (IAM) role that is assigned to the on-premises instance or virtual machines (VM). IAM roles are first assigned to these hybrid instances during the activation process. For more information, see 'CreateActivation' .
module Network.AWS.SSM.UpdateManagedInstanceRole
    (
    -- * Creating a request
      UpdateManagedInstanceRole (..)
    , mkUpdateManagedInstanceRole
    -- ** Request lenses
    , umirInstanceId
    , umirIamRole

    -- * Destructuring the response
    , UpdateManagedInstanceRoleResponse (..)
    , mkUpdateManagedInstanceRoleResponse
    -- ** Response lenses
    , umirrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the managed instance where you want to update the role.
  , iamRole :: Types.IamRole
    -- ^ The IAM role you want to assign or change.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateManagedInstanceRole' value with any optional fields omitted.
mkUpdateManagedInstanceRole
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.IamRole -- ^ 'iamRole'
    -> UpdateManagedInstanceRole
mkUpdateManagedInstanceRole instanceId iamRole
  = UpdateManagedInstanceRole'{instanceId, iamRole}

-- | The ID of the managed instance where you want to update the role.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirInstanceId :: Lens.Lens' UpdateManagedInstanceRole Types.InstanceId
umirInstanceId = Lens.field @"instanceId"
{-# INLINEABLE umirInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The IAM role you want to assign or change.
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirIamRole :: Lens.Lens' UpdateManagedInstanceRole Types.IamRole
umirIamRole = Lens.field @"iamRole"
{-# INLINEABLE umirIamRole #-}
{-# DEPRECATED iamRole "Use generic-lens or generic-optics with 'iamRole' instead"  #-}

instance Core.ToQuery UpdateManagedInstanceRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateManagedInstanceRole where
        toHeaders UpdateManagedInstanceRole{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.UpdateManagedInstanceRole")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateManagedInstanceRole where
        toJSON UpdateManagedInstanceRole{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("IamRole" Core..= iamRole)])

instance Core.AWSRequest UpdateManagedInstanceRole where
        type Rs UpdateManagedInstanceRole =
             UpdateManagedInstanceRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateManagedInstanceRoleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateManagedInstanceRoleResponse' smart constructor.
newtype UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateManagedInstanceRoleResponse' value with any optional fields omitted.
mkUpdateManagedInstanceRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateManagedInstanceRoleResponse
mkUpdateManagedInstanceRoleResponse responseStatus
  = UpdateManagedInstanceRoleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umirrrsResponseStatus :: Lens.Lens' UpdateManagedInstanceRoleResponse Core.Int
umirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
