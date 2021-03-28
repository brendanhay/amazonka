{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with your account. AWS IoT Greengrass will use the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. The role must have at least minimum permissions in the policy ''AWSGreengrassResourceAccessRolePolicy''.
module Network.AWS.Greengrass.AssociateServiceRoleToAccount
    (
    -- * Creating a request
      AssociateServiceRoleToAccount (..)
    , mkAssociateServiceRoleToAccount
    -- ** Request lenses
    , asrtaRoleArn

    -- * Destructuring the response
    , AssociateServiceRoleToAccountResponse (..)
    , mkAssociateServiceRoleToAccountResponse
    -- ** Response lenses
    , asrtarrsAssociatedAt
    , asrtarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateServiceRoleToAccount' smart constructor.
newtype AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { roleArn :: Core.Text
    -- ^ The ARN of the service role you wish to associate with your account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateServiceRoleToAccount' value with any optional fields omitted.
mkAssociateServiceRoleToAccount
    :: Core.Text -- ^ 'roleArn'
    -> AssociateServiceRoleToAccount
mkAssociateServiceRoleToAccount roleArn
  = AssociateServiceRoleToAccount'{roleArn}

-- | The ARN of the service role you wish to associate with your account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtaRoleArn :: Lens.Lens' AssociateServiceRoleToAccount Core.Text
asrtaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE asrtaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery AssociateServiceRoleToAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateServiceRoleToAccount where
        toHeaders AssociateServiceRoleToAccount{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateServiceRoleToAccount where
        toJSON AssociateServiceRoleToAccount{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RoleArn" Core..= roleArn)])

instance Core.AWSRequest AssociateServiceRoleToAccount where
        type Rs AssociateServiceRoleToAccount =
             AssociateServiceRoleToAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/greengrass/servicerole",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateServiceRoleToAccountResponse' Core.<$>
                   (x Core..:? "AssociatedAt") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { associatedAt :: Core.Maybe Core.Text
    -- ^ The time when the service role was associated with the account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateServiceRoleToAccountResponse' value with any optional fields omitted.
mkAssociateServiceRoleToAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateServiceRoleToAccountResponse
mkAssociateServiceRoleToAccountResponse responseStatus
  = AssociateServiceRoleToAccountResponse'{associatedAt =
                                             Core.Nothing,
                                           responseStatus}

-- | The time when the service role was associated with the account.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtarrsAssociatedAt :: Lens.Lens' AssociateServiceRoleToAccountResponse (Core.Maybe Core.Text)
asrtarrsAssociatedAt = Lens.field @"associatedAt"
{-# INLINEABLE asrtarrsAssociatedAt #-}
{-# DEPRECATED associatedAt "Use generic-lens or generic-optics with 'associatedAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtarrsResponseStatus :: Lens.Lens' AssociateServiceRoleToAccountResponse Core.Int
asrtarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asrtarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
