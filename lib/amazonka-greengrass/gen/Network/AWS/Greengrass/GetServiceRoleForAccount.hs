{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetServiceRoleForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the service role that is attached to your account.
module Network.AWS.Greengrass.GetServiceRoleForAccount
    (
    -- * Creating a request
      GetServiceRoleForAccount (..)
    , mkGetServiceRoleForAccount

    -- * Destructuring the response
    , GetServiceRoleForAccountResponse (..)
    , mkGetServiceRoleForAccountResponse
    -- ** Response lenses
    , gsrfarrsAssociatedAt
    , gsrfarrsRoleArn
    , gsrfarrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetServiceRoleForAccount' smart constructor.
data GetServiceRoleForAccount = GetServiceRoleForAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetServiceRoleForAccount' value with any optional fields omitted.
mkGetServiceRoleForAccount
    :: GetServiceRoleForAccount
mkGetServiceRoleForAccount = GetServiceRoleForAccount'

instance Core.ToQuery GetServiceRoleForAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetServiceRoleForAccount where
        toHeaders GetServiceRoleForAccount{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetServiceRoleForAccount where
        type Rs GetServiceRoleForAccount = GetServiceRoleForAccountResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/greengrass/servicerole",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetServiceRoleForAccountResponse' Core.<$>
                   (x Core..:? "AssociatedAt") Core.<*> x Core..:? "RoleArn" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetServiceRoleForAccountResponse' smart constructor.
data GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse'
  { associatedAt :: Core.Maybe Core.Text
    -- ^ The time when the service role was associated with the account.
  , roleArn :: Core.Maybe Core.Text
    -- ^ The ARN of the role which is associated with the account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetServiceRoleForAccountResponse' value with any optional fields omitted.
mkGetServiceRoleForAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetServiceRoleForAccountResponse
mkGetServiceRoleForAccountResponse responseStatus
  = GetServiceRoleForAccountResponse'{associatedAt = Core.Nothing,
                                      roleArn = Core.Nothing, responseStatus}

-- | The time when the service role was associated with the account.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarrsAssociatedAt :: Lens.Lens' GetServiceRoleForAccountResponse (Core.Maybe Core.Text)
gsrfarrsAssociatedAt = Lens.field @"associatedAt"
{-# INLINEABLE gsrfarrsAssociatedAt #-}
{-# DEPRECATED associatedAt "Use generic-lens or generic-optics with 'associatedAt' instead"  #-}

-- | The ARN of the role which is associated with the account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarrsRoleArn :: Lens.Lens' GetServiceRoleForAccountResponse (Core.Maybe Core.Text)
gsrfarrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE gsrfarrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarrsResponseStatus :: Lens.Lens' GetServiceRoleForAccountResponse Core.Int
gsrfarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrfarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
