{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the AWS Organizations master account that is associated with AWS Firewall Manager as the AWS Firewall Manager administrator.
module Network.AWS.FMS.GetAdminAccount
    (
    -- * Creating a request
      GetAdminAccount (..)
    , mkGetAdminAccount

    -- * Destructuring the response
    , GetAdminAccountResponse (..)
    , mkGetAdminAccountResponse
    -- ** Response lenses
    , gaarrsAdminAccount
    , gaarrsRoleStatus
    , gaarrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAdminAccount' smart constructor.
data GetAdminAccount = GetAdminAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAdminAccount' value with any optional fields omitted.
mkGetAdminAccount
    :: GetAdminAccount
mkGetAdminAccount = GetAdminAccount'

instance Core.ToQuery GetAdminAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAdminAccount where
        toHeaders GetAdminAccount{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetAdminAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAdminAccount where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAdminAccount where
        type Rs GetAdminAccount = GetAdminAccountResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAdminAccountResponse' Core.<$>
                   (x Core..:? "AdminAccount") Core.<*> x Core..:? "RoleStatus"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { adminAccount :: Core.Maybe Types.AdminAccount
    -- ^ The AWS account that is set as the AWS Firewall Manager administrator.
  , roleStatus :: Core.Maybe Types.AccountRoleStatus
    -- ^ The status of the AWS account that you set as the AWS Firewall Manager administrator.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAdminAccountResponse' value with any optional fields omitted.
mkGetAdminAccountResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAdminAccountResponse
mkGetAdminAccountResponse responseStatus
  = GetAdminAccountResponse'{adminAccount = Core.Nothing,
                             roleStatus = Core.Nothing, responseStatus}

-- | The AWS account that is set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'adminAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsAdminAccount :: Lens.Lens' GetAdminAccountResponse (Core.Maybe Types.AdminAccount)
gaarrsAdminAccount = Lens.field @"adminAccount"
{-# INLINEABLE gaarrsAdminAccount #-}
{-# DEPRECATED adminAccount "Use generic-lens or generic-optics with 'adminAccount' instead"  #-}

-- | The status of the AWS account that you set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'roleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsRoleStatus :: Lens.Lens' GetAdminAccountResponse (Core.Maybe Types.AccountRoleStatus)
gaarrsRoleStatus = Lens.field @"roleStatus"
{-# INLINEABLE gaarrsRoleStatus #-}
{-# DEPRECATED roleStatus "Use generic-lens or generic-optics with 'roleStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsResponseStatus :: Lens.Lens' GetAdminAccountResponse Core.Int
gaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
