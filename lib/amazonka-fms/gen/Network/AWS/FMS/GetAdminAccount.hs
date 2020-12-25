{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetAdminAccount (..),
    mkGetAdminAccount,

    -- * Destructuring the response
    GetAdminAccountResponse (..),
    mkGetAdminAccountResponse,

    -- ** Response lenses
    gaarrsAdminAccount,
    gaarrsRoleStatus,
    gaarrsResponseStatus,
  )
where

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
mkGetAdminAccount ::
  GetAdminAccount
mkGetAdminAccount = GetAdminAccount'

instance Core.FromJSON GetAdminAccount where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAdminAccount where
  type Rs GetAdminAccount = GetAdminAccountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetAdminAccount")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdminAccountResponse'
            Core.<$> (x Core..:? "AdminAccount")
            Core.<*> (x Core..:? "RoleStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { -- | The AWS account that is set as the AWS Firewall Manager administrator.
    adminAccount :: Core.Maybe Types.AdminAccount,
    -- | The status of the AWS account that you set as the AWS Firewall Manager administrator.
    roleStatus :: Core.Maybe Types.AccountRoleStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAdminAccountResponse' value with any optional fields omitted.
mkGetAdminAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAdminAccountResponse
mkGetAdminAccountResponse responseStatus =
  GetAdminAccountResponse'
    { adminAccount = Core.Nothing,
      roleStatus = Core.Nothing,
      responseStatus
    }

-- | The AWS account that is set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'adminAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsAdminAccount :: Lens.Lens' GetAdminAccountResponse (Core.Maybe Types.AdminAccount)
gaarrsAdminAccount = Lens.field @"adminAccount"
{-# DEPRECATED gaarrsAdminAccount "Use generic-lens or generic-optics with 'adminAccount' instead." #-}

-- | The status of the AWS account that you set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'roleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsRoleStatus :: Lens.Lens' GetAdminAccountResponse (Core.Maybe Types.AccountRoleStatus)
gaarrsRoleStatus = Lens.field @"roleStatus"
{-# DEPRECATED gaarrsRoleStatus "Use generic-lens or generic-optics with 'roleStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarrsResponseStatus :: Lens.Lens' GetAdminAccountResponse Core.Int
gaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
