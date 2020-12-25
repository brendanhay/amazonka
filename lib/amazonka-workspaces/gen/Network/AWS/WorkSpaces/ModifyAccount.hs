{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.ModifyAccount
  ( -- * Creating a request
    ModifyAccount (..),
    mkModifyAccount,

    -- ** Request lenses
    maDedicatedTenancyManagementCidrRange,
    maDedicatedTenancySupport,

    -- * Destructuring the response
    ModifyAccountResponse (..),
    mkModifyAccountResponse,

    -- ** Response lenses
    marrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { -- | The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
    dedicatedTenancyManagementCidrRange :: Core.Maybe Types.DedicatedTenancyManagementCidrRange,
    -- | The status of BYOL.
    dedicatedTenancySupport :: Core.Maybe Types.DedicatedTenancySupportEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyAccount' value with any optional fields omitted.
mkModifyAccount ::
  ModifyAccount
mkModifyAccount =
  ModifyAccount'
    { dedicatedTenancyManagementCidrRange =
        Core.Nothing,
      dedicatedTenancySupport = Core.Nothing
    }

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDedicatedTenancyManagementCidrRange :: Lens.Lens' ModifyAccount (Core.Maybe Types.DedicatedTenancyManagementCidrRange)
maDedicatedTenancyManagementCidrRange = Lens.field @"dedicatedTenancyManagementCidrRange"
{-# DEPRECATED maDedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead." #-}

-- | The status of BYOL.
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDedicatedTenancySupport :: Lens.Lens' ModifyAccount (Core.Maybe Types.DedicatedTenancySupportEnum)
maDedicatedTenancySupport = Lens.field @"dedicatedTenancySupport"
{-# DEPRECATED maDedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead." #-}

instance Core.FromJSON ModifyAccount where
  toJSON ModifyAccount {..} =
    Core.object
      ( Core.catMaybes
          [ ("DedicatedTenancyManagementCidrRange" Core..=)
              Core.<$> dedicatedTenancyManagementCidrRange,
            ("DedicatedTenancySupport" Core..=)
              Core.<$> dedicatedTenancySupport
          ]
      )

instance Core.AWSRequest ModifyAccount where
  type Rs ModifyAccount = ModifyAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.ModifyAccount")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyAccountResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyAccountResponse' smart constructor.
newtype ModifyAccountResponse = ModifyAccountResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyAccountResponse' value with any optional fields omitted.
mkModifyAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyAccountResponse
mkModifyAccountResponse responseStatus =
  ModifyAccountResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marrsResponseStatus :: Lens.Lens' ModifyAccountResponse Core.Int
marrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED marrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
