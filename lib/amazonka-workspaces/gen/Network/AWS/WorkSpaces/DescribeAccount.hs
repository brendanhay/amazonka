{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.DescribeAccount
  ( -- * Creating a request
    DescribeAccount (..),
    mkDescribeAccount,

    -- * Destructuring the response
    DescribeAccountResponse (..),
    mkDescribeAccountResponse,

    -- ** Response lenses
    darrsDedicatedTenancyManagementCidrRange,
    darrsDedicatedTenancySupport,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccount' value with any optional fields omitted.
mkDescribeAccount ::
  DescribeAccount
mkDescribeAccount = DescribeAccount'

instance Core.FromJSON DescribeAccount where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeAccount where
  type Rs DescribeAccount = DescribeAccountResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.DescribeAccount")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Core.<$> (x Core..:? "DedicatedTenancyManagementCidrRange")
            Core.<*> (x Core..:? "DedicatedTenancySupport")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { -- | The IP address range, specified as an IPv4 CIDR block, used for the management network interface.
    --
    -- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
    dedicatedTenancyManagementCidrRange :: Core.Maybe Types.DedicatedTenancyManagementCidrRange,
    -- | The status of BYOL (whether BYOL is enabled or disabled).
    dedicatedTenancySupport :: Core.Maybe Types.DedicatedTenancySupportResultEnum,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountResponse' value with any optional fields omitted.
mkDescribeAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAccountResponse
mkDescribeAccountResponse responseStatus =
  DescribeAccountResponse'
    { dedicatedTenancyManagementCidrRange =
        Core.Nothing,
      dedicatedTenancySupport = Core.Nothing,
      responseStatus
    }

-- | The IP address range, specified as an IPv4 CIDR block, used for the management network interface.
--
-- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDedicatedTenancyManagementCidrRange :: Lens.Lens' DescribeAccountResponse (Core.Maybe Types.DedicatedTenancyManagementCidrRange)
darrsDedicatedTenancyManagementCidrRange = Lens.field @"dedicatedTenancyManagementCidrRange"
{-# DEPRECATED darrsDedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead." #-}

-- | The status of BYOL (whether BYOL is enabled or disabled).
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsDedicatedTenancySupport :: Lens.Lens' DescribeAccountResponse (Core.Maybe Types.DedicatedTenancySupportResultEnum)
darrsDedicatedTenancySupport = Lens.field @"dedicatedTenancySupport"
{-# DEPRECATED darrsDedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAccountResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
