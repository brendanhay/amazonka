{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an AWS account within the Organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.DisableOrganizationAdminAccount
  ( -- * Creating a request
    DisableOrganizationAdminAccount (..),
    mkDisableOrganizationAdminAccount,

    -- ** Request lenses
    doaaAdminAccountId,

    -- * Destructuring the response
    DisableOrganizationAdminAccountResponse (..),
    mkDisableOrganizationAdminAccountResponse,

    -- ** Response lenses
    doaarrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableOrganizationAdminAccount' smart constructor.
newtype DisableOrganizationAdminAccount = DisableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
    adminAccountId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableOrganizationAdminAccount' value with any optional fields omitted.
mkDisableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Types.String ->
  DisableOrganizationAdminAccount
mkDisableOrganizationAdminAccount adminAccountId =
  DisableOrganizationAdminAccount' {adminAccountId}

-- | The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doaaAdminAccountId :: Lens.Lens' DisableOrganizationAdminAccount Types.String
doaaAdminAccountId = Lens.field @"adminAccountId"
{-# DEPRECATED doaaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

instance Core.FromJSON DisableOrganizationAdminAccount where
  toJSON DisableOrganizationAdminAccount {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("adminAccountId" Core..= adminAccountId)]
      )

instance Core.AWSRequest DisableOrganizationAdminAccount where
  type
    Rs DisableOrganizationAdminAccount =
      DisableOrganizationAdminAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/admin/disable",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableOrganizationAdminAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableOrganizationAdminAccountResponse' smart constructor.
newtype DisableOrganizationAdminAccountResponse = DisableOrganizationAdminAccountResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableOrganizationAdminAccountResponse' value with any optional fields omitted.
mkDisableOrganizationAdminAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableOrganizationAdminAccountResponse
mkDisableOrganizationAdminAccountResponse responseStatus =
  DisableOrganizationAdminAccountResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doaarrsResponseStatus :: Lens.Lens' DisableOrganizationAdminAccountResponse Core.Int
doaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED doaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
