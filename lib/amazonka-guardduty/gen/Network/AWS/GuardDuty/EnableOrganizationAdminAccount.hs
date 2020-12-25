{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
  ( -- * Creating a request
    EnableOrganizationAdminAccount (..),
    mkEnableOrganizationAdminAccount,

    -- ** Request lenses
    eoaaAdminAccountId,

    -- * Destructuring the response
    EnableOrganizationAdminAccountResponse (..),
    mkEnableOrganizationAdminAccountResponse,

    -- ** Response lenses
    eoaarrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableOrganizationAdminAccount' smart constructor.
newtype EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
    adminAccountId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableOrganizationAdminAccount' value with any optional fields omitted.
mkEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Types.String ->
  EnableOrganizationAdminAccount
mkEnableOrganizationAdminAccount adminAccountId =
  EnableOrganizationAdminAccount' {adminAccountId}

-- | The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaaAdminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Types.String
eoaaAdminAccountId = Lens.field @"adminAccountId"
{-# DEPRECATED eoaaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

instance Core.FromJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("adminAccountId" Core..= adminAccountId)]
      )

instance Core.AWSRequest EnableOrganizationAdminAccount where
  type
    Rs EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/admin/enable",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableOrganizationAdminAccountResponse' smart constructor.
newtype EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableOrganizationAdminAccountResponse' value with any optional fields omitted.
mkEnableOrganizationAdminAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableOrganizationAdminAccountResponse
mkEnableOrganizationAdminAccountResponse responseStatus =
  EnableOrganizationAdminAccountResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaarrsResponseStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Core.Int
eoaarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eoaarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
