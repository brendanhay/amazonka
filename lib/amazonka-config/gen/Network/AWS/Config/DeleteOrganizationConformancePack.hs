{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteOrganizationConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization conformance pack and all of the config rules and remediation actions from all member accounts in that organization.
--
-- Only a master account or a delegated administrator account can delete an organization conformance pack. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
-- AWS Config sets the state of a conformance pack to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a conformance pack while it is in this state.
module Network.AWS.Config.DeleteOrganizationConformancePack
  ( -- * Creating a request
    DeleteOrganizationConformancePack (..),
    mkDeleteOrganizationConformancePack,

    -- ** Request lenses
    docpOrganizationConformancePackName,

    -- * Destructuring the response
    DeleteOrganizationConformancePackResponse (..),
    mkDeleteOrganizationConformancePackResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOrganizationConformancePack' smart constructor.
newtype DeleteOrganizationConformancePack = DeleteOrganizationConformancePack'
  { -- | The name of organization conformance pack that you want to delete.
    organizationConformancePackName :: Types.OrganizationConformancePackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationConformancePack' value with any optional fields omitted.
mkDeleteOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Types.OrganizationConformancePackName ->
  DeleteOrganizationConformancePack
mkDeleteOrganizationConformancePack organizationConformancePackName =
  DeleteOrganizationConformancePack' {organizationConformancePackName}

-- | The name of organization conformance pack that you want to delete.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
docpOrganizationConformancePackName :: Lens.Lens' DeleteOrganizationConformancePack Types.OrganizationConformancePackName
docpOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# DEPRECATED docpOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

instance Core.FromJSON DeleteOrganizationConformancePack where
  toJSON DeleteOrganizationConformancePack {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OrganizationConformancePackName"
                  Core..= organizationConformancePackName
              )
          ]
      )

instance Core.AWSRequest DeleteOrganizationConformancePack where
  type
    Rs DeleteOrganizationConformancePack =
      DeleteOrganizationConformancePackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DeleteOrganizationConformancePack"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DeleteOrganizationConformancePackResponse'

-- | /See:/ 'mkDeleteOrganizationConformancePackResponse' smart constructor.
data DeleteOrganizationConformancePackResponse = DeleteOrganizationConformancePackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOrganizationConformancePackResponse' value with any optional fields omitted.
mkDeleteOrganizationConformancePackResponse ::
  DeleteOrganizationConformancePackResponse
mkDeleteOrganizationConformancePackResponse =
  DeleteOrganizationConformancePackResponse'
