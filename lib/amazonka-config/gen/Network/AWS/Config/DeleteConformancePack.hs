{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the AWS Config rules, remediation actions, and all evaluation results within that conformance pack.
--
-- AWS Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the deletion is complete. You cannot update a conformance pack while it is in this state.
module Network.AWS.Config.DeleteConformancePack
  ( -- * Creating a request
    DeleteConformancePack (..),
    mkDeleteConformancePack,

    -- ** Request lenses
    dcpConformancePackName,

    -- * Destructuring the response
    DeleteConformancePackResponse (..),
    mkDeleteConformancePackResponse,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteConformancePack' smart constructor.
newtype DeleteConformancePack = DeleteConformancePack'
  { -- | Name of the conformance pack you want to delete.
    conformancePackName :: Types.ConformancePackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConformancePack' value with any optional fields omitted.
mkDeleteConformancePack ::
  -- | 'conformancePackName'
  Types.ConformancePackName ->
  DeleteConformancePack
mkDeleteConformancePack conformancePackName =
  DeleteConformancePack' {conformancePackName}

-- | Name of the conformance pack you want to delete.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConformancePackName :: Lens.Lens' DeleteConformancePack Types.ConformancePackName
dcpConformancePackName = Lens.field @"conformancePackName"
{-# DEPRECATED dcpConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

instance Core.FromJSON DeleteConformancePack where
  toJSON DeleteConformancePack {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ConformancePackName" Core..= conformancePackName)]
      )

instance Core.AWSRequest DeleteConformancePack where
  type Rs DeleteConformancePack = DeleteConformancePackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.DeleteConformancePack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteConformancePackResponse'

-- | /See:/ 'mkDeleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConformancePackResponse' value with any optional fields omitted.
mkDeleteConformancePackResponse ::
  DeleteConformancePackResponse
mkDeleteConformancePackResponse = DeleteConformancePackResponse'
