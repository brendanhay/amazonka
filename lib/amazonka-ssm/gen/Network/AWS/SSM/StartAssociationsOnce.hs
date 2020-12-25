{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartAssociationsOnce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to run an association immediately and only one time. This action can be helpful when troubleshooting associations.
module Network.AWS.SSM.StartAssociationsOnce
  ( -- * Creating a request
    StartAssociationsOnce (..),
    mkStartAssociationsOnce,

    -- ** Request lenses
    saoAssociationIds,

    -- * Destructuring the response
    StartAssociationsOnceResponse (..),
    mkStartAssociationsOnceResponse,

    -- ** Response lenses
    saorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkStartAssociationsOnce' smart constructor.
newtype StartAssociationsOnce = StartAssociationsOnce'
  { -- | The association IDs that you want to run immediately and only one time.
    associationIds :: Core.NonEmpty Types.AssociationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssociationsOnce' value with any optional fields omitted.
mkStartAssociationsOnce ::
  -- | 'associationIds'
  Core.NonEmpty Types.AssociationId ->
  StartAssociationsOnce
mkStartAssociationsOnce associationIds =
  StartAssociationsOnce' {associationIds}

-- | The association IDs that you want to run immediately and only one time.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saoAssociationIds :: Lens.Lens' StartAssociationsOnce (Core.NonEmpty Types.AssociationId)
saoAssociationIds = Lens.field @"associationIds"
{-# DEPRECATED saoAssociationIds "Use generic-lens or generic-optics with 'associationIds' instead." #-}

instance Core.FromJSON StartAssociationsOnce where
  toJSON StartAssociationsOnce {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AssociationIds" Core..= associationIds)]
      )

instance Core.AWSRequest StartAssociationsOnce where
  type Rs StartAssociationsOnce = StartAssociationsOnceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.StartAssociationsOnce")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartAssociationsOnceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartAssociationsOnceResponse' smart constructor.
newtype StartAssociationsOnceResponse = StartAssociationsOnceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssociationsOnceResponse' value with any optional fields omitted.
mkStartAssociationsOnceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartAssociationsOnceResponse
mkStartAssociationsOnceResponse responseStatus =
  StartAssociationsOnceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saorrsResponseStatus :: Lens.Lens' StartAssociationsOnceResponse Core.Int
saorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED saorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
