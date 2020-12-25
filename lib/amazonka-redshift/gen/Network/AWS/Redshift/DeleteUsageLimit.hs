{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage limit from a cluster.
module Network.AWS.Redshift.DeleteUsageLimit
  ( -- * Creating a request
    DeleteUsageLimit (..),
    mkDeleteUsageLimit,

    -- ** Request lenses
    dulUsageLimitId,

    -- * Destructuring the response
    DeleteUsageLimitResponse (..),
    mkDeleteUsageLimitResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUsageLimit' smart constructor.
newtype DeleteUsageLimit = DeleteUsageLimit'
  { -- | The identifier of the usage limit to delete.
    usageLimitId :: Types.UsageLimitId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsageLimit' value with any optional fields omitted.
mkDeleteUsageLimit ::
  -- | 'usageLimitId'
  Types.UsageLimitId ->
  DeleteUsageLimit
mkDeleteUsageLimit usageLimitId = DeleteUsageLimit' {usageLimitId}

-- | The identifier of the usage limit to delete.
--
-- /Note:/ Consider using 'usageLimitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dulUsageLimitId :: Lens.Lens' DeleteUsageLimit Types.UsageLimitId
dulUsageLimitId = Lens.field @"usageLimitId"
{-# DEPRECATED dulUsageLimitId "Use generic-lens or generic-optics with 'usageLimitId' instead." #-}

instance Core.AWSRequest DeleteUsageLimit where
  type Rs DeleteUsageLimit = DeleteUsageLimitResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteUsageLimit")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "UsageLimitId" usageLimitId)
            )
      }
  response = Response.receiveNull DeleteUsageLimitResponse'

-- | /See:/ 'mkDeleteUsageLimitResponse' smart constructor.
data DeleteUsageLimitResponse = DeleteUsageLimitResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUsageLimitResponse' value with any optional fields omitted.
mkDeleteUsageLimitResponse ::
  DeleteUsageLimitResponse
mkDeleteUsageLimitResponse = DeleteUsageLimitResponse'
