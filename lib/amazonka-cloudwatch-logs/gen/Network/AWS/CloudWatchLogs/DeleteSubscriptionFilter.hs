{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription filter.
module Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter
  ( -- * Creating a request
    DeleteSubscriptionFilter (..),
    mkDeleteSubscriptionFilter,

    -- ** Request lenses
    dLogGroupName,
    dFilterName,

    -- * Destructuring the response
    DeleteSubscriptionFilterResponse (..),
    mkDeleteSubscriptionFilterResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSubscriptionFilter' smart constructor.
data DeleteSubscriptionFilter = DeleteSubscriptionFilter'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The name of the subscription filter.
    filterName :: Types.FilterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriptionFilter' value with any optional fields omitted.
mkDeleteSubscriptionFilter ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'filterName'
  Types.FilterName ->
  DeleteSubscriptionFilter
mkDeleteSubscriptionFilter logGroupName filterName =
  DeleteSubscriptionFilter' {logGroupName, filterName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLogGroupName :: Lens.Lens' DeleteSubscriptionFilter Types.LogGroupName
dLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the subscription filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilterName :: Lens.Lens' DeleteSubscriptionFilter Types.FilterName
dFilterName = Lens.field @"filterName"
{-# DEPRECATED dFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Core.FromJSON DeleteSubscriptionFilter where
  toJSON DeleteSubscriptionFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName)
          ]
      )

instance Core.AWSRequest DeleteSubscriptionFilter where
  type Rs DeleteSubscriptionFilter = DeleteSubscriptionFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Logs_20140328.DeleteSubscriptionFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteSubscriptionFilterResponse'

-- | /See:/ 'mkDeleteSubscriptionFilterResponse' smart constructor.
data DeleteSubscriptionFilterResponse = DeleteSubscriptionFilterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubscriptionFilterResponse' value with any optional fields omitted.
mkDeleteSubscriptionFilterResponse ::
  DeleteSubscriptionFilterResponse
mkDeleteSubscriptionFilterResponse =
  DeleteSubscriptionFilterResponse'
