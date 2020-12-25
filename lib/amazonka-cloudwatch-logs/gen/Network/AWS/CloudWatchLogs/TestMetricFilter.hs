{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.
module Network.AWS.CloudWatchLogs.TestMetricFilter
  ( -- * Creating a request
    TestMetricFilter (..),
    mkTestMetricFilter,

    -- ** Request lenses
    tmfFilterPattern,
    tmfLogEventMessages,

    -- * Destructuring the response
    TestMetricFilterResponse (..),
    mkTestMetricFilterResponse,

    -- ** Response lenses
    tmfrrsMatches,
    tmfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTestMetricFilter' smart constructor.
data TestMetricFilter = TestMetricFilter'
  { filterPattern :: Types.FilterPattern,
    -- | The log event messages to test.
    logEventMessages :: Core.NonEmpty Types.EventMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestMetricFilter' value with any optional fields omitted.
mkTestMetricFilter ::
  -- | 'filterPattern'
  Types.FilterPattern ->
  -- | 'logEventMessages'
  Core.NonEmpty Types.EventMessage ->
  TestMetricFilter
mkTestMetricFilter filterPattern logEventMessages =
  TestMetricFilter' {filterPattern, logEventMessages}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfFilterPattern :: Lens.Lens' TestMetricFilter Types.FilterPattern
tmfFilterPattern = Lens.field @"filterPattern"
{-# DEPRECATED tmfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | The log event messages to test.
--
-- /Note:/ Consider using 'logEventMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfLogEventMessages :: Lens.Lens' TestMetricFilter (Core.NonEmpty Types.EventMessage)
tmfLogEventMessages = Lens.field @"logEventMessages"
{-# DEPRECATED tmfLogEventMessages "Use generic-lens or generic-optics with 'logEventMessages' instead." #-}

instance Core.FromJSON TestMetricFilter where
  toJSON TestMetricFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("filterPattern" Core..= filterPattern),
            Core.Just ("logEventMessages" Core..= logEventMessages)
          ]
      )

instance Core.AWSRequest TestMetricFilter where
  type Rs TestMetricFilter = TestMetricFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.TestMetricFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TestMetricFilterResponse'
            Core.<$> (x Core..:? "matches") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTestMetricFilterResponse' smart constructor.
data TestMetricFilterResponse = TestMetricFilterResponse'
  { -- | The matched events.
    matches :: Core.Maybe [Types.MetricFilterMatchRecord],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestMetricFilterResponse' value with any optional fields omitted.
mkTestMetricFilterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestMetricFilterResponse
mkTestMetricFilterResponse responseStatus =
  TestMetricFilterResponse' {matches = Core.Nothing, responseStatus}

-- | The matched events.
--
-- /Note:/ Consider using 'matches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrrsMatches :: Lens.Lens' TestMetricFilterResponse (Core.Maybe [Types.MetricFilterMatchRecord])
tmfrrsMatches = Lens.field @"matches"
{-# DEPRECATED tmfrrsMatches "Use generic-lens or generic-optics with 'matches' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfrrsResponseStatus :: Lens.Lens' TestMetricFilterResponse Core.Int
tmfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
