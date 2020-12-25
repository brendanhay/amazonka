{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListLoggingConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'LoggingConfiguration' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListLoggingConfigurations
  ( -- * Creating a request
    ListLoggingConfigurations (..),
    mkListLoggingConfigurations,

    -- ** Request lenses
    llcLimit,
    llcNextMarker,

    -- * Destructuring the response
    ListLoggingConfigurationsResponse (..),
    mkListLoggingConfigurationsResponse,

    -- ** Response lenses
    llcrrsLoggingConfigurations,
    llcrrsNextMarker,
    llcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { -- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
    limit :: Core.Maybe Core.Natural,
    -- | If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggingConfigurations' value with any optional fields omitted.
mkListLoggingConfigurations ::
  ListLoggingConfigurations
mkListLoggingConfigurations =
  ListLoggingConfigurations'
    { limit = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to return for this request. If you have more @LoggingConfigurations@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @LoggingConfigurations@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcLimit :: Lens.Lens' ListLoggingConfigurations (Core.Maybe Core.Natural)
llcLimit = Lens.field @"limit"
{-# DEPRECATED llcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If you specify a value for @Limit@ and you have more @LoggingConfigurations@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @LoggingConfigurations@ . For the second and subsequent @ListLoggingConfigurations@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ListLoggingConfigurations@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcNextMarker :: Lens.Lens' ListLoggingConfigurations (Core.Maybe Types.NextMarker)
llcNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED llcNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromJSON ListLoggingConfigurations where
  toJSON ListLoggingConfigurations {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextMarker" Core..=) Core.<$> nextMarker
          ]
      )

instance Core.AWSRequest ListLoggingConfigurations where
  type
    Rs ListLoggingConfigurations =
      ListLoggingConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.ListLoggingConfigurations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggingConfigurationsResponse'
            Core.<$> (x Core..:? "LoggingConfigurations")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLoggingConfigurations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"loggingConfigurations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextMarker"
            Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { -- | An array of 'LoggingConfiguration' objects.
    loggingConfigurations :: Core.Maybe [Types.LoggingConfiguration],
    -- | If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggingConfigurationsResponse' value with any optional fields omitted.
mkListLoggingConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLoggingConfigurationsResponse
mkListLoggingConfigurationsResponse responseStatus =
  ListLoggingConfigurationsResponse'
    { loggingConfigurations =
        Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | An array of 'LoggingConfiguration' objects.
--
-- /Note:/ Consider using 'loggingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsLoggingConfigurations :: Lens.Lens' ListLoggingConfigurationsResponse (Core.Maybe [Types.LoggingConfiguration])
llcrrsLoggingConfigurations = Lens.field @"loggingConfigurations"
{-# DEPRECATED llcrrsLoggingConfigurations "Use generic-lens or generic-optics with 'loggingConfigurations' instead." #-}

-- | If you have more @LoggingConfigurations@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @LoggingConfigurations@ , submit another @ListLoggingConfigurations@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsNextMarker :: Lens.Lens' ListLoggingConfigurationsResponse (Core.Maybe Types.NextMarker)
llcrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED llcrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llcrrsResponseStatus :: Lens.Lens' ListLoggingConfigurationsResponse Core.Int
llcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
