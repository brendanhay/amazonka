{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSecurityConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all security configurations.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetSecurityConfigurations
  ( -- * Creating a request
    GetSecurityConfigurations (..),
    mkGetSecurityConfigurations,

    -- ** Request lenses
    gscMaxResults,
    gscNextToken,

    -- * Destructuring the response
    GetSecurityConfigurationsResponse (..),
    mkGetSecurityConfigurationsResponse,

    -- ** Response lenses
    gscrfrsNextToken,
    gscrfrsSecurityConfigurations,
    gscrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSecurityConfigurations' smart constructor.
data GetSecurityConfigurations = GetSecurityConfigurations'
  { -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSecurityConfigurations' value with any optional fields omitted.
mkGetSecurityConfigurations ::
  GetSecurityConfigurations
mkGetSecurityConfigurations =
  GetSecurityConfigurations'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscMaxResults :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Core.Natural)
gscMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscNextToken :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Types.GenericString)
gscNextToken = Lens.field @"nextToken"
{-# DEPRECATED gscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetSecurityConfigurations where
  toJSON GetSecurityConfigurations {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetSecurityConfigurations where
  type
    Rs GetSecurityConfigurations =
      GetSecurityConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetSecurityConfigurations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SecurityConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetSecurityConfigurations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"securityConfigurations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetSecurityConfigurationsResponse' smart constructor.
data GetSecurityConfigurationsResponse = GetSecurityConfigurationsResponse'
  { -- | A continuation token, if there are more security configurations to return.
    nextToken :: Core.Maybe Types.GenericString,
    -- | A list of security configurations.
    securityConfigurations :: Core.Maybe [Types.SecurityConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSecurityConfigurationsResponse' value with any optional fields omitted.
mkGetSecurityConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSecurityConfigurationsResponse
mkGetSecurityConfigurationsResponse responseStatus =
  GetSecurityConfigurationsResponse'
    { nextToken = Core.Nothing,
      securityConfigurations = Core.Nothing,
      responseStatus
    }

-- | A continuation token, if there are more security configurations to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsNextToken :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe Types.GenericString)
gscrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gscrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of security configurations.
--
-- /Note:/ Consider using 'securityConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsSecurityConfigurations :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe [Types.SecurityConfiguration])
gscrfrsSecurityConfigurations = Lens.field @"securityConfigurations"
{-# DEPRECATED gscrfrsSecurityConfigurations "Use generic-lens or generic-optics with 'securityConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsResponseStatus :: Lens.Lens' GetSecurityConfigurationsResponse Core.Int
gscrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gscrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
