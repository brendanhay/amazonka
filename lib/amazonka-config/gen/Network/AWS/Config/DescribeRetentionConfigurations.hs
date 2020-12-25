{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeRetentionConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more retention configurations. If the retention configuration name is not specified, this action returns the details for all the retention configurations for that account.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRetentionConfigurations
  ( -- * Creating a request
    DescribeRetentionConfigurations (..),
    mkDescribeRetentionConfigurations,

    -- ** Request lenses
    drcNextToken,
    drcRetentionConfigurationNames,

    -- * Destructuring the response
    DescribeRetentionConfigurationsResponse (..),
    mkDescribeRetentionConfigurationsResponse,

    -- ** Response lenses
    drsNextToken,
    drsRetentionConfigurations,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
    retentionConfigurationNames :: Core.Maybe [Types.RetentionConfigurationName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRetentionConfigurations' value with any optional fields omitted.
mkDescribeRetentionConfigurations ::
  DescribeRetentionConfigurations
mkDescribeRetentionConfigurations =
  DescribeRetentionConfigurations'
    { nextToken = Core.Nothing,
      retentionConfigurationNames = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcNextToken :: Lens.Lens' DescribeRetentionConfigurations (Core.Maybe Types.NextToken)
drcNextToken = Lens.field @"nextToken"
{-# DEPRECATED drcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of names of retention configurations for which you want details. If you do not specify a name, AWS Config returns details for all the retention configurations for that account.
--
-- /Note:/ Consider using 'retentionConfigurationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRetentionConfigurationNames :: Lens.Lens' DescribeRetentionConfigurations (Core.Maybe [Types.RetentionConfigurationName])
drcRetentionConfigurationNames = Lens.field @"retentionConfigurationNames"
{-# DEPRECATED drcRetentionConfigurationNames "Use generic-lens or generic-optics with 'retentionConfigurationNames' instead." #-}

instance Core.FromJSON DescribeRetentionConfigurations where
  toJSON DescribeRetentionConfigurations {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("RetentionConfigurationNames" Core..=)
              Core.<$> retentionConfigurationNames
          ]
      )

instance Core.AWSRequest DescribeRetentionConfigurations where
  type
    Rs DescribeRetentionConfigurations =
      DescribeRetentionConfigurationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.DescribeRetentionConfigurations"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRetentionConfigurationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "RetentionConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeRetentionConfigurations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"retentionConfigurations" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a retention configuration object.
    retentionConfigurations :: Core.Maybe [Types.RetentionConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRetentionConfigurationsResponse' value with any optional fields omitted.
mkDescribeRetentionConfigurationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRetentionConfigurationsResponse
mkDescribeRetentionConfigurationsResponse responseStatus =
  DescribeRetentionConfigurationsResponse'
    { nextToken =
        Core.Nothing,
      retentionConfigurations = Core.Nothing,
      responseStatus
    }

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeRetentionConfigurationsResponse (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a retention configuration object.
--
-- /Note:/ Consider using 'retentionConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRetentionConfigurations :: Lens.Lens' DescribeRetentionConfigurationsResponse (Core.Maybe [Types.RetentionConfiguration])
drsRetentionConfigurations = Lens.field @"retentionConfigurations"
{-# DEPRECATED drsRetentionConfigurations "Use generic-lens or generic-optics with 'retentionConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeRetentionConfigurationsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
