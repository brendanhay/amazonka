{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch configurations.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    (
    -- * Creating a request
      DescribeLaunchConfigurations (..)
    , mkDescribeLaunchConfigurations
    -- ** Request lenses
    , dlcLaunchConfigurationNames
    , dlcMaxRecords
    , dlcNextToken

    -- * Destructuring the response
    , DescribeLaunchConfigurationsResponse (..)
    , mkDescribeLaunchConfigurationsResponse
    -- ** Response lenses
    , dlcrrsLaunchConfigurations
    , dlcrrsNextToken
    , dlcrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLaunchConfigurations' smart constructor.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
  { launchConfigurationNames :: Core.Maybe [Types.ResourceName]
    -- ^ The launch configuration names. If you omit this parameter, all launch configurations are described.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLaunchConfigurations' value with any optional fields omitted.
mkDescribeLaunchConfigurations
    :: DescribeLaunchConfigurations
mkDescribeLaunchConfigurations
  = DescribeLaunchConfigurations'{launchConfigurationNames =
                                    Core.Nothing,
                                  maxRecords = Core.Nothing, nextToken = Core.Nothing}

-- | The launch configuration names. If you omit this parameter, all launch configurations are described.
--
-- /Note:/ Consider using 'launchConfigurationNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcLaunchConfigurationNames :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe [Types.ResourceName])
dlcLaunchConfigurationNames = Lens.field @"launchConfigurationNames"
{-# INLINEABLE dlcLaunchConfigurationNames #-}
{-# DEPRECATED launchConfigurationNames "Use generic-lens or generic-optics with 'launchConfigurationNames' instead"  #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcMaxRecords :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe Core.Int)
dlcMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dlcMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcNextToken :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe Types.XmlString)
dlcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeLaunchConfigurations where
        toQuery DescribeLaunchConfigurations{..}
          = Core.toQueryPair "Action"
              ("DescribeLaunchConfigurations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "LaunchConfigurationNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   launchConfigurationNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeLaunchConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeLaunchConfigurations where
        type Rs DescribeLaunchConfigurations =
             DescribeLaunchConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeLaunchConfigurationsResult"
              (\ s h x ->
                 DescribeLaunchConfigurationsResponse' Core.<$>
                   (x Core..@ "LaunchConfigurations" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeLaunchConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"launchConfigurations") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeLaunchConfigurationsResponse' smart constructor.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
  { launchConfigurations :: [Types.LaunchConfiguration]
    -- ^ The launch configurations.
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLaunchConfigurationsResponse' value with any optional fields omitted.
mkDescribeLaunchConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLaunchConfigurationsResponse
mkDescribeLaunchConfigurationsResponse responseStatus
  = DescribeLaunchConfigurationsResponse'{launchConfigurations =
                                            Core.mempty,
                                          nextToken = Core.Nothing, responseStatus}

-- | The launch configurations.
--
-- /Note:/ Consider using 'launchConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsLaunchConfigurations :: Lens.Lens' DescribeLaunchConfigurationsResponse [Types.LaunchConfiguration]
dlcrrsLaunchConfigurations = Lens.field @"launchConfigurations"
{-# INLINEABLE dlcrrsLaunchConfigurations #-}
{-# DEPRECATED launchConfigurations "Use generic-lens or generic-optics with 'launchConfigurations' instead"  #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsNextToken :: Lens.Lens' DescribeLaunchConfigurationsResponse (Core.Maybe Types.XmlString)
dlcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dlcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcrrsResponseStatus :: Lens.Lens' DescribeLaunchConfigurationsResponse Core.Int
dlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
