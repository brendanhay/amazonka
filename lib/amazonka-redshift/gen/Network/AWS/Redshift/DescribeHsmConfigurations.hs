{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHsmConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Amazon Redshift HSM configuration. If no configuration ID is specified, returns information about all the HSM configurations owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM connections that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all HSM connections that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, HSM connections are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeHsmConfigurations
    (
    -- * Creating a request
      DescribeHsmConfigurations (..)
    , mkDescribeHsmConfigurations
    -- ** Request lenses
    , dhcsHsmConfigurationIdentifier
    , dhcsMarker
    , dhcsMaxRecords
    , dhcsTagKeys
    , dhcsTagValues

    -- * Destructuring the response
    , DescribeHsmConfigurationsResponse (..)
    , mkDescribeHsmConfigurationsResponse
    -- ** Response lenses
    , dhcrrsHsmConfigurations
    , dhcrrsMarker
    , dhcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeHsmConfigurations' smart constructor.
data DescribeHsmConfigurations = DescribeHsmConfigurations'
  { hsmConfigurationIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , tagKeys :: Core.Maybe [Core.Text]
    -- ^ A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
  , tagValues :: Core.Maybe [Core.Text]
    -- ^ A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsmConfigurations' value with any optional fields omitted.
mkDescribeHsmConfigurations
    :: DescribeHsmConfigurations
mkDescribeHsmConfigurations
  = DescribeHsmConfigurations'{hsmConfigurationIdentifier =
                                 Core.Nothing,
                               marker = Core.Nothing, maxRecords = Core.Nothing,
                               tagKeys = Core.Nothing, tagValues = Core.Nothing}

-- | The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcsHsmConfigurationIdentifier :: Lens.Lens' DescribeHsmConfigurations (Core.Maybe Core.Text)
dhcsHsmConfigurationIdentifier = Lens.field @"hsmConfigurationIdentifier"
{-# INLINEABLE dhcsHsmConfigurationIdentifier #-}
{-# DEPRECATED hsmConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcsMarker :: Lens.Lens' DescribeHsmConfigurations (Core.Maybe Core.Text)
dhcsMarker = Lens.field @"marker"
{-# INLINEABLE dhcsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcsMaxRecords :: Lens.Lens' DescribeHsmConfigurations (Core.Maybe Core.Int)
dhcsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dhcsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcsTagKeys :: Lens.Lens' DescribeHsmConfigurations (Core.Maybe [Core.Text])
dhcsTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dhcsTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

-- | A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcsTagValues :: Lens.Lens' DescribeHsmConfigurations (Core.Maybe [Core.Text])
dhcsTagValues = Lens.field @"tagValues"
{-# INLINEABLE dhcsTagValues #-}
{-# DEPRECATED tagValues "Use generic-lens or generic-optics with 'tagValues' instead"  #-}

instance Core.ToQuery DescribeHsmConfigurations where
        toQuery DescribeHsmConfigurations{..}
          = Core.toQueryPair "Action"
              ("DescribeHsmConfigurations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmConfigurationIdentifier")
                hsmConfigurationIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.toQueryPair "TagKeys"
                (Core.maybe Core.mempty (Core.toQueryList "TagKey") tagKeys)
              Core.<>
              Core.toQueryPair "TagValues"
                (Core.maybe Core.mempty (Core.toQueryList "TagValue") tagValues)

instance Core.ToHeaders DescribeHsmConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeHsmConfigurations where
        type Rs DescribeHsmConfigurations =
             DescribeHsmConfigurationsResponse
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
          = Response.receiveXMLWrapper "DescribeHsmConfigurationsResult"
              (\ s h x ->
                 DescribeHsmConfigurationsResponse' Core.<$>
                   (x Core..@? "HsmConfigurations" Core..<@>
                      Core.parseXMLList "HsmConfiguration")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeHsmConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"hsmConfigurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeHsmConfigurationsResponse' smart constructor.
data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse'
  { hsmConfigurations :: Core.Maybe [Types.HsmConfiguration]
    -- ^ A list of @HsmConfiguration@ objects.
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsmConfigurationsResponse' value with any optional fields omitted.
mkDescribeHsmConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHsmConfigurationsResponse
mkDescribeHsmConfigurationsResponse responseStatus
  = DescribeHsmConfigurationsResponse'{hsmConfigurations =
                                         Core.Nothing,
                                       marker = Core.Nothing, responseStatus}

-- | A list of @HsmConfiguration@ objects.
--
-- /Note:/ Consider using 'hsmConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsHsmConfigurations :: Lens.Lens' DescribeHsmConfigurationsResponse (Core.Maybe [Types.HsmConfiguration])
dhcrrsHsmConfigurations = Lens.field @"hsmConfigurations"
{-# INLINEABLE dhcrrsHsmConfigurations #-}
{-# DEPRECATED hsmConfigurations "Use generic-lens or generic-optics with 'hsmConfigurations' instead"  #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsMarker :: Lens.Lens' DescribeHsmConfigurationsResponse (Core.Maybe Core.Text)
dhcrrsMarker = Lens.field @"marker"
{-# INLINEABLE dhcrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrrsResponseStatus :: Lens.Lens' DescribeHsmConfigurationsResponse Core.Int
dhcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
