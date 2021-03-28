{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHsmClientCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified HSM client certificate. If no certificate ID is specified, returns information about all the HSM certificates owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM client certificates that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all HSM client certificates that have any combination of those values are returned.
-- If both tag keys and values are omitted from the request, HSM client certificates are returned regardless of whether they have tag keys or values associated with them.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeHsmClientCertificates
    (
    -- * Creating a request
      DescribeHsmClientCertificates (..)
    , mkDescribeHsmClientCertificates
    -- ** Request lenses
    , dhccHsmClientCertificateIdentifier
    , dhccMarker
    , dhccMaxRecords
    , dhccTagKeys
    , dhccTagValues

    -- * Destructuring the response
    , DescribeHsmClientCertificatesResponse (..)
    , mkDescribeHsmClientCertificatesResponse
    -- ** Response lenses
    , dhccrrsHsmClientCertificates
    , dhccrrsMarker
    , dhccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeHsmClientCertificates' smart constructor.
data DescribeHsmClientCertificates = DescribeHsmClientCertificates'
  { hsmClientCertificateIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of a specific HSM client certificate for which you want information. If no identifier is specified, information is returned for all HSM client certificates owned by your AWS customer account.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmClientCertificates' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
  , tagKeys :: Core.Maybe [Core.Text]
    -- ^ A tag key or keys for which you want to return all matching HSM client certificates that are associated with the specified key or keys. For example, suppose that you have HSM client certificates that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag keys associated with them.
  , tagValues :: Core.Maybe [Core.Text]
    -- ^ A tag value or values for which you want to return all matching HSM client certificates that are associated with the specified tag value or values. For example, suppose that you have HSM client certificates that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag values associated with them.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsmClientCertificates' value with any optional fields omitted.
mkDescribeHsmClientCertificates
    :: DescribeHsmClientCertificates
mkDescribeHsmClientCertificates
  = DescribeHsmClientCertificates'{hsmClientCertificateIdentifier =
                                     Core.Nothing,
                                   marker = Core.Nothing, maxRecords = Core.Nothing,
                                   tagKeys = Core.Nothing, tagValues = Core.Nothing}

-- | The identifier of a specific HSM client certificate for which you want information. If no identifier is specified, information is returned for all HSM client certificates owned by your AWS customer account.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccHsmClientCertificateIdentifier :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Text)
dhccHsmClientCertificateIdentifier = Lens.field @"hsmClientCertificateIdentifier"
{-# INLINEABLE dhccHsmClientCertificateIdentifier #-}
{-# DEPRECATED hsmClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead"  #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmClientCertificates' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccMarker :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Text)
dhccMarker = Lens.field @"marker"
{-# INLINEABLE dhccMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value. 
--
-- Default: @100@ 
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccMaxRecords :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Int)
dhccMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dhccMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | A tag key or keys for which you want to return all matching HSM client certificates that are associated with the specified key or keys. For example, suppose that you have HSM client certificates that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccTagKeys :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe [Core.Text])
dhccTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE dhccTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

-- | A tag value or values for which you want to return all matching HSM client certificates that are associated with the specified tag value or values. For example, suppose that you have HSM client certificates that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccTagValues :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe [Core.Text])
dhccTagValues = Lens.field @"tagValues"
{-# INLINEABLE dhccTagValues #-}
{-# DEPRECATED tagValues "Use generic-lens or generic-optics with 'tagValues' instead"  #-}

instance Core.ToQuery DescribeHsmClientCertificates where
        toQuery DescribeHsmClientCertificates{..}
          = Core.toQueryPair "Action"
              ("DescribeHsmClientCertificates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "HsmClientCertificateIdentifier")
                hsmClientCertificateIdentifier
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.toQueryPair "TagKeys"
                (Core.maybe Core.mempty (Core.toQueryList "TagKey") tagKeys)
              Core.<>
              Core.toQueryPair "TagValues"
                (Core.maybe Core.mempty (Core.toQueryList "TagValue") tagValues)

instance Core.ToHeaders DescribeHsmClientCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeHsmClientCertificates where
        type Rs DescribeHsmClientCertificates =
             DescribeHsmClientCertificatesResponse
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
          = Response.receiveXMLWrapper "DescribeHsmClientCertificatesResult"
              (\ s h x ->
                 DescribeHsmClientCertificatesResponse' Core.<$>
                   (x Core..@? "HsmClientCertificates" Core..<@>
                      Core.parseXMLList "HsmClientCertificate")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeHsmClientCertificates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"hsmClientCertificates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | 
--
-- /See:/ 'mkDescribeHsmClientCertificatesResponse' smart constructor.
data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse'
  { hsmClientCertificates :: Core.Maybe [Types.HsmClientCertificate]
    -- ^ A list of the identifiers for one or more HSM client certificates used by Amazon Redshift clusters to store and retrieve database encryption keys in an HSM.
  , marker :: Core.Maybe Core.Text
    -- ^ A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHsmClientCertificatesResponse' value with any optional fields omitted.
mkDescribeHsmClientCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeHsmClientCertificatesResponse
mkDescribeHsmClientCertificatesResponse responseStatus
  = DescribeHsmClientCertificatesResponse'{hsmClientCertificates =
                                             Core.Nothing,
                                           marker = Core.Nothing, responseStatus}

-- | A list of the identifiers for one or more HSM client certificates used by Amazon Redshift clusters to store and retrieve database encryption keys in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrrsHsmClientCertificates :: Lens.Lens' DescribeHsmClientCertificatesResponse (Core.Maybe [Types.HsmClientCertificate])
dhccrrsHsmClientCertificates = Lens.field @"hsmClientCertificates"
{-# INLINEABLE dhccrrsHsmClientCertificates #-}
{-# DEPRECATED hsmClientCertificates "Use generic-lens or generic-optics with 'hsmClientCertificates' instead"  #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request. 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrrsMarker :: Lens.Lens' DescribeHsmClientCertificatesResponse (Core.Maybe Core.Text)
dhccrrsMarker = Lens.field @"marker"
{-# INLINEABLE dhccrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrrsResponseStatus :: Lens.Lens' DescribeHsmClientCertificatesResponse Core.Int
dhccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dhccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
