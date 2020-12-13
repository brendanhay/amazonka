{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHSMClientCertificates
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
module Network.AWS.Redshift.DescribeHSMClientCertificates
  ( -- * Creating a request
    DescribeHSMClientCertificates (..),
    mkDescribeHSMClientCertificates,

    -- ** Request lenses
    dhccTagValues,
    dhccTagKeys,
    dhccHSMClientCertificateIdentifier,
    dhccMarker,
    dhccMaxRecords,

    -- * Destructuring the response
    DescribeHSMClientCertificatesResponse (..),
    mkDescribeHSMClientCertificatesResponse,

    -- ** Response lenses
    dhccrsMarker,
    dhccrsHSMClientCertificates,
    dhccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeHSMClientCertificates' smart constructor.
data DescribeHSMClientCertificates = DescribeHSMClientCertificates'
  { -- | A tag value or values for which you want to return all matching HSM client certificates that are associated with the specified tag value or values. For example, suppose that you have HSM client certificates that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | A tag key or keys for which you want to return all matching HSM client certificates that are associated with the specified key or keys. For example, suppose that you have HSM client certificates that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | The identifier of a specific HSM client certificate for which you want information. If no identifier is specified, information is returned for all HSM client certificates owned by your AWS customer account.
    hsmClientCertificateIdentifier :: Lude.Maybe Lude.Text,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmClientCertificates' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMClientCertificates' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching HSM client certificates that are associated with the specified tag value or values. For example, suppose that you have HSM client certificates that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag values associated with them.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching HSM client certificates that are associated with the specified key or keys. For example, suppose that you have HSM client certificates that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag keys associated with them.
-- * 'hsmClientCertificateIdentifier' - The identifier of a specific HSM client certificate for which you want information. If no identifier is specified, information is returned for all HSM client certificates owned by your AWS customer account.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmClientCertificates' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
mkDescribeHSMClientCertificates ::
  DescribeHSMClientCertificates
mkDescribeHSMClientCertificates =
  DescribeHSMClientCertificates'
    { tagValues = Lude.Nothing,
      tagKeys = Lude.Nothing,
      hsmClientCertificateIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching HSM client certificates that are associated with the specified tag value or values. For example, suppose that you have HSM client certificates that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccTagValues :: Lens.Lens' DescribeHSMClientCertificates (Lude.Maybe [Lude.Text])
dhccTagValues = Lens.lens (tagValues :: DescribeHSMClientCertificates -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeHSMClientCertificates)
{-# DEPRECATED dhccTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | A tag key or keys for which you want to return all matching HSM client certificates that are associated with the specified key or keys. For example, suppose that you have HSM client certificates that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM client certificates that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccTagKeys :: Lens.Lens' DescribeHSMClientCertificates (Lude.Maybe [Lude.Text])
dhccTagKeys = Lens.lens (tagKeys :: DescribeHSMClientCertificates -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeHSMClientCertificates)
{-# DEPRECATED dhccTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The identifier of a specific HSM client certificate for which you want information. If no identifier is specified, information is returned for all HSM client certificates owned by your AWS customer account.
--
-- /Note:/ Consider using 'hsmClientCertificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccHSMClientCertificateIdentifier :: Lens.Lens' DescribeHSMClientCertificates (Lude.Maybe Lude.Text)
dhccHSMClientCertificateIdentifier = Lens.lens (hsmClientCertificateIdentifier :: DescribeHSMClientCertificates -> Lude.Maybe Lude.Text) (\s a -> s {hsmClientCertificateIdentifier = a} :: DescribeHSMClientCertificates)
{-# DEPRECATED dhccHSMClientCertificateIdentifier "Use generic-lens or generic-optics with 'hsmClientCertificateIdentifier' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmClientCertificates' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccMarker :: Lens.Lens' DescribeHSMClientCertificates (Lude.Maybe Lude.Text)
dhccMarker = Lens.lens (marker :: DescribeHSMClientCertificates -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeHSMClientCertificates)
{-# DEPRECATED dhccMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccMaxRecords :: Lens.Lens' DescribeHSMClientCertificates (Lude.Maybe Lude.Int)
dhccMaxRecords = Lens.lens (maxRecords :: DescribeHSMClientCertificates -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeHSMClientCertificates)
{-# DEPRECATED dhccMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeHSMClientCertificates where
  page rq rs
    | Page.stop (rs Lens.^. dhccrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dhccrsHSMClientCertificates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dhccMarker Lens..~ rs Lens.^. dhccrsMarker

instance Lude.AWSRequest DescribeHSMClientCertificates where
  type
    Rs DescribeHSMClientCertificates =
      DescribeHSMClientCertificatesResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeHsmClientCertificatesResult"
      ( \s h x ->
          DescribeHSMClientCertificatesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "HsmClientCertificates" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "HsmClientCertificate")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHSMClientCertificates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeHSMClientCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHSMClientCertificates where
  toQuery DescribeHSMClientCertificates' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeHsmClientCertificates" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "HsmClientCertificateIdentifier"
          Lude.=: hsmClientCertificateIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeHSMClientCertificatesResponse' smart constructor.
data DescribeHSMClientCertificatesResponse = DescribeHSMClientCertificatesResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of the identifiers for one or more HSM client certificates used by Amazon Redshift clusters to store and retrieve database encryption keys in an HSM.
    hsmClientCertificates :: Lude.Maybe [HSMClientCertificate],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMClientCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'hsmClientCertificates' - A list of the identifiers for one or more HSM client certificates used by Amazon Redshift clusters to store and retrieve database encryption keys in an HSM.
-- * 'responseStatus' - The response status code.
mkDescribeHSMClientCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHSMClientCertificatesResponse
mkDescribeHSMClientCertificatesResponse pResponseStatus_ =
  DescribeHSMClientCertificatesResponse'
    { marker = Lude.Nothing,
      hsmClientCertificates = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrsMarker :: Lens.Lens' DescribeHSMClientCertificatesResponse (Lude.Maybe Lude.Text)
dhccrsMarker = Lens.lens (marker :: DescribeHSMClientCertificatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeHSMClientCertificatesResponse)
{-# DEPRECATED dhccrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of the identifiers for one or more HSM client certificates used by Amazon Redshift clusters to store and retrieve database encryption keys in an HSM.
--
-- /Note:/ Consider using 'hsmClientCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrsHSMClientCertificates :: Lens.Lens' DescribeHSMClientCertificatesResponse (Lude.Maybe [HSMClientCertificate])
dhccrsHSMClientCertificates = Lens.lens (hsmClientCertificates :: DescribeHSMClientCertificatesResponse -> Lude.Maybe [HSMClientCertificate]) (\s a -> s {hsmClientCertificates = a} :: DescribeHSMClientCertificatesResponse)
{-# DEPRECATED dhccrsHSMClientCertificates "Use generic-lens or generic-optics with 'hsmClientCertificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhccrsResponseStatus :: Lens.Lens' DescribeHSMClientCertificatesResponse Lude.Int
dhccrsResponseStatus = Lens.lens (responseStatus :: DescribeHSMClientCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHSMClientCertificatesResponse)
{-# DEPRECATED dhccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
