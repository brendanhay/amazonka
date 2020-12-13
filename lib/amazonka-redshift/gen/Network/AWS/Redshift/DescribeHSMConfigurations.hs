{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHSMConfigurations
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
module Network.AWS.Redshift.DescribeHSMConfigurations
  ( -- * Creating a request
    DescribeHSMConfigurations (..),
    mkDescribeHSMConfigurations,

    -- ** Request lenses
    dhcTagValues,
    dhcHSMConfigurationIdentifier,
    dhcTagKeys,
    dhcMarker,
    dhcMaxRecords,

    -- * Destructuring the response
    DescribeHSMConfigurationsResponse (..),
    mkDescribeHSMConfigurationsResponse,

    -- ** Response lenses
    dhcrsMarker,
    dhcrsHSMConfigurations,
    dhcrsResponseStatus,
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
-- /See:/ 'mkDescribeHSMConfigurations' smart constructor.
data DescribeHSMConfigurations = DescribeHSMConfigurations'
  { -- | A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
    tagValues :: Lude.Maybe [Lude.Text],
    -- | The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
    hsmConfigurationIdentifier :: Lude.Maybe Lude.Text,
    -- | A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
    tagKeys :: Lude.Maybe [Lude.Text],
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMConfigurations' with the minimum fields required to make a request.
--
-- * 'tagValues' - A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
-- * 'hsmConfigurationIdentifier' - The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
-- * 'tagKeys' - A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
mkDescribeHSMConfigurations ::
  DescribeHSMConfigurations
mkDescribeHSMConfigurations =
  DescribeHSMConfigurations'
    { tagValues = Lude.Nothing,
      hsmConfigurationIdentifier = Lude.Nothing,
      tagKeys = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
--
-- /Note:/ Consider using 'tagValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcTagValues :: Lens.Lens' DescribeHSMConfigurations (Lude.Maybe [Lude.Text])
dhcTagValues = Lens.lens (tagValues :: DescribeHSMConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {tagValues = a} :: DescribeHSMConfigurations)
{-# DEPRECATED dhcTagValues "Use generic-lens or generic-optics with 'tagValues' instead." #-}

-- | The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcHSMConfigurationIdentifier :: Lens.Lens' DescribeHSMConfigurations (Lude.Maybe Lude.Text)
dhcHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: DescribeHSMConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: DescribeHSMConfigurations)
{-# DEPRECATED dhcHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcTagKeys :: Lens.Lens' DescribeHSMConfigurations (Lude.Maybe [Lude.Text])
dhcTagKeys = Lens.lens (tagKeys :: DescribeHSMConfigurations -> Lude.Maybe [Lude.Text]) (\s a -> s {tagKeys = a} :: DescribeHSMConfigurations)
{-# DEPRECATED dhcTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcMarker :: Lens.Lens' DescribeHSMConfigurations (Lude.Maybe Lude.Text)
dhcMarker = Lens.lens (marker :: DescribeHSMConfigurations -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeHSMConfigurations)
{-# DEPRECATED dhcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcMaxRecords :: Lens.Lens' DescribeHSMConfigurations (Lude.Maybe Lude.Int)
dhcMaxRecords = Lens.lens (maxRecords :: DescribeHSMConfigurations -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeHSMConfigurations)
{-# DEPRECATED dhcMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeHSMConfigurations where
  page rq rs
    | Page.stop (rs Lens.^. dhcrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dhcrsHSMConfigurations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dhcMarker Lens..~ rs Lens.^. dhcrsMarker

instance Lude.AWSRequest DescribeHSMConfigurations where
  type
    Rs DescribeHSMConfigurations =
      DescribeHSMConfigurationsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeHsmConfigurationsResult"
      ( \s h x ->
          DescribeHSMConfigurationsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "HsmConfigurations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "HsmConfiguration")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHSMConfigurations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeHSMConfigurations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHSMConfigurations where
  toQuery DescribeHSMConfigurations' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeHsmConfigurations" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "TagValues"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagValue" Lude.<$> tagValues),
        "HsmConfigurationIdentifier" Lude.=: hsmConfigurationIdentifier,
        "TagKeys"
          Lude.=: Lude.toQuery (Lude.toQueryList "TagKey" Lude.<$> tagKeys),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeHSMConfigurationsResponse' smart constructor.
data DescribeHSMConfigurationsResponse = DescribeHSMConfigurationsResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of @HsmConfiguration@ objects.
    hsmConfigurations :: Lude.Maybe [HSMConfiguration],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHSMConfigurationsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'hsmConfigurations' - A list of @HsmConfiguration@ objects.
-- * 'responseStatus' - The response status code.
mkDescribeHSMConfigurationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHSMConfigurationsResponse
mkDescribeHSMConfigurationsResponse pResponseStatus_ =
  DescribeHSMConfigurationsResponse'
    { marker = Lude.Nothing,
      hsmConfigurations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrsMarker :: Lens.Lens' DescribeHSMConfigurationsResponse (Lude.Maybe Lude.Text)
dhcrsMarker = Lens.lens (marker :: DescribeHSMConfigurationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeHSMConfigurationsResponse)
{-# DEPRECATED dhcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @HsmConfiguration@ objects.
--
-- /Note:/ Consider using 'hsmConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrsHSMConfigurations :: Lens.Lens' DescribeHSMConfigurationsResponse (Lude.Maybe [HSMConfiguration])
dhcrsHSMConfigurations = Lens.lens (hsmConfigurations :: DescribeHSMConfigurationsResponse -> Lude.Maybe [HSMConfiguration]) (\s a -> s {hsmConfigurations = a} :: DescribeHSMConfigurationsResponse)
{-# DEPRECATED dhcrsHSMConfigurations "Use generic-lens or generic-optics with 'hsmConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhcrsResponseStatus :: Lens.Lens' DescribeHSMConfigurationsResponse Lude.Int
dhcrsResponseStatus = Lens.lens (responseStatus :: DescribeHSMConfigurationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHSMConfigurationsResponse)
{-# DEPRECATED dhcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
