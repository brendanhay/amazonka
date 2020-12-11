{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
  ( -- * Creating a request
    DescribeOrderableDBInstanceOptions (..),
    mkDescribeOrderableDBInstanceOptions,

    -- ** Request lenses
    dodioEngineVersion,
    dodioAvailabilityZoneGroup,
    dodioFilters,
    dodioDBInstanceClass,
    dodioLicenseModel,
    dodioMarker,
    dodioMaxRecords,
    dodioVPC,
    dodioEngine,

    -- * Destructuring the response
    DescribeOrderableDBInstanceOptionsResponse (..),
    mkDescribeOrderableDBInstanceOptionsResponse,

    -- ** Response lenses
    dodiorsOrderableDBInstanceOptions,
    dodiorsMarker,
    dodiorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeOrderableDBInstanceOptions' smart constructor.
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    availabilityZoneGroup ::
      Lude.Maybe Lude.Text,
    filters ::
      Lude.Maybe [Filter],
    dbInstanceClass ::
      Lude.Maybe Lude.Text,
    licenseModel ::
      Lude.Maybe Lude.Text,
    marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    vpc ::
      Lude.Maybe Lude.Bool,
    engine :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableDBInstanceOptions' with the minimum fields required to make a request.
--
-- * 'availabilityZoneGroup' - The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
--
-- Omit this parameter to show the available offerings in the specified AWS Region.
-- * 'dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
-- * 'engine' - The name of the engine to retrieve DB instance options for.
-- * 'engineVersion' - The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
-- * 'filters' - This parameter isn't currently supported.
-- * 'licenseModel' - The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
-- * 'marker' - An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'vpc' - A value that indicates whether to show only VPC or non-VPC offerings.
mkDescribeOrderableDBInstanceOptions ::
  -- | 'engine'
  Lude.Text ->
  DescribeOrderableDBInstanceOptions
mkDescribeOrderableDBInstanceOptions pEngine_ =
  DescribeOrderableDBInstanceOptions'
    { engineVersion = Lude.Nothing,
      availabilityZoneGroup = Lude.Nothing,
      filters = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      licenseModel = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      vpc = Lude.Nothing,
      engine = pEngine_
    }

-- | The engine version filter value. Specify this parameter to show only the available offerings matching the specified engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioEngineVersion :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Text)
dodioEngineVersion = Lens.lens (engineVersion :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The Availability Zone group associated with a Local Zone. Specify this parameter to retrieve available offerings for the Local Zones in the group.
--
-- Omit this parameter to show the available offerings in the specified AWS Region.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioAvailabilityZoneGroup :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Text)
dodioAvailabilityZoneGroup = Lens.lens (availabilityZoneGroup :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneGroup = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioFilters :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe [Filter])
dodioFilters = Lens.lens (filters :: DescribeOrderableDBInstanceOptions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioDBInstanceClass :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Text)
dodioDBInstanceClass = Lens.lens (dbInstanceClass :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | The license model filter value. Specify this parameter to show only the available offerings matching the specified license model.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioLicenseModel :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Text)
dodioLicenseModel = Lens.lens (licenseModel :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | An optional pagination token provided by a previous DescribeOrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioMarker :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Text)
dodioMarker = Lens.lens (marker :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioMaxRecords :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Int)
dodioMaxRecords = Lens.lens (maxRecords :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to show only VPC or non-VPC offerings.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioVPC :: Lens.Lens' DescribeOrderableDBInstanceOptions (Lude.Maybe Lude.Bool)
dodioVPC = Lens.lens (vpc :: DescribeOrderableDBInstanceOptions -> Lude.Maybe Lude.Bool) (\s a -> s {vpc = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The name of the engine to retrieve DB instance options for.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodioEngine :: Lens.Lens' DescribeOrderableDBInstanceOptions Lude.Text
dodioEngine = Lens.lens (engine :: DescribeOrderableDBInstanceOptions -> Lude.Text) (\s a -> s {engine = a} :: DescribeOrderableDBInstanceOptions)
{-# DEPRECATED dodioEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

instance Page.AWSPager DescribeOrderableDBInstanceOptions where
  page rq rs
    | Page.stop (rs Lens.^. dodiorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dodiorsOrderableDBInstanceOptions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dodioMarker Lens..~ rs Lens.^. dodiorsMarker

instance Lude.AWSRequest DescribeOrderableDBInstanceOptions where
  type
    Rs DescribeOrderableDBInstanceOptions =
      DescribeOrderableDBInstanceOptionsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeOrderableDBInstanceOptionsResult"
      ( \s h x ->
          DescribeOrderableDBInstanceOptionsResponse'
            Lude.<$> ( x Lude..@? "OrderableDBInstanceOptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "OrderableDBInstanceOption")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOrderableDBInstanceOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeOrderableDBInstanceOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeOrderableDBInstanceOptions where
  toQuery DescribeOrderableDBInstanceOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeOrderableDBInstanceOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "AvailabilityZoneGroup" Lude.=: availabilityZoneGroup,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "LicenseModel" Lude.=: licenseModel,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "Vpc" Lude.=: vpc,
        "Engine" Lude.=: engine
      ]

-- | Contains the result of a successful invocation of the @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'mkDescribeOrderableDBInstanceOptionsResponse' smart constructor.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
  { orderableDBInstanceOptions ::
      Lude.Maybe
        [OrderableDBInstanceOption],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOrderableDBInstanceOptionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'orderableDBInstanceOptions' - An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
-- * 'responseStatus' - The response status code.
mkDescribeOrderableDBInstanceOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOrderableDBInstanceOptionsResponse
mkDescribeOrderableDBInstanceOptionsResponse pResponseStatus_ =
  DescribeOrderableDBInstanceOptionsResponse'
    { orderableDBInstanceOptions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An @OrderableDBInstanceOption@ structure containing information about orderable options for the DB instance.
--
-- /Note:/ Consider using 'orderableDBInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodiorsOrderableDBInstanceOptions :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Lude.Maybe [OrderableDBInstanceOption])
dodiorsOrderableDBInstanceOptions = Lens.lens (orderableDBInstanceOptions :: DescribeOrderableDBInstanceOptionsResponse -> Lude.Maybe [OrderableDBInstanceOption]) (\s a -> s {orderableDBInstanceOptions = a} :: DescribeOrderableDBInstanceOptionsResponse)
{-# DEPRECATED dodiorsOrderableDBInstanceOptions "Use generic-lens or generic-optics with 'orderableDBInstanceOptions' instead." #-}

-- | An optional pagination token provided by a previous OrderableDBInstanceOptions request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodiorsMarker :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Lude.Maybe Lude.Text)
dodiorsMarker = Lens.lens (marker :: DescribeOrderableDBInstanceOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeOrderableDBInstanceOptionsResponse)
{-# DEPRECATED dodiorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dodiorsResponseStatus :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse Lude.Int
dodiorsResponseStatus = Lens.lens (responseStatus :: DescribeOrderableDBInstanceOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOrderableDBInstanceOptionsResponse)
{-# DEPRECATED dodiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
