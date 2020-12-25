{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.
--
-- You can specify /source/ filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from 'ModifyClusterParameterGroup' , you can specify /source/ equal to /user/ .
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameters
  ( -- * Creating a request
    DescribeClusterParameters (..),
    mkDescribeClusterParameters,

    -- ** Request lenses
    dcpsParameterGroupName,
    dcpsMarker,
    dcpsMaxRecords,
    dcpsSource,

    -- * Destructuring the response
    DescribeClusterParametersResponse (..),
    mkDescribeClusterParametersResponse,

    -- ** Response lenses
    dcprrsMarker,
    dcprrsParameters,
    dcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeClusterParameters' smart constructor.
data DescribeClusterParameters = DescribeClusterParameters'
  { -- | The name of a cluster parameter group for which to return details.
    parameterGroupName :: Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.
    --
    -- Default: All parameter types returned.
    -- Valid Values: @user@ | @engine-default@
    source :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterParameters' value with any optional fields omitted.
mkDescribeClusterParameters ::
  -- | 'parameterGroupName'
  Types.String ->
  DescribeClusterParameters
mkDescribeClusterParameters parameterGroupName =
  DescribeClusterParameters'
    { parameterGroupName,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      source = Core.Nothing
    }

-- | The name of a cluster parameter group for which to return details.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsParameterGroupName :: Lens.Lens' DescribeClusterParameters Types.String
dcpsParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED dcpsParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsMarker :: Lens.Lens' DescribeClusterParameters (Core.Maybe Types.String)
dcpsMarker = Lens.field @"marker"
{-# DEPRECATED dcpsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsMaxRecords :: Lens.Lens' DescribeClusterParameters (Core.Maybe Core.Int)
dcpsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dcpsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
-- Valid Values: @user@ | @engine-default@
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsSource :: Lens.Lens' DescribeClusterParameters (Core.Maybe Types.String)
dcpsSource = Lens.field @"source"
{-# DEPRECATED dcpsSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.AWSRequest DescribeClusterParameters where
  type
    Rs DescribeClusterParameters =
      DescribeClusterParametersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeClusterParameters")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupName" parameterGroupName)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "Source" Core.<$> source)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeClusterParametersResult"
      ( \s h x ->
          DescribeClusterParametersResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "Parameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClusterParameters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"parameters" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the output from the 'DescribeClusterParameters' action.
--
-- /See:/ 'mkDescribeClusterParametersResponse' smart constructor.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
  { -- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
    marker :: Core.Maybe Types.String,
    -- | A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
    parameters :: Core.Maybe [Types.Parameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClusterParametersResponse' value with any optional fields omitted.
mkDescribeClusterParametersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClusterParametersResponse
mkDescribeClusterParametersResponse responseStatus =
  DescribeClusterParametersResponse'
    { marker = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsMarker :: Lens.Lens' DescribeClusterParametersResponse (Core.Maybe Types.String)
dcprrsMarker = Lens.field @"marker"
{-# DEPRECATED dcprrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsParameters :: Lens.Lens' DescribeClusterParametersResponse (Core.Maybe [Types.Parameter])
dcprrsParameters = Lens.field @"parameters"
{-# DEPRECATED dcprrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeClusterParametersResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
