{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter settings for the specified parameter group family.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
  ( -- * Creating a request
    DescribeDefaultClusterParameters (..),
    mkDescribeDefaultClusterParameters,

    -- ** Request lenses
    ddcpParameterGroupFamily,
    ddcpMarker,
    ddcpMaxRecords,

    -- * Destructuring the response
    DescribeDefaultClusterParametersResponse (..),
    mkDescribeDefaultClusterParametersResponse,

    -- ** Response lenses
    ddcprrsDefaultClusterParameters,
    ddcprrsResponseStatus,
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
-- /See:/ 'mkDescribeDefaultClusterParameters' smart constructor.
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
  { -- | The name of the cluster parameter group family.
    parameterGroupFamily :: Types.String,
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultClusterParameters' value with any optional fields omitted.
mkDescribeDefaultClusterParameters ::
  -- | 'parameterGroupFamily'
  Types.String ->
  DescribeDefaultClusterParameters
mkDescribeDefaultClusterParameters parameterGroupFamily =
  DescribeDefaultClusterParameters'
    { parameterGroupFamily,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the cluster parameter group family.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpParameterGroupFamily :: Lens.Lens' DescribeDefaultClusterParameters Types.String
ddcpParameterGroupFamily = Lens.field @"parameterGroupFamily"
{-# DEPRECATED ddcpParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMarker :: Lens.Lens' DescribeDefaultClusterParameters (Core.Maybe Types.String)
ddcpMarker = Lens.field @"marker"
{-# DEPRECATED ddcpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcpMaxRecords :: Lens.Lens' DescribeDefaultClusterParameters (Core.Maybe Core.Int)
ddcpMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddcpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDefaultClusterParameters where
  type
    Rs DescribeDefaultClusterParameters =
      DescribeDefaultClusterParametersResponse
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
            ( Core.pure ("Action", "DescribeDefaultClusterParameters")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ParameterGroupFamily" parameterGroupFamily)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDefaultClusterParametersResult"
      ( \s h x ->
          DescribeDefaultClusterParametersResponse'
            Core.<$> (x Core..@ "DefaultClusterParameters")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDefaultClusterParameters where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^. Lens.field @"defaultClusterParameters" Core.. Lens.field @"marker"
        ) =
      Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"defaultClusterParameters"
              Core.. Lens.field @"parameters"
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs
            Lens.^. Lens.field @"defaultClusterParameters" Core.. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeDefaultClusterParametersResponse' smart constructor.
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
  { defaultClusterParameters :: Types.DefaultClusterParameters,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDefaultClusterParametersResponse' value with any optional fields omitted.
mkDescribeDefaultClusterParametersResponse ::
  -- | 'defaultClusterParameters'
  Types.DefaultClusterParameters ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeDefaultClusterParametersResponse
mkDescribeDefaultClusterParametersResponse
  defaultClusterParameters
  responseStatus =
    DescribeDefaultClusterParametersResponse'
      { defaultClusterParameters,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'defaultClusterParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprrsDefaultClusterParameters :: Lens.Lens' DescribeDefaultClusterParametersResponse Types.DefaultClusterParameters
ddcprrsDefaultClusterParameters = Lens.field @"defaultClusterParameters"
{-# DEPRECATED ddcprrsDefaultClusterParameters "Use generic-lens or generic-optics with 'defaultClusterParameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcprrsResponseStatus :: Lens.Lens' DescribeDefaultClusterParametersResponse Core.Int
ddcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
