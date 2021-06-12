{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a detailed list of parameters contained within the specified
-- Amazon Redshift parameter group. For each parameter the response
-- includes information such as parameter name, description, data type,
-- value, whether the parameter value is modifiable, and so on.
--
-- You can specify /source/ filter to retrieve parameters of only specific
-- type. For example, to retrieve parameters that were modified by a user
-- action such as from ModifyClusterParameterGroup, you can specify
-- /source/ equal to /user/.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameters
  ( -- * Creating a Request
    DescribeClusterParameters (..),
    newDescribeClusterParameters,

    -- * Request Lenses
    describeClusterParameters_source,
    describeClusterParameters_marker,
    describeClusterParameters_maxRecords,
    describeClusterParameters_parameterGroupName,

    -- * Destructuring the Response
    DescribeClusterParametersResponse (..),
    newDescribeClusterParametersResponse,

    -- * Response Lenses
    describeClusterParametersResponse_parameters,
    describeClusterParametersResponse_marker,
    describeClusterParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterParameters' smart constructor.
data DescribeClusterParameters = DescribeClusterParameters'
  { -- | The parameter types to return. Specify @user@ to show parameters that
    -- are different form the default. Similarly, specify @engine-default@ to
    -- show parameters that are the same as the default parameter group.
    --
    -- Default: All parameter types returned.
    --
    -- Valid Values: @user@ | @engine-default@
    source :: Core.Maybe Core.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterParameters
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of a cluster parameter group for which to return details.
    parameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'describeClusterParameters_source' - The parameter types to return. Specify @user@ to show parameters that
-- are different form the default. Similarly, specify @engine-default@ to
-- show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: @user@ | @engine-default@
--
-- 'marker', 'describeClusterParameters_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
--
-- 'maxRecords', 'describeClusterParameters_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'parameterGroupName', 'describeClusterParameters_parameterGroupName' - The name of a cluster parameter group for which to return details.
newDescribeClusterParameters ::
  -- | 'parameterGroupName'
  Core.Text ->
  DescribeClusterParameters
newDescribeClusterParameters pParameterGroupName_ =
  DescribeClusterParameters'
    { source = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | The parameter types to return. Specify @user@ to show parameters that
-- are different form the default. Similarly, specify @engine-default@ to
-- show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: @user@ | @engine-default@
describeClusterParameters_source :: Lens.Lens' DescribeClusterParameters (Core.Maybe Core.Text)
describeClusterParameters_source = Lens.lens (\DescribeClusterParameters' {source} -> source) (\s@DescribeClusterParameters' {} a -> s {source = a} :: DescribeClusterParameters)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeClusterParameters_marker :: Lens.Lens' DescribeClusterParameters (Core.Maybe Core.Text)
describeClusterParameters_marker = Lens.lens (\DescribeClusterParameters' {marker} -> marker) (\s@DescribeClusterParameters' {} a -> s {marker = a} :: DescribeClusterParameters)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeClusterParameters_maxRecords :: Lens.Lens' DescribeClusterParameters (Core.Maybe Core.Int)
describeClusterParameters_maxRecords = Lens.lens (\DescribeClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeClusterParameters' {} a -> s {maxRecords = a} :: DescribeClusterParameters)

-- | The name of a cluster parameter group for which to return details.
describeClusterParameters_parameterGroupName :: Lens.Lens' DescribeClusterParameters Core.Text
describeClusterParameters_parameterGroupName = Lens.lens (\DescribeClusterParameters' {parameterGroupName} -> parameterGroupName) (\s@DescribeClusterParameters' {} a -> s {parameterGroupName = a} :: DescribeClusterParameters)

instance Core.AWSPager DescribeClusterParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterParametersResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterParametersResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClusterParameters_marker
          Lens..~ rs
          Lens.^? describeClusterParametersResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeClusterParameters where
  type
    AWSResponse DescribeClusterParameters =
      DescribeClusterParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeClusterParametersResult"
      ( \s h x ->
          DescribeClusterParametersResponse'
            Core.<$> ( x Core..@? "Parameters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Parameter")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeClusterParameters

instance Core.NFData DescribeClusterParameters

instance Core.ToHeaders DescribeClusterParameters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClusterParameters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClusterParameters where
  toQuery DescribeClusterParameters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeClusterParameters" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Source" Core.=: source,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "ParameterGroupName" Core.=: parameterGroupName
      ]

-- | Contains the output from the DescribeClusterParameters action.
--
-- /See:/ 'newDescribeClusterParametersResponse' smart constructor.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
  { -- | A list of Parameter instances. Each instance lists the parameters of one
    -- cluster parameter group.
    parameters :: Core.Maybe [Parameter],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'describeClusterParametersResponse_parameters' - A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
--
-- 'marker', 'describeClusterParametersResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeClusterParametersResponse_httpStatus' - The response's http status code.
newDescribeClusterParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClusterParametersResponse
newDescribeClusterParametersResponse pHttpStatus_ =
  DescribeClusterParametersResponse'
    { parameters =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
describeClusterParametersResponse_parameters :: Lens.Lens' DescribeClusterParametersResponse (Core.Maybe [Parameter])
describeClusterParametersResponse_parameters = Lens.lens (\DescribeClusterParametersResponse' {parameters} -> parameters) (\s@DescribeClusterParametersResponse' {} a -> s {parameters = a} :: DescribeClusterParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterParametersResponse_marker :: Lens.Lens' DescribeClusterParametersResponse (Core.Maybe Core.Text)
describeClusterParametersResponse_marker = Lens.lens (\DescribeClusterParametersResponse' {marker} -> marker) (\s@DescribeClusterParametersResponse' {} a -> s {marker = a} :: DescribeClusterParametersResponse)

-- | The response's http status code.
describeClusterParametersResponse_httpStatus :: Lens.Lens' DescribeClusterParametersResponse Core.Int
describeClusterParametersResponse_httpStatus = Lens.lens (\DescribeClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeClusterParametersResponse)

instance
  Core.NFData
    DescribeClusterParametersResponse
