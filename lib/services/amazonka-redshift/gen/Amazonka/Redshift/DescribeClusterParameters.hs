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
-- Module      : Amazonka.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Redshift.DescribeClusterParameters
  ( -- * Creating a Request
    DescribeClusterParameters (..),
    newDescribeClusterParameters,

    -- * Request Lenses
    describeClusterParameters_marker,
    describeClusterParameters_maxRecords,
    describeClusterParameters_source,
    describeClusterParameters_parameterGroupName,

    -- * Destructuring the Response
    DescribeClusterParametersResponse (..),
    newDescribeClusterParametersResponse,

    -- * Response Lenses
    describeClusterParametersResponse_marker,
    describeClusterParametersResponse_parameters,
    describeClusterParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeClusterParameters' smart constructor.
data DescribeClusterParameters = DescribeClusterParameters'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeClusterParameters
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The parameter types to return. Specify @user@ to show parameters that
    -- are different form the default. Similarly, specify @engine-default@ to
    -- show parameters that are the same as the default parameter group.
    --
    -- Default: All parameter types returned.
    --
    -- Valid Values: @user@ | @engine-default@
    source :: Prelude.Maybe Prelude.Text,
    -- | The name of a cluster parameter group for which to return details.
    parameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeClusterParameters_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
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
-- 'source', 'describeClusterParameters_source' - The parameter types to return. Specify @user@ to show parameters that
-- are different form the default. Similarly, specify @engine-default@ to
-- show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: @user@ | @engine-default@
--
-- 'parameterGroupName', 'describeClusterParameters_parameterGroupName' - The name of a cluster parameter group for which to return details.
newDescribeClusterParameters ::
  -- | 'parameterGroupName'
  Prelude.Text ->
  DescribeClusterParameters
newDescribeClusterParameters pParameterGroupName_ =
  DescribeClusterParameters'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      source = Prelude.Nothing,
      parameterGroupName = pParameterGroupName_
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeClusterParameters_marker :: Lens.Lens' DescribeClusterParameters (Prelude.Maybe Prelude.Text)
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
describeClusterParameters_maxRecords :: Lens.Lens' DescribeClusterParameters (Prelude.Maybe Prelude.Int)
describeClusterParameters_maxRecords = Lens.lens (\DescribeClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeClusterParameters' {} a -> s {maxRecords = a} :: DescribeClusterParameters)

-- | The parameter types to return. Specify @user@ to show parameters that
-- are different form the default. Similarly, specify @engine-default@ to
-- show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: @user@ | @engine-default@
describeClusterParameters_source :: Lens.Lens' DescribeClusterParameters (Prelude.Maybe Prelude.Text)
describeClusterParameters_source = Lens.lens (\DescribeClusterParameters' {source} -> source) (\s@DescribeClusterParameters' {} a -> s {source = a} :: DescribeClusterParameters)

-- | The name of a cluster parameter group for which to return details.
describeClusterParameters_parameterGroupName :: Lens.Lens' DescribeClusterParameters Prelude.Text
describeClusterParameters_parameterGroupName = Lens.lens (\DescribeClusterParameters' {parameterGroupName} -> parameterGroupName) (\s@DescribeClusterParameters' {} a -> s {parameterGroupName = a} :: DescribeClusterParameters)

instance Core.AWSPager DescribeClusterParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClusterParametersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClusterParametersResponse_parameters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeClusterParameters_marker
          Lens..~ rs
          Lens.^? describeClusterParametersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusterParameters where
  type
    AWSResponse DescribeClusterParameters =
      DescribeClusterParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeClusterParametersResult"
      ( \s h x ->
          DescribeClusterParametersResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "Parameters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Parameter")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusterParameters where
  hashWithSalt _salt DescribeClusterParameters' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` parameterGroupName

instance Prelude.NFData DescribeClusterParameters where
  rnf DescribeClusterParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf parameterGroupName

instance Data.ToHeaders DescribeClusterParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeClusterParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusterParameters where
  toQuery DescribeClusterParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeClusterParameters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "Source" Data.=: source,
        "ParameterGroupName" Data.=: parameterGroupName
      ]

-- | Contains the output from the DescribeClusterParameters action.
--
-- /See:/ 'newDescribeClusterParametersResponse' smart constructor.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of Parameter instances. Each instance lists the parameters of one
    -- cluster parameter group.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeClusterParametersResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'parameters', 'describeClusterParametersResponse_parameters' - A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
--
-- 'httpStatus', 'describeClusterParametersResponse_httpStatus' - The response's http status code.
newDescribeClusterParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClusterParametersResponse
newDescribeClusterParametersResponse pHttpStatus_ =
  DescribeClusterParametersResponse'
    { marker =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeClusterParametersResponse_marker :: Lens.Lens' DescribeClusterParametersResponse (Prelude.Maybe Prelude.Text)
describeClusterParametersResponse_marker = Lens.lens (\DescribeClusterParametersResponse' {marker} -> marker) (\s@DescribeClusterParametersResponse' {} a -> s {marker = a} :: DescribeClusterParametersResponse)

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
describeClusterParametersResponse_parameters :: Lens.Lens' DescribeClusterParametersResponse (Prelude.Maybe [Parameter])
describeClusterParametersResponse_parameters = Lens.lens (\DescribeClusterParametersResponse' {parameters} -> parameters) (\s@DescribeClusterParametersResponse' {} a -> s {parameters = a} :: DescribeClusterParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClusterParametersResponse_httpStatus :: Lens.Lens' DescribeClusterParametersResponse Prelude.Int
describeClusterParametersResponse_httpStatus = Lens.lens (\DescribeClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeClusterParametersResponse)

instance
  Prelude.NFData
    DescribeClusterParametersResponse
  where
  rnf DescribeClusterParametersResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
