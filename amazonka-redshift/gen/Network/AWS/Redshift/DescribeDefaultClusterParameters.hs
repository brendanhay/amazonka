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
-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter settings for the specified parameter group
-- family.
--
-- For more information about parameters and parameter groups, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
  ( -- * Creating a Request
    DescribeDefaultClusterParameters (..),
    newDescribeDefaultClusterParameters,

    -- * Request Lenses
    describeDefaultClusterParameters_marker,
    describeDefaultClusterParameters_maxRecords,
    describeDefaultClusterParameters_parameterGroupFamily,

    -- * Destructuring the Response
    DescribeDefaultClusterParametersResponse (..),
    newDescribeDefaultClusterParametersResponse,

    -- * Response Lenses
    describeDefaultClusterParametersResponse_httpStatus,
    describeDefaultClusterParametersResponse_defaultClusterParameters,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDefaultClusterParameters' smart constructor.
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeDefaultClusterParameters request exceed the value specified in
    -- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
    -- You can retrieve the next set of response records by providing the
    -- returned marker value in the @Marker@ parameter and retrying the
    -- request.
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
    -- | The name of the cluster parameter group family.
    parameterGroupFamily :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDefaultClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDefaultClusterParameters_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeDefaultClusterParameters request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
--
-- 'maxRecords', 'describeDefaultClusterParameters_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'parameterGroupFamily', 'describeDefaultClusterParameters_parameterGroupFamily' - The name of the cluster parameter group family.
newDescribeDefaultClusterParameters ::
  -- | 'parameterGroupFamily'
  Core.Text ->
  DescribeDefaultClusterParameters
newDescribeDefaultClusterParameters
  pParameterGroupFamily_ =
    DescribeDefaultClusterParameters'
      { marker =
          Core.Nothing,
        maxRecords = Core.Nothing,
        parameterGroupFamily =
          pParameterGroupFamily_
      }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeDefaultClusterParameters request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
describeDefaultClusterParameters_marker :: Lens.Lens' DescribeDefaultClusterParameters (Core.Maybe Core.Text)
describeDefaultClusterParameters_marker = Lens.lens (\DescribeDefaultClusterParameters' {marker} -> marker) (\s@DescribeDefaultClusterParameters' {} a -> s {marker = a} :: DescribeDefaultClusterParameters)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeDefaultClusterParameters_maxRecords :: Lens.Lens' DescribeDefaultClusterParameters (Core.Maybe Core.Int)
describeDefaultClusterParameters_maxRecords = Lens.lens (\DescribeDefaultClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeDefaultClusterParameters' {} a -> s {maxRecords = a} :: DescribeDefaultClusterParameters)

-- | The name of the cluster parameter group family.
describeDefaultClusterParameters_parameterGroupFamily :: Lens.Lens' DescribeDefaultClusterParameters Core.Text
describeDefaultClusterParameters_parameterGroupFamily = Lens.lens (\DescribeDefaultClusterParameters' {parameterGroupFamily} -> parameterGroupFamily) (\s@DescribeDefaultClusterParameters' {} a -> s {parameterGroupFamily = a} :: DescribeDefaultClusterParameters)

instance
  Core.AWSPager
    DescribeDefaultClusterParameters
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
              Core.. defaultClusterParameters_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
              Core.. defaultClusterParameters_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDefaultClusterParameters_marker
          Lens..~ rs
          Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
            Core.. defaultClusterParameters_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeDefaultClusterParameters
  where
  type
    AWSResponse DescribeDefaultClusterParameters =
      DescribeDefaultClusterParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDefaultClusterParametersResult"
      ( \s h x ->
          DescribeDefaultClusterParametersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "DefaultClusterParameters")
      )

instance
  Core.Hashable
    DescribeDefaultClusterParameters

instance Core.NFData DescribeDefaultClusterParameters

instance
  Core.ToHeaders
    DescribeDefaultClusterParameters
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDefaultClusterParameters where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeDefaultClusterParameters
  where
  toQuery DescribeDefaultClusterParameters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeDefaultClusterParameters" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "ParameterGroupFamily" Core.=: parameterGroupFamily
      ]

-- | /See:/ 'newDescribeDefaultClusterParametersResponse' smart constructor.
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    defaultClusterParameters :: DefaultClusterParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDefaultClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDefaultClusterParametersResponse_httpStatus' - The response's http status code.
--
-- 'defaultClusterParameters', 'describeDefaultClusterParametersResponse_defaultClusterParameters' - Undocumented member.
newDescribeDefaultClusterParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'defaultClusterParameters'
  DefaultClusterParameters ->
  DescribeDefaultClusterParametersResponse
newDescribeDefaultClusterParametersResponse
  pHttpStatus_
  pDefaultClusterParameters_ =
    DescribeDefaultClusterParametersResponse'
      { httpStatus =
          pHttpStatus_,
        defaultClusterParameters =
          pDefaultClusterParameters_
      }

-- | The response's http status code.
describeDefaultClusterParametersResponse_httpStatus :: Lens.Lens' DescribeDefaultClusterParametersResponse Core.Int
describeDefaultClusterParametersResponse_httpStatus = Lens.lens (\DescribeDefaultClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeDefaultClusterParametersResponse)

-- | Undocumented member.
describeDefaultClusterParametersResponse_defaultClusterParameters :: Lens.Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
describeDefaultClusterParametersResponse_defaultClusterParameters = Lens.lens (\DescribeDefaultClusterParametersResponse' {defaultClusterParameters} -> defaultClusterParameters) (\s@DescribeDefaultClusterParametersResponse' {} a -> s {defaultClusterParameters = a} :: DescribeDefaultClusterParametersResponse)

instance
  Core.NFData
    DescribeDefaultClusterParametersResponse
