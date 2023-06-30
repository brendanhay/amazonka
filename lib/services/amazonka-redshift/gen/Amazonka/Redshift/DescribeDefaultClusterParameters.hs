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
-- Module      : Amazonka.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Redshift.DescribeDefaultClusterParameters
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeDefaultClusterParameters' smart constructor.
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a
    -- DescribeDefaultClusterParameters request exceed the value specified in
    -- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
    -- of the response. You can retrieve the next set of response records by
    -- providing the returned marker value in the @Marker@ parameter and
    -- retrying the request.
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
    -- | The name of the cluster parameter group family.
    parameterGroupFamily :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
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
  Prelude.Text ->
  DescribeDefaultClusterParameters
newDescribeDefaultClusterParameters
  pParameterGroupFamily_ =
    DescribeDefaultClusterParameters'
      { marker =
          Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        parameterGroupFamily =
          pParameterGroupFamily_
      }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeDefaultClusterParameters request exceed the value specified in
-- @MaxRecords@, Amazon Web Services returns a value in the @Marker@ field
-- of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
describeDefaultClusterParameters_marker :: Lens.Lens' DescribeDefaultClusterParameters (Prelude.Maybe Prelude.Text)
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
describeDefaultClusterParameters_maxRecords :: Lens.Lens' DescribeDefaultClusterParameters (Prelude.Maybe Prelude.Int)
describeDefaultClusterParameters_maxRecords = Lens.lens (\DescribeDefaultClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeDefaultClusterParameters' {} a -> s {maxRecords = a} :: DescribeDefaultClusterParameters)

-- | The name of the cluster parameter group family.
describeDefaultClusterParameters_parameterGroupFamily :: Lens.Lens' DescribeDefaultClusterParameters Prelude.Text
describeDefaultClusterParameters_parameterGroupFamily = Lens.lens (\DescribeDefaultClusterParameters' {parameterGroupFamily} -> parameterGroupFamily) (\s@DescribeDefaultClusterParameters' {} a -> s {parameterGroupFamily = a} :: DescribeDefaultClusterParameters)

instance
  Core.AWSPager
    DescribeDefaultClusterParameters
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
            Prelude.. defaultClusterParameters_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
            Prelude.. defaultClusterParameters_parameters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDefaultClusterParameters_marker
          Lens..~ rs
          Lens.^? describeDefaultClusterParametersResponse_defaultClusterParameters
          Prelude.. defaultClusterParameters_marker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDefaultClusterParameters
  where
  type
    AWSResponse DescribeDefaultClusterParameters =
      DescribeDefaultClusterParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDefaultClusterParametersResult"
      ( \s h x ->
          DescribeDefaultClusterParametersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "DefaultClusterParameters")
      )

instance
  Prelude.Hashable
    DescribeDefaultClusterParameters
  where
  hashWithSalt
    _salt
    DescribeDefaultClusterParameters' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` parameterGroupFamily

instance
  Prelude.NFData
    DescribeDefaultClusterParameters
  where
  rnf DescribeDefaultClusterParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf parameterGroupFamily

instance
  Data.ToHeaders
    DescribeDefaultClusterParameters
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDefaultClusterParameters where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDefaultClusterParameters
  where
  toQuery DescribeDefaultClusterParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDefaultClusterParameters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ParameterGroupFamily" Data.=: parameterGroupFamily
      ]

-- | /See:/ 'newDescribeDefaultClusterParametersResponse' smart constructor.
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    defaultClusterParameters :: DefaultClusterParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
describeDefaultClusterParametersResponse_httpStatus :: Lens.Lens' DescribeDefaultClusterParametersResponse Prelude.Int
describeDefaultClusterParametersResponse_httpStatus = Lens.lens (\DescribeDefaultClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeDefaultClusterParametersResponse)

-- | Undocumented member.
describeDefaultClusterParametersResponse_defaultClusterParameters :: Lens.Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
describeDefaultClusterParametersResponse_defaultClusterParameters = Lens.lens (\DescribeDefaultClusterParametersResponse' {defaultClusterParameters} -> defaultClusterParameters) (\s@DescribeDefaultClusterParametersResponse' {} a -> s {defaultClusterParameters = a} :: DescribeDefaultClusterParametersResponse)

instance
  Prelude.NFData
    DescribeDefaultClusterParametersResponse
  where
  rnf DescribeDefaultClusterParametersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf defaultClusterParameters
