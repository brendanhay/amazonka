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
-- Module      : Amazonka.Redshift.DescribeHsmConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Amazon Redshift HSM
-- configuration. If no configuration ID is specified, returns information
-- about all the HSM configurations owned by your Amazon Web Services
-- account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM connections that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- HSM connections that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, HSM
-- connections are returned regardless of whether they have tag keys or
-- values associated with them.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeHsmConfigurations
  ( -- * Creating a Request
    DescribeHsmConfigurations (..),
    newDescribeHsmConfigurations,

    -- * Request Lenses
    describeHsmConfigurations_tagKeys,
    describeHsmConfigurations_marker,
    describeHsmConfigurations_tagValues,
    describeHsmConfigurations_maxRecords,
    describeHsmConfigurations_hsmConfigurationIdentifier,

    -- * Destructuring the Response
    DescribeHsmConfigurationsResponse (..),
    newDescribeHsmConfigurationsResponse,

    -- * Response Lenses
    describeHsmConfigurationsResponse_marker,
    describeHsmConfigurationsResponse_hsmConfigurations,
    describeHsmConfigurationsResponse_httpStatus,
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
-- /See:/ 'newDescribeHsmConfigurations' smart constructor.
data DescribeHsmConfigurations = DescribeHsmConfigurations'
  { -- | A tag key or keys for which you want to return all matching HSM
    -- configurations that are associated with the specified key or keys. For
    -- example, suppose that you have HSM configurations that are tagged with
    -- keys called @owner@ and @environment@. If you specify both of these tag
    -- keys in the request, Amazon Redshift returns a response with the HSM
    -- configurations that have either or both of these tag keys associated
    -- with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeHsmConfigurations
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A tag value or values for which you want to return all matching HSM
    -- configurations that are associated with the specified tag value or
    -- values. For example, suppose that you have HSM configurations that are
    -- tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the HSM configurations that have either or both of these tag values
    -- associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
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
    -- | The identifier of a specific Amazon Redshift HSM configuration to be
    -- described. If no identifier is specified, information is returned for
    -- all HSM configurations owned by your Amazon Web Services account.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsmConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'describeHsmConfigurations_tagKeys' - A tag key or keys for which you want to return all matching HSM
-- configurations that are associated with the specified key or keys. For
-- example, suppose that you have HSM configurations that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the HSM
-- configurations that have either or both of these tag keys associated
-- with them.
--
-- 'marker', 'describeHsmConfigurations_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmConfigurations
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'tagValues', 'describeHsmConfigurations_tagValues' - A tag value or values for which you want to return all matching HSM
-- configurations that are associated with the specified tag value or
-- values. For example, suppose that you have HSM configurations that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM configurations that have either or both of these tag values
-- associated with them.
--
-- 'maxRecords', 'describeHsmConfigurations_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'hsmConfigurationIdentifier', 'describeHsmConfigurations_hsmConfigurationIdentifier' - The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for
-- all HSM configurations owned by your Amazon Web Services account.
newDescribeHsmConfigurations ::
  DescribeHsmConfigurations
newDescribeHsmConfigurations =
  DescribeHsmConfigurations'
    { tagKeys =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing
    }

-- | A tag key or keys for which you want to return all matching HSM
-- configurations that are associated with the specified key or keys. For
-- example, suppose that you have HSM configurations that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the HSM
-- configurations that have either or both of these tag keys associated
-- with them.
describeHsmConfigurations_tagKeys :: Lens.Lens' DescribeHsmConfigurations (Prelude.Maybe [Prelude.Text])
describeHsmConfigurations_tagKeys = Lens.lens (\DescribeHsmConfigurations' {tagKeys} -> tagKeys) (\s@DescribeHsmConfigurations' {} a -> s {tagKeys = a} :: DescribeHsmConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmConfigurations
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeHsmConfigurations_marker :: Lens.Lens' DescribeHsmConfigurations (Prelude.Maybe Prelude.Text)
describeHsmConfigurations_marker = Lens.lens (\DescribeHsmConfigurations' {marker} -> marker) (\s@DescribeHsmConfigurations' {} a -> s {marker = a} :: DescribeHsmConfigurations)

-- | A tag value or values for which you want to return all matching HSM
-- configurations that are associated with the specified tag value or
-- values. For example, suppose that you have HSM configurations that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM configurations that have either or both of these tag values
-- associated with them.
describeHsmConfigurations_tagValues :: Lens.Lens' DescribeHsmConfigurations (Prelude.Maybe [Prelude.Text])
describeHsmConfigurations_tagValues = Lens.lens (\DescribeHsmConfigurations' {tagValues} -> tagValues) (\s@DescribeHsmConfigurations' {} a -> s {tagValues = a} :: DescribeHsmConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeHsmConfigurations_maxRecords :: Lens.Lens' DescribeHsmConfigurations (Prelude.Maybe Prelude.Int)
describeHsmConfigurations_maxRecords = Lens.lens (\DescribeHsmConfigurations' {maxRecords} -> maxRecords) (\s@DescribeHsmConfigurations' {} a -> s {maxRecords = a} :: DescribeHsmConfigurations)

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for
-- all HSM configurations owned by your Amazon Web Services account.
describeHsmConfigurations_hsmConfigurationIdentifier :: Lens.Lens' DescribeHsmConfigurations (Prelude.Maybe Prelude.Text)
describeHsmConfigurations_hsmConfigurationIdentifier = Lens.lens (\DescribeHsmConfigurations' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@DescribeHsmConfigurations' {} a -> s {hsmConfigurationIdentifier = a} :: DescribeHsmConfigurations)

instance Core.AWSPager DescribeHsmConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHsmConfigurationsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHsmConfigurationsResponse_hsmConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeHsmConfigurations_marker
          Lens..~ rs
          Lens.^? describeHsmConfigurationsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeHsmConfigurations where
  type
    AWSResponse DescribeHsmConfigurations =
      DescribeHsmConfigurationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeHsmConfigurationsResult"
      ( \s h x ->
          DescribeHsmConfigurationsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "HsmConfigurations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "HsmConfiguration")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHsmConfigurations where
  hashWithSalt _salt DescribeHsmConfigurations' {..} =
    _salt `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` hsmConfigurationIdentifier

instance Prelude.NFData DescribeHsmConfigurations where
  rnf DescribeHsmConfigurations' {..} =
    Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf hsmConfigurationIdentifier

instance Data.ToHeaders DescribeHsmConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeHsmConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHsmConfigurations where
  toQuery DescribeHsmConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeHsmConfigurations" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "Marker" Data.=: marker,
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues),
        "MaxRecords" Data.=: maxRecords,
        "HsmConfigurationIdentifier"
          Data.=: hsmConfigurationIdentifier
      ]

-- |
--
-- /See:/ 'newDescribeHsmConfigurationsResponse' smart constructor.
data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of @HsmConfiguration@ objects.
    hsmConfigurations :: Prelude.Maybe [HsmConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsmConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeHsmConfigurationsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'hsmConfigurations', 'describeHsmConfigurationsResponse_hsmConfigurations' - A list of @HsmConfiguration@ objects.
--
-- 'httpStatus', 'describeHsmConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeHsmConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHsmConfigurationsResponse
newDescribeHsmConfigurationsResponse pHttpStatus_ =
  DescribeHsmConfigurationsResponse'
    { marker =
        Prelude.Nothing,
      hsmConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeHsmConfigurationsResponse_marker :: Lens.Lens' DescribeHsmConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeHsmConfigurationsResponse_marker = Lens.lens (\DescribeHsmConfigurationsResponse' {marker} -> marker) (\s@DescribeHsmConfigurationsResponse' {} a -> s {marker = a} :: DescribeHsmConfigurationsResponse)

-- | A list of @HsmConfiguration@ objects.
describeHsmConfigurationsResponse_hsmConfigurations :: Lens.Lens' DescribeHsmConfigurationsResponse (Prelude.Maybe [HsmConfiguration])
describeHsmConfigurationsResponse_hsmConfigurations = Lens.lens (\DescribeHsmConfigurationsResponse' {hsmConfigurations} -> hsmConfigurations) (\s@DescribeHsmConfigurationsResponse' {} a -> s {hsmConfigurations = a} :: DescribeHsmConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeHsmConfigurationsResponse_httpStatus :: Lens.Lens' DescribeHsmConfigurationsResponse Prelude.Int
describeHsmConfigurationsResponse_httpStatus = Lens.lens (\DescribeHsmConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeHsmConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeHsmConfigurationsResponse)

instance
  Prelude.NFData
    DescribeHsmConfigurationsResponse
  where
  rnf DescribeHsmConfigurationsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf hsmConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
