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
-- Module      : Amazonka.Redshift.DescribeHsmClientCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your Amazon Web Services account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM client certificates that match any combination
-- of the specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- HSM client certificates that have any combination of those values are
-- returned.
--
-- If both tag keys and values are omitted from the request, HSM client
-- certificates are returned regardless of whether they have tag keys or
-- values associated with them.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeHsmClientCertificates
  ( -- * Creating a Request
    DescribeHsmClientCertificates (..),
    newDescribeHsmClientCertificates,

    -- * Request Lenses
    describeHsmClientCertificates_tagKeys,
    describeHsmClientCertificates_marker,
    describeHsmClientCertificates_tagValues,
    describeHsmClientCertificates_hsmClientCertificateIdentifier,
    describeHsmClientCertificates_maxRecords,

    -- * Destructuring the Response
    DescribeHsmClientCertificatesResponse (..),
    newDescribeHsmClientCertificatesResponse,

    -- * Response Lenses
    describeHsmClientCertificatesResponse_marker,
    describeHsmClientCertificatesResponse_hsmClientCertificates,
    describeHsmClientCertificatesResponse_httpStatus,
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
-- /See:/ 'newDescribeHsmClientCertificates' smart constructor.
data DescribeHsmClientCertificates = DescribeHsmClientCertificates'
  { -- | A tag key or keys for which you want to return all matching HSM client
    -- certificates that are associated with the specified key or keys. For
    -- example, suppose that you have HSM client certificates that are tagged
    -- with keys called @owner@ and @environment@. If you specify both of these
    -- tag keys in the request, Amazon Redshift returns a response with the HSM
    -- client certificates that have either or both of these tag keys
    -- associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeHsmClientCertificates
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A tag value or values for which you want to return all matching HSM
    -- client certificates that are associated with the specified tag value or
    -- values. For example, suppose that you have HSM client certificates that
    -- are tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the HSM client certificates that have either or both of these tag values
    -- associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of a specific HSM client certificate for which you want
    -- information. If no identifier is specified, information is returned for
    -- all HSM client certificates owned by your Amazon Web Services account.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    --
    -- Default: @100@
    --
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsmClientCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeys', 'describeHsmClientCertificates_tagKeys' - A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For
-- example, suppose that you have HSM client certificates that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the HSM
-- client certificates that have either or both of these tag keys
-- associated with them.
--
-- 'marker', 'describeHsmClientCertificates_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'tagValues', 'describeHsmClientCertificates_tagValues' - A tag value or values for which you want to return all matching HSM
-- client certificates that are associated with the specified tag value or
-- values. For example, suppose that you have HSM client certificates that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM client certificates that have either or both of these tag values
-- associated with them.
--
-- 'hsmClientCertificateIdentifier', 'describeHsmClientCertificates_hsmClientCertificateIdentifier' - The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your Amazon Web Services account.
--
-- 'maxRecords', 'describeHsmClientCertificates_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
newDescribeHsmClientCertificates ::
  DescribeHsmClientCertificates
newDescribeHsmClientCertificates =
  DescribeHsmClientCertificates'
    { tagKeys =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      hsmClientCertificateIdentifier =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For
-- example, suppose that you have HSM client certificates that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the HSM
-- client certificates that have either or both of these tag keys
-- associated with them.
describeHsmClientCertificates_tagKeys :: Lens.Lens' DescribeHsmClientCertificates (Prelude.Maybe [Prelude.Text])
describeHsmClientCertificates_tagKeys = Lens.lens (\DescribeHsmClientCertificates' {tagKeys} -> tagKeys) (\s@DescribeHsmClientCertificates' {} a -> s {tagKeys = a} :: DescribeHsmClientCertificates) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeHsmClientCertificates_marker :: Lens.Lens' DescribeHsmClientCertificates (Prelude.Maybe Prelude.Text)
describeHsmClientCertificates_marker = Lens.lens (\DescribeHsmClientCertificates' {marker} -> marker) (\s@DescribeHsmClientCertificates' {} a -> s {marker = a} :: DescribeHsmClientCertificates)

-- | A tag value or values for which you want to return all matching HSM
-- client certificates that are associated with the specified tag value or
-- values. For example, suppose that you have HSM client certificates that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM client certificates that have either or both of these tag values
-- associated with them.
describeHsmClientCertificates_tagValues :: Lens.Lens' DescribeHsmClientCertificates (Prelude.Maybe [Prelude.Text])
describeHsmClientCertificates_tagValues = Lens.lens (\DescribeHsmClientCertificates' {tagValues} -> tagValues) (\s@DescribeHsmClientCertificates' {} a -> s {tagValues = a} :: DescribeHsmClientCertificates) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your Amazon Web Services account.
describeHsmClientCertificates_hsmClientCertificateIdentifier :: Lens.Lens' DescribeHsmClientCertificates (Prelude.Maybe Prelude.Text)
describeHsmClientCertificates_hsmClientCertificateIdentifier = Lens.lens (\DescribeHsmClientCertificates' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@DescribeHsmClientCertificates' {} a -> s {hsmClientCertificateIdentifier = a} :: DescribeHsmClientCertificates)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeHsmClientCertificates_maxRecords :: Lens.Lens' DescribeHsmClientCertificates (Prelude.Maybe Prelude.Int)
describeHsmClientCertificates_maxRecords = Lens.lens (\DescribeHsmClientCertificates' {maxRecords} -> maxRecords) (\s@DescribeHsmClientCertificates' {} a -> s {maxRecords = a} :: DescribeHsmClientCertificates)

instance Core.AWSPager DescribeHsmClientCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHsmClientCertificatesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHsmClientCertificatesResponse_hsmClientCertificates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeHsmClientCertificates_marker
          Lens..~ rs
          Lens.^? describeHsmClientCertificatesResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeHsmClientCertificates
  where
  type
    AWSResponse DescribeHsmClientCertificates =
      DescribeHsmClientCertificatesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeHsmClientCertificatesResult"
      ( \s h x ->
          DescribeHsmClientCertificatesResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "HsmClientCertificates"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "HsmClientCertificate")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeHsmClientCertificates
  where
  hashWithSalt _salt DescribeHsmClientCertificates' {..} =
    _salt `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeHsmClientCertificates where
  rnf DescribeHsmClientCertificates' {..} =
    Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf hsmClientCertificateIdentifier
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeHsmClientCertificates where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeHsmClientCertificates where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHsmClientCertificates where
  toQuery DescribeHsmClientCertificates' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeHsmClientCertificates" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "Marker" Data.=: marker,
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues),
        "HsmClientCertificateIdentifier"
          Data.=: hsmClientCertificateIdentifier,
        "MaxRecords" Data.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeHsmClientCertificatesResponse' smart constructor.
data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of the identifiers for one or more HSM client certificates used
    -- by Amazon Redshift clusters to store and retrieve database encryption
    -- keys in an HSM.
    hsmClientCertificates :: Prelude.Maybe [HsmClientCertificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHsmClientCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeHsmClientCertificatesResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'hsmClientCertificates', 'describeHsmClientCertificatesResponse_hsmClientCertificates' - A list of the identifiers for one or more HSM client certificates used
-- by Amazon Redshift clusters to store and retrieve database encryption
-- keys in an HSM.
--
-- 'httpStatus', 'describeHsmClientCertificatesResponse_httpStatus' - The response's http status code.
newDescribeHsmClientCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHsmClientCertificatesResponse
newDescribeHsmClientCertificatesResponse pHttpStatus_ =
  DescribeHsmClientCertificatesResponse'
    { marker =
        Prelude.Nothing,
      hsmClientCertificates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeHsmClientCertificatesResponse_marker :: Lens.Lens' DescribeHsmClientCertificatesResponse (Prelude.Maybe Prelude.Text)
describeHsmClientCertificatesResponse_marker = Lens.lens (\DescribeHsmClientCertificatesResponse' {marker} -> marker) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {marker = a} :: DescribeHsmClientCertificatesResponse)

-- | A list of the identifiers for one or more HSM client certificates used
-- by Amazon Redshift clusters to store and retrieve database encryption
-- keys in an HSM.
describeHsmClientCertificatesResponse_hsmClientCertificates :: Lens.Lens' DescribeHsmClientCertificatesResponse (Prelude.Maybe [HsmClientCertificate])
describeHsmClientCertificatesResponse_hsmClientCertificates = Lens.lens (\DescribeHsmClientCertificatesResponse' {hsmClientCertificates} -> hsmClientCertificates) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {hsmClientCertificates = a} :: DescribeHsmClientCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeHsmClientCertificatesResponse_httpStatus :: Lens.Lens' DescribeHsmClientCertificatesResponse Prelude.Int
describeHsmClientCertificatesResponse_httpStatus = Lens.lens (\DescribeHsmClientCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeHsmClientCertificatesResponse)

instance
  Prelude.NFData
    DescribeHsmClientCertificatesResponse
  where
  rnf DescribeHsmClientCertificatesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf hsmClientCertificates
      `Prelude.seq` Prelude.rnf httpStatus
