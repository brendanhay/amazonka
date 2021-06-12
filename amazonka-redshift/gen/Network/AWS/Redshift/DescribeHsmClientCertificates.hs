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
-- Module      : Network.AWS.Redshift.DescribeHsmClientCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
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
module Network.AWS.Redshift.DescribeHsmClientCertificates
  ( -- * Creating a Request
    DescribeHsmClientCertificates (..),
    newDescribeHsmClientCertificates,

    -- * Request Lenses
    describeHsmClientCertificates_hsmClientCertificateIdentifier,
    describeHsmClientCertificates_tagKeys,
    describeHsmClientCertificates_tagValues,
    describeHsmClientCertificates_marker,
    describeHsmClientCertificates_maxRecords,

    -- * Destructuring the Response
    DescribeHsmClientCertificatesResponse (..),
    newDescribeHsmClientCertificatesResponse,

    -- * Response Lenses
    describeHsmClientCertificatesResponse_hsmClientCertificates,
    describeHsmClientCertificatesResponse_marker,
    describeHsmClientCertificatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeHsmClientCertificates' smart constructor.
data DescribeHsmClientCertificates = DescribeHsmClientCertificates'
  { -- | The identifier of a specific HSM client certificate for which you want
    -- information. If no identifier is specified, information is returned for
    -- all HSM client certificates owned by your AWS customer account.
    hsmClientCertificateIdentifier :: Core.Maybe Core.Text,
    -- | A tag key or keys for which you want to return all matching HSM client
    -- certificates that are associated with the specified key or keys. For
    -- example, suppose that you have HSM client certificates that are tagged
    -- with keys called @owner@ and @environment@. If you specify both of these
    -- tag keys in the request, Amazon Redshift returns a response with the HSM
    -- client certificates that have either or both of these tag keys
    -- associated with them.
    tagKeys :: Core.Maybe [Core.Text],
    -- | A tag value or values for which you want to return all matching HSM
    -- client certificates that are associated with the specified tag value or
    -- values. For example, suppose that you have HSM client certificates that
    -- are tagged with values called @admin@ and @test@. If you specify both of
    -- these tag values in the request, Amazon Redshift returns a response with
    -- the HSM client certificates that have either or both of these tag values
    -- associated with them.
    tagValues :: Core.Maybe [Core.Text],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeHsmClientCertificates
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
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHsmClientCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificateIdentifier', 'describeHsmClientCertificates_hsmClientCertificateIdentifier' - The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your AWS customer account.
--
-- 'tagKeys', 'describeHsmClientCertificates_tagKeys' - A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For
-- example, suppose that you have HSM client certificates that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the HSM
-- client certificates that have either or both of these tag keys
-- associated with them.
--
-- 'tagValues', 'describeHsmClientCertificates_tagValues' - A tag value or values for which you want to return all matching HSM
-- client certificates that are associated with the specified tag value or
-- values. For example, suppose that you have HSM client certificates that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM client certificates that have either or both of these tag values
-- associated with them.
--
-- 'marker', 'describeHsmClientCertificates_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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
    { hsmClientCertificateIdentifier =
        Core.Nothing,
      tagKeys = Core.Nothing,
      tagValues = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your AWS customer account.
describeHsmClientCertificates_hsmClientCertificateIdentifier :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Text)
describeHsmClientCertificates_hsmClientCertificateIdentifier = Lens.lens (\DescribeHsmClientCertificates' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@DescribeHsmClientCertificates' {} a -> s {hsmClientCertificateIdentifier = a} :: DescribeHsmClientCertificates)

-- | A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For
-- example, suppose that you have HSM client certificates that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the HSM
-- client certificates that have either or both of these tag keys
-- associated with them.
describeHsmClientCertificates_tagKeys :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe [Core.Text])
describeHsmClientCertificates_tagKeys = Lens.lens (\DescribeHsmClientCertificates' {tagKeys} -> tagKeys) (\s@DescribeHsmClientCertificates' {} a -> s {tagKeys = a} :: DescribeHsmClientCertificates) Core.. Lens.mapping Lens._Coerce

-- | A tag value or values for which you want to return all matching HSM
-- client certificates that are associated with the specified tag value or
-- values. For example, suppose that you have HSM client certificates that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM client certificates that have either or both of these tag values
-- associated with them.
describeHsmClientCertificates_tagValues :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe [Core.Text])
describeHsmClientCertificates_tagValues = Lens.lens (\DescribeHsmClientCertificates' {tagValues} -> tagValues) (\s@DescribeHsmClientCertificates' {} a -> s {tagValues = a} :: DescribeHsmClientCertificates) Core.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeHsmClientCertificates_marker :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Text)
describeHsmClientCertificates_marker = Lens.lens (\DescribeHsmClientCertificates' {marker} -> marker) (\s@DescribeHsmClientCertificates' {} a -> s {marker = a} :: DescribeHsmClientCertificates)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeHsmClientCertificates_maxRecords :: Lens.Lens' DescribeHsmClientCertificates (Core.Maybe Core.Int)
describeHsmClientCertificates_maxRecords = Lens.lens (\DescribeHsmClientCertificates' {maxRecords} -> maxRecords) (\s@DescribeHsmClientCertificates' {} a -> s {maxRecords = a} :: DescribeHsmClientCertificates)

instance Core.AWSPager DescribeHsmClientCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHsmClientCertificatesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHsmClientCertificatesResponse_hsmClientCertificates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeHsmClientCertificates_marker
          Lens..~ rs
          Lens.^? describeHsmClientCertificatesResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeHsmClientCertificates
  where
  type
    AWSResponse DescribeHsmClientCertificates =
      DescribeHsmClientCertificatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeHsmClientCertificatesResult"
      ( \s h x ->
          DescribeHsmClientCertificatesResponse'
            Core.<$> ( x Core..@? "HsmClientCertificates"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "HsmClientCertificate")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeHsmClientCertificates

instance Core.NFData DescribeHsmClientCertificates

instance Core.ToHeaders DescribeHsmClientCertificates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeHsmClientCertificates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeHsmClientCertificates where
  toQuery DescribeHsmClientCertificates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeHsmClientCertificates" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "HsmClientCertificateIdentifier"
          Core.=: hsmClientCertificateIdentifier,
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Core.<$> tagKeys),
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Core.<$> tagValues),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeHsmClientCertificatesResponse' smart constructor.
data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse'
  { -- | A list of the identifiers for one or more HSM client certificates used
    -- by Amazon Redshift clusters to store and retrieve database encryption
    -- keys in an HSM.
    hsmClientCertificates :: Core.Maybe [HsmClientCertificate],
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
-- Create a value of 'DescribeHsmClientCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificates', 'describeHsmClientCertificatesResponse_hsmClientCertificates' - A list of the identifiers for one or more HSM client certificates used
-- by Amazon Redshift clusters to store and retrieve database encryption
-- keys in an HSM.
--
-- 'marker', 'describeHsmClientCertificatesResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'httpStatus', 'describeHsmClientCertificatesResponse_httpStatus' - The response's http status code.
newDescribeHsmClientCertificatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHsmClientCertificatesResponse
newDescribeHsmClientCertificatesResponse pHttpStatus_ =
  DescribeHsmClientCertificatesResponse'
    { hsmClientCertificates =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the identifiers for one or more HSM client certificates used
-- by Amazon Redshift clusters to store and retrieve database encryption
-- keys in an HSM.
describeHsmClientCertificatesResponse_hsmClientCertificates :: Lens.Lens' DescribeHsmClientCertificatesResponse (Core.Maybe [HsmClientCertificate])
describeHsmClientCertificatesResponse_hsmClientCertificates = Lens.lens (\DescribeHsmClientCertificatesResponse' {hsmClientCertificates} -> hsmClientCertificates) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {hsmClientCertificates = a} :: DescribeHsmClientCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeHsmClientCertificatesResponse_marker :: Lens.Lens' DescribeHsmClientCertificatesResponse (Core.Maybe Core.Text)
describeHsmClientCertificatesResponse_marker = Lens.lens (\DescribeHsmClientCertificatesResponse' {marker} -> marker) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {marker = a} :: DescribeHsmClientCertificatesResponse)

-- | The response's http status code.
describeHsmClientCertificatesResponse_httpStatus :: Lens.Lens' DescribeHsmClientCertificatesResponse Core.Int
describeHsmClientCertificatesResponse_httpStatus = Lens.lens (\DescribeHsmClientCertificatesResponse' {httpStatus} -> httpStatus) (\s@DescribeHsmClientCertificatesResponse' {} a -> s {httpStatus = a} :: DescribeHsmClientCertificatesResponse)

instance
  Core.NFData
    DescribeHsmClientCertificatesResponse
