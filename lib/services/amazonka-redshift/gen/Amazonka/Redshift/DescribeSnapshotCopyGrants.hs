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
-- Module      : Amazonka.Redshift.DescribeSnapshotCopyGrants
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of snapshot copy grants owned by the Amazon Web Services
-- account in the destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeSnapshotCopyGrants
  ( -- * Creating a Request
    DescribeSnapshotCopyGrants (..),
    newDescribeSnapshotCopyGrants,

    -- * Request Lenses
    describeSnapshotCopyGrants_marker,
    describeSnapshotCopyGrants_maxRecords,
    describeSnapshotCopyGrants_snapshotCopyGrantName,
    describeSnapshotCopyGrants_tagKeys,
    describeSnapshotCopyGrants_tagValues,

    -- * Destructuring the Response
    DescribeSnapshotCopyGrantsResponse (..),
    newDescribeSnapshotCopyGrantsResponse,

    -- * Response Lenses
    describeSnapshotCopyGrantsResponse_marker,
    describeSnapshotCopyGrantsResponse_snapshotCopyGrants,
    describeSnapshotCopyGrantsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The result of the @DescribeSnapshotCopyGrants@ action.
--
-- /See:/ 'newDescribeSnapshotCopyGrants' smart constructor.
data DescribeSnapshotCopyGrants = DescribeSnapshotCopyGrants'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a @DescribeSnapshotCopyGrant@
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__
    -- parameter or the __Marker__ parameter, but not both.
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
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text,
    -- | A tag key or keys for which you want to return all matching resources
    -- that are associated with the specified key or keys. For example, suppose
    -- that you have resources tagged with keys called @owner@ and
    -- @environment@. If you specify both of these tag keys in the request,
    -- Amazon Redshift returns a response with all resources that have either
    -- or both of these tag keys associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A tag value or values for which you want to return all matching
    -- resources that are associated with the specified value or values. For
    -- example, suppose that you have resources tagged with values called
    -- @admin@ and @test@. If you specify both of these tag values in the
    -- request, Amazon Redshift returns a response with all resources that have
    -- either or both of these tag values associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotCopyGrants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSnapshotCopyGrants_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
--
-- 'maxRecords', 'describeSnapshotCopyGrants_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
--
-- 'snapshotCopyGrantName', 'describeSnapshotCopyGrants_snapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- 'tagKeys', 'describeSnapshotCopyGrants_tagKeys' - A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
--
-- 'tagValues', 'describeSnapshotCopyGrants_tagValues' - A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
newDescribeSnapshotCopyGrants ::
  DescribeSnapshotCopyGrants
newDescribeSnapshotCopyGrants =
  DescribeSnapshotCopyGrants'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      snapshotCopyGrantName = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
describeSnapshotCopyGrants_marker :: Lens.Lens' DescribeSnapshotCopyGrants (Prelude.Maybe Prelude.Text)
describeSnapshotCopyGrants_marker = Lens.lens (\DescribeSnapshotCopyGrants' {marker} -> marker) (\s@DescribeSnapshotCopyGrants' {} a -> s {marker = a} :: DescribeSnapshotCopyGrants)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
describeSnapshotCopyGrants_maxRecords :: Lens.Lens' DescribeSnapshotCopyGrants (Prelude.Maybe Prelude.Int)
describeSnapshotCopyGrants_maxRecords = Lens.lens (\DescribeSnapshotCopyGrants' {maxRecords} -> maxRecords) (\s@DescribeSnapshotCopyGrants' {} a -> s {maxRecords = a} :: DescribeSnapshotCopyGrants)

-- | The name of the snapshot copy grant.
describeSnapshotCopyGrants_snapshotCopyGrantName :: Lens.Lens' DescribeSnapshotCopyGrants (Prelude.Maybe Prelude.Text)
describeSnapshotCopyGrants_snapshotCopyGrantName = Lens.lens (\DescribeSnapshotCopyGrants' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@DescribeSnapshotCopyGrants' {} a -> s {snapshotCopyGrantName = a} :: DescribeSnapshotCopyGrants)

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
describeSnapshotCopyGrants_tagKeys :: Lens.Lens' DescribeSnapshotCopyGrants (Prelude.Maybe [Prelude.Text])
describeSnapshotCopyGrants_tagKeys = Lens.lens (\DescribeSnapshotCopyGrants' {tagKeys} -> tagKeys) (\s@DescribeSnapshotCopyGrants' {} a -> s {tagKeys = a} :: DescribeSnapshotCopyGrants) Prelude.. Lens.mapping Lens.coerced

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
describeSnapshotCopyGrants_tagValues :: Lens.Lens' DescribeSnapshotCopyGrants (Prelude.Maybe [Prelude.Text])
describeSnapshotCopyGrants_tagValues = Lens.lens (\DescribeSnapshotCopyGrants' {tagValues} -> tagValues) (\s@DescribeSnapshotCopyGrants' {} a -> s {tagValues = a} :: DescribeSnapshotCopyGrants) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeSnapshotCopyGrants where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSnapshotCopyGrantsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSnapshotCopyGrantsResponse_snapshotCopyGrants
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSnapshotCopyGrants_marker
          Lens..~ rs
          Lens.^? describeSnapshotCopyGrantsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSnapshotCopyGrants where
  type
    AWSResponse DescribeSnapshotCopyGrants =
      DescribeSnapshotCopyGrantsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeSnapshotCopyGrantsResult"
      ( \s h x ->
          DescribeSnapshotCopyGrantsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "SnapshotCopyGrants"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "SnapshotCopyGrant")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSnapshotCopyGrants where
  hashWithSalt _salt DescribeSnapshotCopyGrants' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` snapshotCopyGrantName
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData DescribeSnapshotCopyGrants where
  rnf DescribeSnapshotCopyGrants' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf snapshotCopyGrantName
      `Prelude.seq` Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToHeaders DescribeSnapshotCopyGrants where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSnapshotCopyGrants where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSnapshotCopyGrants where
  toQuery DescribeSnapshotCopyGrants' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSnapshotCopyGrants" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "SnapshotCopyGrantName"
          Data.=: snapshotCopyGrantName,
        "TagKeys"
          Data.=: Data.toQuery
            (Data.toQueryList "TagKey" Prelude.<$> tagKeys),
        "TagValues"
          Data.=: Data.toQuery
            (Data.toQueryList "TagValue" Prelude.<$> tagValues)
      ]

-- |
--
-- /See:/ 'newDescribeSnapshotCopyGrantsResponse' smart constructor.
data DescribeSnapshotCopyGrantsResponse = DescribeSnapshotCopyGrantsResponse'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a @DescribeSnapshotCopyGrant@
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    --
    -- Constraints: You can specify either the __SnapshotCopyGrantName__
    -- parameter or the __Marker__ parameter, but not both.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of @SnapshotCopyGrant@ objects.
    snapshotCopyGrants :: Prelude.Maybe [SnapshotCopyGrant],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSnapshotCopyGrantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeSnapshotCopyGrantsResponse_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
--
-- 'snapshotCopyGrants', 'describeSnapshotCopyGrantsResponse_snapshotCopyGrants' - The list of @SnapshotCopyGrant@ objects.
--
-- 'httpStatus', 'describeSnapshotCopyGrantsResponse_httpStatus' - The response's http status code.
newDescribeSnapshotCopyGrantsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSnapshotCopyGrantsResponse
newDescribeSnapshotCopyGrantsResponse pHttpStatus_ =
  DescribeSnapshotCopyGrantsResponse'
    { marker =
        Prelude.Nothing,
      snapshotCopyGrants = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a @DescribeSnapshotCopyGrant@
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- Constraints: You can specify either the __SnapshotCopyGrantName__
-- parameter or the __Marker__ parameter, but not both.
describeSnapshotCopyGrantsResponse_marker :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Prelude.Maybe Prelude.Text)
describeSnapshotCopyGrantsResponse_marker = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {marker} -> marker) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {marker = a} :: DescribeSnapshotCopyGrantsResponse)

-- | The list of @SnapshotCopyGrant@ objects.
describeSnapshotCopyGrantsResponse_snapshotCopyGrants :: Lens.Lens' DescribeSnapshotCopyGrantsResponse (Prelude.Maybe [SnapshotCopyGrant])
describeSnapshotCopyGrantsResponse_snapshotCopyGrants = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {snapshotCopyGrants} -> snapshotCopyGrants) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {snapshotCopyGrants = a} :: DescribeSnapshotCopyGrantsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSnapshotCopyGrantsResponse_httpStatus :: Lens.Lens' DescribeSnapshotCopyGrantsResponse Prelude.Int
describeSnapshotCopyGrantsResponse_httpStatus = Lens.lens (\DescribeSnapshotCopyGrantsResponse' {httpStatus} -> httpStatus) (\s@DescribeSnapshotCopyGrantsResponse' {} a -> s {httpStatus = a} :: DescribeSnapshotCopyGrantsResponse)

instance
  Prelude.NFData
    DescribeSnapshotCopyGrantsResponse
  where
  rnf DescribeSnapshotCopyGrantsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf snapshotCopyGrants
      `Prelude.seq` Prelude.rnf httpStatus
