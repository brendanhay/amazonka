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
-- Module      : Amazonka.ElastiCache.DescribeUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of users.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeUsers
  ( -- * Creating a Request
    DescribeUsers (..),
    newDescribeUsers,

    -- * Request Lenses
    describeUsers_engine,
    describeUsers_filters,
    describeUsers_marker,
    describeUsers_maxRecords,
    describeUsers_userId,

    -- * Destructuring the Response
    DescribeUsersResponse (..),
    newDescribeUsersResponse,

    -- * Response Lenses
    describeUsersResponse_marker,
    describeUsersResponse_users,
    describeUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUsers' smart constructor.
data DescribeUsers = DescribeUsers'
  { -- | The Redis engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Filter to determine the list of User IDs to return.
    filters :: Prelude.Maybe [Filter],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The ID of the user.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engine', 'describeUsers_engine' - The Redis engine.
--
-- 'filters', 'describeUsers_filters' - Filter to determine the list of User IDs to return.
--
-- 'marker', 'describeUsers_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'maxRecords', 'describeUsers_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'userId', 'describeUsers_userId' - The ID of the user.
newDescribeUsers ::
  DescribeUsers
newDescribeUsers =
  DescribeUsers'
    { engine = Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The Redis engine.
describeUsers_engine :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_engine = Lens.lens (\DescribeUsers' {engine} -> engine) (\s@DescribeUsers' {} a -> s {engine = a} :: DescribeUsers)

-- | Filter to determine the list of User IDs to return.
describeUsers_filters :: Lens.Lens' DescribeUsers (Prelude.Maybe [Filter])
describeUsers_filters = Lens.lens (\DescribeUsers' {filters} -> filters) (\s@DescribeUsers' {} a -> s {filters = a} :: DescribeUsers) Prelude.. Lens.mapping Lens.coerced

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeUsers_marker :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_marker = Lens.lens (\DescribeUsers' {marker} -> marker) (\s@DescribeUsers' {} a -> s {marker = a} :: DescribeUsers)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeUsers_maxRecords :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Int)
describeUsers_maxRecords = Lens.lens (\DescribeUsers' {maxRecords} -> maxRecords) (\s@DescribeUsers' {} a -> s {maxRecords = a} :: DescribeUsers)

-- | The ID of the user.
describeUsers_userId :: Lens.Lens' DescribeUsers (Prelude.Maybe Prelude.Text)
describeUsers_userId = Lens.lens (\DescribeUsers' {userId} -> userId) (\s@DescribeUsers' {} a -> s {userId = a} :: DescribeUsers)

instance Core.AWSPager DescribeUsers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUsersResponse_users
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeUsers_marker
              Lens..~ rs
              Lens.^? describeUsersResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeUsers where
  type
    AWSResponse DescribeUsers =
      DescribeUsersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeUsersResult"
      ( \s h x ->
          DescribeUsersResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "Users" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUsers where
  hashWithSalt _salt DescribeUsers' {..} =
    _salt
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DescribeUsers where
  rnf DescribeUsers' {..} =
    Prelude.rnf engine `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf marker `Prelude.seq`
          Prelude.rnf maxRecords `Prelude.seq`
            Prelude.rnf userId

instance Data.ToHeaders DescribeUsers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUsers where
  toQuery DescribeUsers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeUsers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Engine" Data.=: engine,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "UserId" Data.=: userId
      ]

-- | /See:/ 'newDescribeUsersResponse' smart constructor.
data DescribeUsersResponse = DescribeUsersResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of users.
    users :: Prelude.Maybe [User],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeUsersResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'users', 'describeUsersResponse_users' - A list of users.
--
-- 'httpStatus', 'describeUsersResponse_httpStatus' - The response's http status code.
newDescribeUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUsersResponse
newDescribeUsersResponse pHttpStatus_ =
  DescribeUsersResponse'
    { marker = Prelude.Nothing,
      users = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeUsersResponse_marker :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe Prelude.Text)
describeUsersResponse_marker = Lens.lens (\DescribeUsersResponse' {marker} -> marker) (\s@DescribeUsersResponse' {} a -> s {marker = a} :: DescribeUsersResponse)

-- | A list of users.
describeUsersResponse_users :: Lens.Lens' DescribeUsersResponse (Prelude.Maybe [User])
describeUsersResponse_users = Lens.lens (\DescribeUsersResponse' {users} -> users) (\s@DescribeUsersResponse' {} a -> s {users = a} :: DescribeUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUsersResponse_httpStatus :: Lens.Lens' DescribeUsersResponse Prelude.Int
describeUsersResponse_httpStatus = Lens.lens (\DescribeUsersResponse' {httpStatus} -> httpStatus) (\s@DescribeUsersResponse' {} a -> s {httpStatus = a} :: DescribeUsersResponse)

instance Prelude.NFData DescribeUsersResponse where
  rnf DescribeUsersResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf users `Prelude.seq`
        Prelude.rnf httpStatus
