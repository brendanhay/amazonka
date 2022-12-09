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
-- Module      : Amazonka.EC2.DescribeFastSnapshotRestores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of fast snapshot restores for your snapshots.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeFastSnapshotRestores
  ( -- * Creating a Request
    DescribeFastSnapshotRestores (..),
    newDescribeFastSnapshotRestores,

    -- * Request Lenses
    describeFastSnapshotRestores_dryRun,
    describeFastSnapshotRestores_filters,
    describeFastSnapshotRestores_maxResults,
    describeFastSnapshotRestores_nextToken,

    -- * Destructuring the Response
    DescribeFastSnapshotRestoresResponse (..),
    newDescribeFastSnapshotRestoresResponse,

    -- * Response Lenses
    describeFastSnapshotRestoresResponse_fastSnapshotRestores,
    describeFastSnapshotRestoresResponse_nextToken,
    describeFastSnapshotRestoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFastSnapshotRestores' smart constructor.
data DescribeFastSnapshotRestores = DescribeFastSnapshotRestores'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters. The possible values are:
    --
    -- -   @availability-zone@: The Availability Zone of the snapshot.
    --
    -- -   @owner-id@: The ID of the Amazon Web Services account that enabled
    --     fast snapshot restore on the snapshot.
    --
    -- -   @snapshot-id@: The ID of the snapshot.
    --
    -- -   @state@: The state of fast snapshot restores for the snapshot
    --     (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFastSnapshotRestores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeFastSnapshotRestores_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeFastSnapshotRestores_filters' - The filters. The possible values are:
--
-- -   @availability-zone@: The Availability Zone of the snapshot.
--
-- -   @owner-id@: The ID of the Amazon Web Services account that enabled
--     fast snapshot restore on the snapshot.
--
-- -   @snapshot-id@: The ID of the snapshot.
--
-- -   @state@: The state of fast snapshot restores for the snapshot
--     (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@).
--
-- 'maxResults', 'describeFastSnapshotRestores_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeFastSnapshotRestores_nextToken' - The token for the next page of results.
newDescribeFastSnapshotRestores ::
  DescribeFastSnapshotRestores
newDescribeFastSnapshotRestores =
  DescribeFastSnapshotRestores'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFastSnapshotRestores_dryRun :: Lens.Lens' DescribeFastSnapshotRestores (Prelude.Maybe Prelude.Bool)
describeFastSnapshotRestores_dryRun = Lens.lens (\DescribeFastSnapshotRestores' {dryRun} -> dryRun) (\s@DescribeFastSnapshotRestores' {} a -> s {dryRun = a} :: DescribeFastSnapshotRestores)

-- | The filters. The possible values are:
--
-- -   @availability-zone@: The Availability Zone of the snapshot.
--
-- -   @owner-id@: The ID of the Amazon Web Services account that enabled
--     fast snapshot restore on the snapshot.
--
-- -   @snapshot-id@: The ID of the snapshot.
--
-- -   @state@: The state of fast snapshot restores for the snapshot
--     (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@).
describeFastSnapshotRestores_filters :: Lens.Lens' DescribeFastSnapshotRestores (Prelude.Maybe [Filter])
describeFastSnapshotRestores_filters = Lens.lens (\DescribeFastSnapshotRestores' {filters} -> filters) (\s@DescribeFastSnapshotRestores' {} a -> s {filters = a} :: DescribeFastSnapshotRestores) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeFastSnapshotRestores_maxResults :: Lens.Lens' DescribeFastSnapshotRestores (Prelude.Maybe Prelude.Natural)
describeFastSnapshotRestores_maxResults = Lens.lens (\DescribeFastSnapshotRestores' {maxResults} -> maxResults) (\s@DescribeFastSnapshotRestores' {} a -> s {maxResults = a} :: DescribeFastSnapshotRestores)

-- | The token for the next page of results.
describeFastSnapshotRestores_nextToken :: Lens.Lens' DescribeFastSnapshotRestores (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestores_nextToken = Lens.lens (\DescribeFastSnapshotRestores' {nextToken} -> nextToken) (\s@DescribeFastSnapshotRestores' {} a -> s {nextToken = a} :: DescribeFastSnapshotRestores)

instance Core.AWSPager DescribeFastSnapshotRestores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFastSnapshotRestoresResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFastSnapshotRestoresResponse_fastSnapshotRestores
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFastSnapshotRestores_nextToken
          Lens..~ rs
          Lens.^? describeFastSnapshotRestoresResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFastSnapshotRestores where
  type
    AWSResponse DescribeFastSnapshotRestores =
      DescribeFastSnapshotRestoresResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFastSnapshotRestoresResponse'
            Prelude.<$> ( x Data..@? "fastSnapshotRestoreSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFastSnapshotRestores
  where
  hashWithSalt _salt DescribeFastSnapshotRestores' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeFastSnapshotRestores where
  rnf DescribeFastSnapshotRestores' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeFastSnapshotRestores where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFastSnapshotRestores where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFastSnapshotRestores where
  toQuery DescribeFastSnapshotRestores' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeFastSnapshotRestores" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeFastSnapshotRestoresResponse' smart constructor.
data DescribeFastSnapshotRestoresResponse = DescribeFastSnapshotRestoresResponse'
  { -- | Information about the state of fast snapshot restores.
    fastSnapshotRestores :: Prelude.Maybe [DescribeFastSnapshotRestoreSuccessItem],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFastSnapshotRestoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fastSnapshotRestores', 'describeFastSnapshotRestoresResponse_fastSnapshotRestores' - Information about the state of fast snapshot restores.
--
-- 'nextToken', 'describeFastSnapshotRestoresResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeFastSnapshotRestoresResponse_httpStatus' - The response's http status code.
newDescribeFastSnapshotRestoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFastSnapshotRestoresResponse
newDescribeFastSnapshotRestoresResponse pHttpStatus_ =
  DescribeFastSnapshotRestoresResponse'
    { fastSnapshotRestores =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the state of fast snapshot restores.
describeFastSnapshotRestoresResponse_fastSnapshotRestores :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Prelude.Maybe [DescribeFastSnapshotRestoreSuccessItem])
describeFastSnapshotRestoresResponse_fastSnapshotRestores = Lens.lens (\DescribeFastSnapshotRestoresResponse' {fastSnapshotRestores} -> fastSnapshotRestores) (\s@DescribeFastSnapshotRestoresResponse' {} a -> s {fastSnapshotRestores = a} :: DescribeFastSnapshotRestoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeFastSnapshotRestoresResponse_nextToken :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Prelude.Maybe Prelude.Text)
describeFastSnapshotRestoresResponse_nextToken = Lens.lens (\DescribeFastSnapshotRestoresResponse' {nextToken} -> nextToken) (\s@DescribeFastSnapshotRestoresResponse' {} a -> s {nextToken = a} :: DescribeFastSnapshotRestoresResponse)

-- | The response's http status code.
describeFastSnapshotRestoresResponse_httpStatus :: Lens.Lens' DescribeFastSnapshotRestoresResponse Prelude.Int
describeFastSnapshotRestoresResponse_httpStatus = Lens.lens (\DescribeFastSnapshotRestoresResponse' {httpStatus} -> httpStatus) (\s@DescribeFastSnapshotRestoresResponse' {} a -> s {httpStatus = a} :: DescribeFastSnapshotRestoresResponse)

instance
  Prelude.NFData
    DescribeFastSnapshotRestoresResponse
  where
  rnf DescribeFastSnapshotRestoresResponse' {..} =
    Prelude.rnf fastSnapshotRestores
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
