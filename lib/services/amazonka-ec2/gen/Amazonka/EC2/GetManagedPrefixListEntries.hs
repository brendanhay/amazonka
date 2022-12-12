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
-- Module      : Amazonka.EC2.GetManagedPrefixListEntries
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the entries for a specified managed prefix list.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetManagedPrefixListEntries
  ( -- * Creating a Request
    GetManagedPrefixListEntries (..),
    newGetManagedPrefixListEntries,

    -- * Request Lenses
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_prefixListId,

    -- * Destructuring the Response
    GetManagedPrefixListEntriesResponse (..),
    newGetManagedPrefixListEntriesResponse,

    -- * Response Lenses
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetManagedPrefixListEntries' smart constructor.
data GetManagedPrefixListEntries = GetManagedPrefixListEntries'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the prefix list for which to return the entries. The
    -- default is the current version.
    targetVersion :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedPrefixListEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getManagedPrefixListEntries_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getManagedPrefixListEntries_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'getManagedPrefixListEntries_nextToken' - The token for the next page of results.
--
-- 'targetVersion', 'getManagedPrefixListEntries_targetVersion' - The version of the prefix list for which to return the entries. The
-- default is the current version.
--
-- 'prefixListId', 'getManagedPrefixListEntries_prefixListId' - The ID of the prefix list.
newGetManagedPrefixListEntries ::
  -- | 'prefixListId'
  Prelude.Text ->
  GetManagedPrefixListEntries
newGetManagedPrefixListEntries pPrefixListId_ =
  GetManagedPrefixListEntries'
    { dryRun =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targetVersion = Prelude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getManagedPrefixListEntries_dryRun :: Lens.Lens' GetManagedPrefixListEntries (Prelude.Maybe Prelude.Bool)
getManagedPrefixListEntries_dryRun = Lens.lens (\GetManagedPrefixListEntries' {dryRun} -> dryRun) (\s@GetManagedPrefixListEntries' {} a -> s {dryRun = a} :: GetManagedPrefixListEntries)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getManagedPrefixListEntries_maxResults :: Lens.Lens' GetManagedPrefixListEntries (Prelude.Maybe Prelude.Natural)
getManagedPrefixListEntries_maxResults = Lens.lens (\GetManagedPrefixListEntries' {maxResults} -> maxResults) (\s@GetManagedPrefixListEntries' {} a -> s {maxResults = a} :: GetManagedPrefixListEntries)

-- | The token for the next page of results.
getManagedPrefixListEntries_nextToken :: Lens.Lens' GetManagedPrefixListEntries (Prelude.Maybe Prelude.Text)
getManagedPrefixListEntries_nextToken = Lens.lens (\GetManagedPrefixListEntries' {nextToken} -> nextToken) (\s@GetManagedPrefixListEntries' {} a -> s {nextToken = a} :: GetManagedPrefixListEntries)

-- | The version of the prefix list for which to return the entries. The
-- default is the current version.
getManagedPrefixListEntries_targetVersion :: Lens.Lens' GetManagedPrefixListEntries (Prelude.Maybe Prelude.Integer)
getManagedPrefixListEntries_targetVersion = Lens.lens (\GetManagedPrefixListEntries' {targetVersion} -> targetVersion) (\s@GetManagedPrefixListEntries' {} a -> s {targetVersion = a} :: GetManagedPrefixListEntries)

-- | The ID of the prefix list.
getManagedPrefixListEntries_prefixListId :: Lens.Lens' GetManagedPrefixListEntries Prelude.Text
getManagedPrefixListEntries_prefixListId = Lens.lens (\GetManagedPrefixListEntries' {prefixListId} -> prefixListId) (\s@GetManagedPrefixListEntries' {} a -> s {prefixListId = a} :: GetManagedPrefixListEntries)

instance Core.AWSPager GetManagedPrefixListEntries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListEntriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListEntriesResponse_entries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getManagedPrefixListEntries_nextToken
          Lens..~ rs
          Lens.^? getManagedPrefixListEntriesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetManagedPrefixListEntries where
  type
    AWSResponse GetManagedPrefixListEntries =
      GetManagedPrefixListEntriesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetManagedPrefixListEntriesResponse'
            Prelude.<$> ( x Data..@? "entrySet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetManagedPrefixListEntries where
  hashWithSalt _salt GetManagedPrefixListEntries' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetVersion
      `Prelude.hashWithSalt` prefixListId

instance Prelude.NFData GetManagedPrefixListEntries where
  rnf GetManagedPrefixListEntries' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetVersion
      `Prelude.seq` Prelude.rnf prefixListId

instance Data.ToHeaders GetManagedPrefixListEntries where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetManagedPrefixListEntries where
  toPath = Prelude.const "/"

instance Data.ToQuery GetManagedPrefixListEntries where
  toQuery GetManagedPrefixListEntries' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetManagedPrefixListEntries" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "TargetVersion" Data.=: targetVersion,
        "PrefixListId" Data.=: prefixListId
      ]

-- | /See:/ 'newGetManagedPrefixListEntriesResponse' smart constructor.
data GetManagedPrefixListEntriesResponse = GetManagedPrefixListEntriesResponse'
  { -- | Information about the prefix list entries.
    entries :: Prelude.Maybe [PrefixListEntry],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedPrefixListEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'getManagedPrefixListEntriesResponse_entries' - Information about the prefix list entries.
--
-- 'nextToken', 'getManagedPrefixListEntriesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getManagedPrefixListEntriesResponse_httpStatus' - The response's http status code.
newGetManagedPrefixListEntriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetManagedPrefixListEntriesResponse
newGetManagedPrefixListEntriesResponse pHttpStatus_ =
  GetManagedPrefixListEntriesResponse'
    { entries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list entries.
getManagedPrefixListEntriesResponse_entries :: Lens.Lens' GetManagedPrefixListEntriesResponse (Prelude.Maybe [PrefixListEntry])
getManagedPrefixListEntriesResponse_entries = Lens.lens (\GetManagedPrefixListEntriesResponse' {entries} -> entries) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {entries = a} :: GetManagedPrefixListEntriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getManagedPrefixListEntriesResponse_nextToken :: Lens.Lens' GetManagedPrefixListEntriesResponse (Prelude.Maybe Prelude.Text)
getManagedPrefixListEntriesResponse_nextToken = Lens.lens (\GetManagedPrefixListEntriesResponse' {nextToken} -> nextToken) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {nextToken = a} :: GetManagedPrefixListEntriesResponse)

-- | The response's http status code.
getManagedPrefixListEntriesResponse_httpStatus :: Lens.Lens' GetManagedPrefixListEntriesResponse Prelude.Int
getManagedPrefixListEntriesResponse_httpStatus = Lens.lens (\GetManagedPrefixListEntriesResponse' {httpStatus} -> httpStatus) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {httpStatus = a} :: GetManagedPrefixListEntriesResponse)

instance
  Prelude.NFData
    GetManagedPrefixListEntriesResponse
  where
  rnf GetManagedPrefixListEntriesResponse' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
