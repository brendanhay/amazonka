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
-- Module      : Network.AWS.EC2.GetManagedPrefixListEntries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the entries for a specified managed prefix list.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetManagedPrefixListEntries
  ( -- * Creating a Request
    GetManagedPrefixListEntries (..),
    newGetManagedPrefixListEntries,

    -- * Request Lenses
    getManagedPrefixListEntries_nextToken,
    getManagedPrefixListEntries_dryRun,
    getManagedPrefixListEntries_maxResults,
    getManagedPrefixListEntries_targetVersion,
    getManagedPrefixListEntries_prefixListId,

    -- * Destructuring the Response
    GetManagedPrefixListEntriesResponse (..),
    newGetManagedPrefixListEntriesResponse,

    -- * Response Lenses
    getManagedPrefixListEntriesResponse_nextToken,
    getManagedPrefixListEntriesResponse_entries,
    getManagedPrefixListEntriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetManagedPrefixListEntries' smart constructor.
data GetManagedPrefixListEntries = GetManagedPrefixListEntries'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The version of the prefix list for which to return the entries. The
    -- default is the current version.
    targetVersion :: Core.Maybe Core.Integer,
    -- | The ID of the prefix list.
    prefixListId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetManagedPrefixListEntries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getManagedPrefixListEntries_nextToken' - The token for the next page of results.
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
-- 'targetVersion', 'getManagedPrefixListEntries_targetVersion' - The version of the prefix list for which to return the entries. The
-- default is the current version.
--
-- 'prefixListId', 'getManagedPrefixListEntries_prefixListId' - The ID of the prefix list.
newGetManagedPrefixListEntries ::
  -- | 'prefixListId'
  Core.Text ->
  GetManagedPrefixListEntries
newGetManagedPrefixListEntries pPrefixListId_ =
  GetManagedPrefixListEntries'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      targetVersion = Core.Nothing,
      prefixListId = pPrefixListId_
    }

-- | The token for the next page of results.
getManagedPrefixListEntries_nextToken :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Text)
getManagedPrefixListEntries_nextToken = Lens.lens (\GetManagedPrefixListEntries' {nextToken} -> nextToken) (\s@GetManagedPrefixListEntries' {} a -> s {nextToken = a} :: GetManagedPrefixListEntries)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getManagedPrefixListEntries_dryRun :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Bool)
getManagedPrefixListEntries_dryRun = Lens.lens (\GetManagedPrefixListEntries' {dryRun} -> dryRun) (\s@GetManagedPrefixListEntries' {} a -> s {dryRun = a} :: GetManagedPrefixListEntries)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getManagedPrefixListEntries_maxResults :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Natural)
getManagedPrefixListEntries_maxResults = Lens.lens (\GetManagedPrefixListEntries' {maxResults} -> maxResults) (\s@GetManagedPrefixListEntries' {} a -> s {maxResults = a} :: GetManagedPrefixListEntries)

-- | The version of the prefix list for which to return the entries. The
-- default is the current version.
getManagedPrefixListEntries_targetVersion :: Lens.Lens' GetManagedPrefixListEntries (Core.Maybe Core.Integer)
getManagedPrefixListEntries_targetVersion = Lens.lens (\GetManagedPrefixListEntries' {targetVersion} -> targetVersion) (\s@GetManagedPrefixListEntries' {} a -> s {targetVersion = a} :: GetManagedPrefixListEntries)

-- | The ID of the prefix list.
getManagedPrefixListEntries_prefixListId :: Lens.Lens' GetManagedPrefixListEntries Core.Text
getManagedPrefixListEntries_prefixListId = Lens.lens (\GetManagedPrefixListEntries' {prefixListId} -> prefixListId) (\s@GetManagedPrefixListEntries' {} a -> s {prefixListId = a} :: GetManagedPrefixListEntries)

instance Core.AWSPager GetManagedPrefixListEntries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListEntriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getManagedPrefixListEntriesResponse_entries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getManagedPrefixListEntries_nextToken
          Lens..~ rs
          Lens.^? getManagedPrefixListEntriesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetManagedPrefixListEntries where
  type
    AWSResponse GetManagedPrefixListEntries =
      GetManagedPrefixListEntriesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetManagedPrefixListEntriesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "entrySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetManagedPrefixListEntries

instance Core.NFData GetManagedPrefixListEntries

instance Core.ToHeaders GetManagedPrefixListEntries where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetManagedPrefixListEntries where
  toPath = Core.const "/"

instance Core.ToQuery GetManagedPrefixListEntries where
  toQuery GetManagedPrefixListEntries' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetManagedPrefixListEntries" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "TargetVersion" Core.=: targetVersion,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newGetManagedPrefixListEntriesResponse' smart constructor.
data GetManagedPrefixListEntriesResponse = GetManagedPrefixListEntriesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the prefix list entries.
    entries :: Core.Maybe [PrefixListEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetManagedPrefixListEntriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getManagedPrefixListEntriesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'entries', 'getManagedPrefixListEntriesResponse_entries' - Information about the prefix list entries.
--
-- 'httpStatus', 'getManagedPrefixListEntriesResponse_httpStatus' - The response's http status code.
newGetManagedPrefixListEntriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetManagedPrefixListEntriesResponse
newGetManagedPrefixListEntriesResponse pHttpStatus_ =
  GetManagedPrefixListEntriesResponse'
    { nextToken =
        Core.Nothing,
      entries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getManagedPrefixListEntriesResponse_nextToken :: Lens.Lens' GetManagedPrefixListEntriesResponse (Core.Maybe Core.Text)
getManagedPrefixListEntriesResponse_nextToken = Lens.lens (\GetManagedPrefixListEntriesResponse' {nextToken} -> nextToken) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {nextToken = a} :: GetManagedPrefixListEntriesResponse)

-- | Information about the prefix list entries.
getManagedPrefixListEntriesResponse_entries :: Lens.Lens' GetManagedPrefixListEntriesResponse (Core.Maybe [PrefixListEntry])
getManagedPrefixListEntriesResponse_entries = Lens.lens (\GetManagedPrefixListEntriesResponse' {entries} -> entries) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {entries = a} :: GetManagedPrefixListEntriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getManagedPrefixListEntriesResponse_httpStatus :: Lens.Lens' GetManagedPrefixListEntriesResponse Core.Int
getManagedPrefixListEntriesResponse_httpStatus = Lens.lens (\GetManagedPrefixListEntriesResponse' {httpStatus} -> httpStatus) (\s@GetManagedPrefixListEntriesResponse' {} a -> s {httpStatus = a} :: GetManagedPrefixListEntriesResponse)

instance
  Core.NFData
    GetManagedPrefixListEntriesResponse
