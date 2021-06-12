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
-- Module      : Network.AWS.SSM.ListResourceDataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your resource data sync configurations. Includes information about
-- the last time a sync attempted to start, the last sync status, and the
-- last time a sync successfully completed.
--
-- The number of sync configurations might be too large to return using a
-- single call to @ListResourceDataSync@. You can limit the number of sync
-- configurations returned by using the @MaxResults@ parameter. To
-- determine whether there are more sync configurations to list, check the
-- value of @NextToken@ in the output. If there are more sync
-- configurations to list, you can request them by specifying the
-- @NextToken@ returned in the call to the parameter of a subsequent call.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceDataSync
  ( -- * Creating a Request
    ListResourceDataSync (..),
    newListResourceDataSync,

    -- * Request Lenses
    listResourceDataSync_syncType,
    listResourceDataSync_nextToken,
    listResourceDataSync_maxResults,

    -- * Destructuring the Response
    ListResourceDataSyncResponse (..),
    newListResourceDataSyncResponse,

    -- * Response Lenses
    listResourceDataSyncResponse_nextToken,
    listResourceDataSyncResponse_resourceDataSyncItems,
    listResourceDataSyncResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListResourceDataSync' smart constructor.
data ListResourceDataSync = ListResourceDataSync'
  { -- | View a list of resource data syncs according to the sync type. Specify
    -- @SyncToDestination@ to view resource data syncs that synchronize data to
    -- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
    -- syncs from AWS Organizations or from multiple AWS Regions.
    syncType :: Core.Maybe Core.Text,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncType', 'listResourceDataSync_syncType' - View a list of resource data syncs according to the sync type. Specify
-- @SyncToDestination@ to view resource data syncs that synchronize data to
-- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
-- syncs from AWS Organizations or from multiple AWS Regions.
--
-- 'nextToken', 'listResourceDataSync_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listResourceDataSync_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newListResourceDataSync ::
  ListResourceDataSync
newListResourceDataSync =
  ListResourceDataSync'
    { syncType = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | View a list of resource data syncs according to the sync type. Specify
-- @SyncToDestination@ to view resource data syncs that synchronize data to
-- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
-- syncs from AWS Organizations or from multiple AWS Regions.
listResourceDataSync_syncType :: Lens.Lens' ListResourceDataSync (Core.Maybe Core.Text)
listResourceDataSync_syncType = Lens.lens (\ListResourceDataSync' {syncType} -> syncType) (\s@ListResourceDataSync' {} a -> s {syncType = a} :: ListResourceDataSync)

-- | A token to start the list. Use this token to get the next set of
-- results.
listResourceDataSync_nextToken :: Lens.Lens' ListResourceDataSync (Core.Maybe Core.Text)
listResourceDataSync_nextToken = Lens.lens (\ListResourceDataSync' {nextToken} -> nextToken) (\s@ListResourceDataSync' {} a -> s {nextToken = a} :: ListResourceDataSync)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listResourceDataSync_maxResults :: Lens.Lens' ListResourceDataSync (Core.Maybe Core.Natural)
listResourceDataSync_maxResults = Lens.lens (\ListResourceDataSync' {maxResults} -> maxResults) (\s@ListResourceDataSync' {} a -> s {maxResults = a} :: ListResourceDataSync)

instance Core.AWSPager ListResourceDataSync where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDataSyncResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDataSyncResponse_resourceDataSyncItems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourceDataSync_nextToken
          Lens..~ rs
          Lens.^? listResourceDataSyncResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListResourceDataSync where
  type
    AWSResponse ListResourceDataSync =
      ListResourceDataSyncResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDataSyncResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ResourceDataSyncItems"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResourceDataSync

instance Core.NFData ListResourceDataSync

instance Core.ToHeaders ListResourceDataSync where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListResourceDataSync" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResourceDataSync where
  toJSON ListResourceDataSync' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SyncType" Core..=) Core.<$> syncType,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListResourceDataSync where
  toPath = Core.const "/"

instance Core.ToQuery ListResourceDataSync where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourceDataSyncResponse' smart constructor.
data ListResourceDataSyncResponse = ListResourceDataSyncResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of your current Resource Data Sync configurations and their
    -- statuses.
    resourceDataSyncItems :: Core.Maybe [ResourceDataSyncItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResourceDataSyncResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceDataSyncResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'resourceDataSyncItems', 'listResourceDataSyncResponse_resourceDataSyncItems' - A list of your current Resource Data Sync configurations and their
-- statuses.
--
-- 'httpStatus', 'listResourceDataSyncResponse_httpStatus' - The response's http status code.
newListResourceDataSyncResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResourceDataSyncResponse
newListResourceDataSyncResponse pHttpStatus_ =
  ListResourceDataSyncResponse'
    { nextToken =
        Core.Nothing,
      resourceDataSyncItems = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listResourceDataSyncResponse_nextToken :: Lens.Lens' ListResourceDataSyncResponse (Core.Maybe Core.Text)
listResourceDataSyncResponse_nextToken = Lens.lens (\ListResourceDataSyncResponse' {nextToken} -> nextToken) (\s@ListResourceDataSyncResponse' {} a -> s {nextToken = a} :: ListResourceDataSyncResponse)

-- | A list of your current Resource Data Sync configurations and their
-- statuses.
listResourceDataSyncResponse_resourceDataSyncItems :: Lens.Lens' ListResourceDataSyncResponse (Core.Maybe [ResourceDataSyncItem])
listResourceDataSyncResponse_resourceDataSyncItems = Lens.lens (\ListResourceDataSyncResponse' {resourceDataSyncItems} -> resourceDataSyncItems) (\s@ListResourceDataSyncResponse' {} a -> s {resourceDataSyncItems = a} :: ListResourceDataSyncResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceDataSyncResponse_httpStatus :: Lens.Lens' ListResourceDataSyncResponse Core.Int
listResourceDataSyncResponse_httpStatus = Lens.lens (\ListResourceDataSyncResponse' {httpStatus} -> httpStatus) (\s@ListResourceDataSyncResponse' {} a -> s {httpStatus = a} :: ListResourceDataSyncResponse)

instance Core.NFData ListResourceDataSyncResponse
