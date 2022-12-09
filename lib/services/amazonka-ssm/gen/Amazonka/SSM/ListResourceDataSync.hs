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
-- Module      : Amazonka.SSM.ListResourceDataSync
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SSM.ListResourceDataSync
  ( -- * Creating a Request
    ListResourceDataSync (..),
    newListResourceDataSync,

    -- * Request Lenses
    listResourceDataSync_maxResults,
    listResourceDataSync_nextToken,
    listResourceDataSync_syncType,

    -- * Destructuring the Response
    ListResourceDataSyncResponse (..),
    newListResourceDataSyncResponse,

    -- * Response Lenses
    listResourceDataSyncResponse_nextToken,
    listResourceDataSyncResponse_resourceDataSyncItems,
    listResourceDataSyncResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListResourceDataSync' smart constructor.
data ListResourceDataSync = ListResourceDataSync'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | View a list of resource data syncs according to the sync type. Specify
    -- @SyncToDestination@ to view resource data syncs that synchronize data to
    -- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
    -- syncs from Organizations or from multiple Amazon Web Services Regions.
    syncType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceDataSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceDataSync_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listResourceDataSync_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'syncType', 'listResourceDataSync_syncType' - View a list of resource data syncs according to the sync type. Specify
-- @SyncToDestination@ to view resource data syncs that synchronize data to
-- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
-- syncs from Organizations or from multiple Amazon Web Services Regions.
newListResourceDataSync ::
  ListResourceDataSync
newListResourceDataSync =
  ListResourceDataSync'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      syncType = Prelude.Nothing
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listResourceDataSync_maxResults :: Lens.Lens' ListResourceDataSync (Prelude.Maybe Prelude.Natural)
listResourceDataSync_maxResults = Lens.lens (\ListResourceDataSync' {maxResults} -> maxResults) (\s@ListResourceDataSync' {} a -> s {maxResults = a} :: ListResourceDataSync)

-- | A token to start the list. Use this token to get the next set of
-- results.
listResourceDataSync_nextToken :: Lens.Lens' ListResourceDataSync (Prelude.Maybe Prelude.Text)
listResourceDataSync_nextToken = Lens.lens (\ListResourceDataSync' {nextToken} -> nextToken) (\s@ListResourceDataSync' {} a -> s {nextToken = a} :: ListResourceDataSync)

-- | View a list of resource data syncs according to the sync type. Specify
-- @SyncToDestination@ to view resource data syncs that synchronize data to
-- an Amazon S3 bucket. Specify @SyncFromSource@ to view resource data
-- syncs from Organizations or from multiple Amazon Web Services Regions.
listResourceDataSync_syncType :: Lens.Lens' ListResourceDataSync (Prelude.Maybe Prelude.Text)
listResourceDataSync_syncType = Lens.lens (\ListResourceDataSync' {syncType} -> syncType) (\s@ListResourceDataSync' {} a -> s {syncType = a} :: ListResourceDataSync)

instance Core.AWSPager ListResourceDataSync where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceDataSyncResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceDataSyncResponse_resourceDataSyncItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceDataSync_nextToken
          Lens..~ rs
          Lens.^? listResourceDataSyncResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceDataSync where
  type
    AWSResponse ListResourceDataSync =
      ListResourceDataSyncResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceDataSyncResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ResourceDataSyncItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResourceDataSync where
  hashWithSalt _salt ListResourceDataSync' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` syncType

instance Prelude.NFData ListResourceDataSync where
  rnf ListResourceDataSync' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf syncType

instance Data.ToHeaders ListResourceDataSync where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ListResourceDataSync" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceDataSync where
  toJSON ListResourceDataSync' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SyncType" Data..=) Prelude.<$> syncType
          ]
      )

instance Data.ToPath ListResourceDataSync where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourceDataSync where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceDataSyncResponse' smart constructor.
data ListResourceDataSyncResponse = ListResourceDataSyncResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of your current resource data sync configurations and their
    -- statuses.
    resourceDataSyncItems :: Prelude.Maybe [ResourceDataSyncItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceDataSyncItems', 'listResourceDataSyncResponse_resourceDataSyncItems' - A list of your current resource data sync configurations and their
-- statuses.
--
-- 'httpStatus', 'listResourceDataSyncResponse_httpStatus' - The response's http status code.
newListResourceDataSyncResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceDataSyncResponse
newListResourceDataSyncResponse pHttpStatus_ =
  ListResourceDataSyncResponse'
    { nextToken =
        Prelude.Nothing,
      resourceDataSyncItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listResourceDataSyncResponse_nextToken :: Lens.Lens' ListResourceDataSyncResponse (Prelude.Maybe Prelude.Text)
listResourceDataSyncResponse_nextToken = Lens.lens (\ListResourceDataSyncResponse' {nextToken} -> nextToken) (\s@ListResourceDataSyncResponse' {} a -> s {nextToken = a} :: ListResourceDataSyncResponse)

-- | A list of your current resource data sync configurations and their
-- statuses.
listResourceDataSyncResponse_resourceDataSyncItems :: Lens.Lens' ListResourceDataSyncResponse (Prelude.Maybe [ResourceDataSyncItem])
listResourceDataSyncResponse_resourceDataSyncItems = Lens.lens (\ListResourceDataSyncResponse' {resourceDataSyncItems} -> resourceDataSyncItems) (\s@ListResourceDataSyncResponse' {} a -> s {resourceDataSyncItems = a} :: ListResourceDataSyncResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceDataSyncResponse_httpStatus :: Lens.Lens' ListResourceDataSyncResponse Prelude.Int
listResourceDataSyncResponse_httpStatus = Lens.lens (\ListResourceDataSyncResponse' {httpStatus} -> httpStatus) (\s@ListResourceDataSyncResponse' {} a -> s {httpStatus = a} :: ListResourceDataSyncResponse)

instance Prelude.NFData ListResourceDataSyncResponse where
  rnf ListResourceDataSyncResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceDataSyncItems
      `Prelude.seq` Prelude.rnf httpStatus
