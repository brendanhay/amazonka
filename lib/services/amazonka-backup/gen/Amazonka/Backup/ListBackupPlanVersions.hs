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
-- Module      : Amazonka.Backup.ListBackupPlanVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns version metadata of your backup plans, including Amazon Resource
-- Names (ARNs), backup plan IDs, creation and deletion dates, plan names,
-- and version IDs.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListBackupPlanVersions
  ( -- * Creating a Request
    ListBackupPlanVersions (..),
    newListBackupPlanVersions,

    -- * Request Lenses
    listBackupPlanVersions_nextToken,
    listBackupPlanVersions_maxResults,
    listBackupPlanVersions_backupPlanId,

    -- * Destructuring the Response
    ListBackupPlanVersionsResponse (..),
    newListBackupPlanVersionsResponse,

    -- * Response Lenses
    listBackupPlanVersionsResponse_nextToken,
    listBackupPlanVersionsResponse_backupPlanVersionsList,
    listBackupPlanVersionsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBackupPlanVersions' smart constructor.
data ListBackupPlanVersions = ListBackupPlanVersions'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupPlanVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBackupPlanVersions_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'maxResults', 'listBackupPlanVersions_maxResults' - The maximum number of items to be returned.
--
-- 'backupPlanId', 'listBackupPlanVersions_backupPlanId' - Uniquely identifies a backup plan.
newListBackupPlanVersions ::
  -- | 'backupPlanId'
  Prelude.Text ->
  ListBackupPlanVersions
newListBackupPlanVersions pBackupPlanId_ =
  ListBackupPlanVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      backupPlanId = pBackupPlanId_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupPlanVersions_nextToken :: Lens.Lens' ListBackupPlanVersions (Prelude.Maybe Prelude.Text)
listBackupPlanVersions_nextToken = Lens.lens (\ListBackupPlanVersions' {nextToken} -> nextToken) (\s@ListBackupPlanVersions' {} a -> s {nextToken = a} :: ListBackupPlanVersions)

-- | The maximum number of items to be returned.
listBackupPlanVersions_maxResults :: Lens.Lens' ListBackupPlanVersions (Prelude.Maybe Prelude.Natural)
listBackupPlanVersions_maxResults = Lens.lens (\ListBackupPlanVersions' {maxResults} -> maxResults) (\s@ListBackupPlanVersions' {} a -> s {maxResults = a} :: ListBackupPlanVersions)

-- | Uniquely identifies a backup plan.
listBackupPlanVersions_backupPlanId :: Lens.Lens' ListBackupPlanVersions Prelude.Text
listBackupPlanVersions_backupPlanId = Lens.lens (\ListBackupPlanVersions' {backupPlanId} -> backupPlanId) (\s@ListBackupPlanVersions' {} a -> s {backupPlanId = a} :: ListBackupPlanVersions)

instance Core.AWSPager ListBackupPlanVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBackupPlanVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBackupPlanVersionsResponse_backupPlanVersionsList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBackupPlanVersions_nextToken
          Lens..~ rs
          Lens.^? listBackupPlanVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBackupPlanVersions where
  type
    AWSResponse ListBackupPlanVersions =
      ListBackupPlanVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupPlanVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "BackupPlanVersionsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackupPlanVersions where
  hashWithSalt _salt ListBackupPlanVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` backupPlanId

instance Prelude.NFData ListBackupPlanVersions where
  rnf ListBackupPlanVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf backupPlanId

instance Data.ToHeaders ListBackupPlanVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBackupPlanVersions where
  toPath ListBackupPlanVersions' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Data.toBS backupPlanId,
        "/versions/"
      ]

instance Data.ToQuery ListBackupPlanVersions where
  toQuery ListBackupPlanVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListBackupPlanVersionsResponse' smart constructor.
data ListBackupPlanVersionsResponse = ListBackupPlanVersionsResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of version list items containing metadata about your backup
    -- plans.
    backupPlanVersionsList :: Prelude.Maybe [BackupPlansListMember],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupPlanVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBackupPlanVersionsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'backupPlanVersionsList', 'listBackupPlanVersionsResponse_backupPlanVersionsList' - An array of version list items containing metadata about your backup
-- plans.
--
-- 'httpStatus', 'listBackupPlanVersionsResponse_httpStatus' - The response's http status code.
newListBackupPlanVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupPlanVersionsResponse
newListBackupPlanVersionsResponse pHttpStatus_ =
  ListBackupPlanVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      backupPlanVersionsList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupPlanVersionsResponse_nextToken :: Lens.Lens' ListBackupPlanVersionsResponse (Prelude.Maybe Prelude.Text)
listBackupPlanVersionsResponse_nextToken = Lens.lens (\ListBackupPlanVersionsResponse' {nextToken} -> nextToken) (\s@ListBackupPlanVersionsResponse' {} a -> s {nextToken = a} :: ListBackupPlanVersionsResponse)

-- | An array of version list items containing metadata about your backup
-- plans.
listBackupPlanVersionsResponse_backupPlanVersionsList :: Lens.Lens' ListBackupPlanVersionsResponse (Prelude.Maybe [BackupPlansListMember])
listBackupPlanVersionsResponse_backupPlanVersionsList = Lens.lens (\ListBackupPlanVersionsResponse' {backupPlanVersionsList} -> backupPlanVersionsList) (\s@ListBackupPlanVersionsResponse' {} a -> s {backupPlanVersionsList = a} :: ListBackupPlanVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBackupPlanVersionsResponse_httpStatus :: Lens.Lens' ListBackupPlanVersionsResponse Prelude.Int
listBackupPlanVersionsResponse_httpStatus = Lens.lens (\ListBackupPlanVersionsResponse' {httpStatus} -> httpStatus) (\s@ListBackupPlanVersionsResponse' {} a -> s {httpStatus = a} :: ListBackupPlanVersionsResponse)

instance
  Prelude.NFData
    ListBackupPlanVersionsResponse
  where
  rnf ListBackupPlanVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf backupPlanVersionsList
      `Prelude.seq` Prelude.rnf httpStatus
