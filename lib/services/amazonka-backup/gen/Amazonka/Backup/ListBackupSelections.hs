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
-- Module      : Amazonka.Backup.ListBackupSelections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array containing metadata of the resources associated with
-- the target backup plan.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListBackupSelections
  ( -- * Creating a Request
    ListBackupSelections (..),
    newListBackupSelections,

    -- * Request Lenses
    listBackupSelections_maxResults,
    listBackupSelections_nextToken,
    listBackupSelections_backupPlanId,

    -- * Destructuring the Response
    ListBackupSelectionsResponse (..),
    newListBackupSelectionsResponse,

    -- * Response Lenses
    listBackupSelectionsResponse_backupSelectionsList,
    listBackupSelectionsResponse_nextToken,
    listBackupSelectionsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBackupSelections' smart constructor.
data ListBackupSelections = ListBackupSelections'
  { -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupSelections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBackupSelections_maxResults' - The maximum number of items to be returned.
--
-- 'nextToken', 'listBackupSelections_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'backupPlanId', 'listBackupSelections_backupPlanId' - Uniquely identifies a backup plan.
newListBackupSelections ::
  -- | 'backupPlanId'
  Prelude.Text ->
  ListBackupSelections
newListBackupSelections pBackupPlanId_ =
  ListBackupSelections'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      backupPlanId = pBackupPlanId_
    }

-- | The maximum number of items to be returned.
listBackupSelections_maxResults :: Lens.Lens' ListBackupSelections (Prelude.Maybe Prelude.Natural)
listBackupSelections_maxResults = Lens.lens (\ListBackupSelections' {maxResults} -> maxResults) (\s@ListBackupSelections' {} a -> s {maxResults = a} :: ListBackupSelections)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupSelections_nextToken :: Lens.Lens' ListBackupSelections (Prelude.Maybe Prelude.Text)
listBackupSelections_nextToken = Lens.lens (\ListBackupSelections' {nextToken} -> nextToken) (\s@ListBackupSelections' {} a -> s {nextToken = a} :: ListBackupSelections)

-- | Uniquely identifies a backup plan.
listBackupSelections_backupPlanId :: Lens.Lens' ListBackupSelections Prelude.Text
listBackupSelections_backupPlanId = Lens.lens (\ListBackupSelections' {backupPlanId} -> backupPlanId) (\s@ListBackupSelections' {} a -> s {backupPlanId = a} :: ListBackupSelections)

instance Core.AWSPager ListBackupSelections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBackupSelectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBackupSelectionsResponse_backupSelectionsList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listBackupSelections_nextToken
          Lens..~ rs
          Lens.^? listBackupSelectionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListBackupSelections where
  type
    AWSResponse ListBackupSelections =
      ListBackupSelectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBackupSelectionsResponse'
            Prelude.<$> ( x
                            Data..?> "BackupSelectionsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBackupSelections where
  hashWithSalt _salt ListBackupSelections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` backupPlanId

instance Prelude.NFData ListBackupSelections where
  rnf ListBackupSelections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf backupPlanId

instance Data.ToHeaders ListBackupSelections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBackupSelections where
  toPath ListBackupSelections' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Data.toBS backupPlanId,
        "/selections/"
      ]

instance Data.ToQuery ListBackupSelections where
  toQuery ListBackupSelections' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListBackupSelectionsResponse' smart constructor.
data ListBackupSelectionsResponse = ListBackupSelectionsResponse'
  { -- | An array of backup selection list items containing metadata about each
    -- resource in the list.
    backupSelectionsList :: Prelude.Maybe [BackupSelectionsListMember],
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBackupSelectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupSelectionsList', 'listBackupSelectionsResponse_backupSelectionsList' - An array of backup selection list items containing metadata about each
-- resource in the list.
--
-- 'nextToken', 'listBackupSelectionsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'httpStatus', 'listBackupSelectionsResponse_httpStatus' - The response's http status code.
newListBackupSelectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBackupSelectionsResponse
newListBackupSelectionsResponse pHttpStatus_ =
  ListBackupSelectionsResponse'
    { backupSelectionsList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of backup selection list items containing metadata about each
-- resource in the list.
listBackupSelectionsResponse_backupSelectionsList :: Lens.Lens' ListBackupSelectionsResponse (Prelude.Maybe [BackupSelectionsListMember])
listBackupSelectionsResponse_backupSelectionsList = Lens.lens (\ListBackupSelectionsResponse' {backupSelectionsList} -> backupSelectionsList) (\s@ListBackupSelectionsResponse' {} a -> s {backupSelectionsList = a} :: ListBackupSelectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listBackupSelectionsResponse_nextToken :: Lens.Lens' ListBackupSelectionsResponse (Prelude.Maybe Prelude.Text)
listBackupSelectionsResponse_nextToken = Lens.lens (\ListBackupSelectionsResponse' {nextToken} -> nextToken) (\s@ListBackupSelectionsResponse' {} a -> s {nextToken = a} :: ListBackupSelectionsResponse)

-- | The response's http status code.
listBackupSelectionsResponse_httpStatus :: Lens.Lens' ListBackupSelectionsResponse Prelude.Int
listBackupSelectionsResponse_httpStatus = Lens.lens (\ListBackupSelectionsResponse' {httpStatus} -> httpStatus) (\s@ListBackupSelectionsResponse' {} a -> s {httpStatus = a} :: ListBackupSelectionsResponse)

instance Prelude.NFData ListBackupSelectionsResponse where
  rnf ListBackupSelectionsResponse' {..} =
    Prelude.rnf backupSelectionsList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
