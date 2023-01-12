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
-- Module      : Amazonka.RedshiftServerLess.ListTableRestoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an array of @TableRestoreStatus@ objects.
--
-- This operation returns paginated results.
module Amazonka.RedshiftServerLess.ListTableRestoreStatus
  ( -- * Creating a Request
    ListTableRestoreStatus (..),
    newListTableRestoreStatus,

    -- * Request Lenses
    listTableRestoreStatus_maxResults,
    listTableRestoreStatus_namespaceName,
    listTableRestoreStatus_nextToken,
    listTableRestoreStatus_workgroupName,

    -- * Destructuring the Response
    ListTableRestoreStatusResponse (..),
    newListTableRestoreStatusResponse,

    -- * Response Lenses
    listTableRestoreStatusResponse_nextToken,
    listTableRestoreStatusResponse_tableRestoreStatuses,
    listTableRestoreStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTableRestoreStatus' smart constructor.
data ListTableRestoreStatus = ListTableRestoreStatus'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use nextToken to display the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The namespace from which to list all of the statuses of
    -- @RestoreTableFromSnapshot@ operations .
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | If your initial @ListTableRestoreStatus@ operation returns a nextToken,
    -- you can include the returned @nextToken@ in following
    -- @ListTableRestoreStatus@ operations. This will return results on the
    -- next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The workgroup from which to list all of the statuses of
    -- @RestoreTableFromSnapshot@ operations.
    workgroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTableRestoreStatus_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use nextToken to display the next page of results.
--
-- 'namespaceName', 'listTableRestoreStatus_namespaceName' - The namespace from which to list all of the statuses of
-- @RestoreTableFromSnapshot@ operations .
--
-- 'nextToken', 'listTableRestoreStatus_nextToken' - If your initial @ListTableRestoreStatus@ operation returns a nextToken,
-- you can include the returned @nextToken@ in following
-- @ListTableRestoreStatus@ operations. This will return results on the
-- next page.
--
-- 'workgroupName', 'listTableRestoreStatus_workgroupName' - The workgroup from which to list all of the statuses of
-- @RestoreTableFromSnapshot@ operations.
newListTableRestoreStatus ::
  ListTableRestoreStatus
newListTableRestoreStatus =
  ListTableRestoreStatus'
    { maxResults =
        Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workgroupName = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use nextToken to display the next page of results.
listTableRestoreStatus_maxResults :: Lens.Lens' ListTableRestoreStatus (Prelude.Maybe Prelude.Natural)
listTableRestoreStatus_maxResults = Lens.lens (\ListTableRestoreStatus' {maxResults} -> maxResults) (\s@ListTableRestoreStatus' {} a -> s {maxResults = a} :: ListTableRestoreStatus)

-- | The namespace from which to list all of the statuses of
-- @RestoreTableFromSnapshot@ operations .
listTableRestoreStatus_namespaceName :: Lens.Lens' ListTableRestoreStatus (Prelude.Maybe Prelude.Text)
listTableRestoreStatus_namespaceName = Lens.lens (\ListTableRestoreStatus' {namespaceName} -> namespaceName) (\s@ListTableRestoreStatus' {} a -> s {namespaceName = a} :: ListTableRestoreStatus)

-- | If your initial @ListTableRestoreStatus@ operation returns a nextToken,
-- you can include the returned @nextToken@ in following
-- @ListTableRestoreStatus@ operations. This will return results on the
-- next page.
listTableRestoreStatus_nextToken :: Lens.Lens' ListTableRestoreStatus (Prelude.Maybe Prelude.Text)
listTableRestoreStatus_nextToken = Lens.lens (\ListTableRestoreStatus' {nextToken} -> nextToken) (\s@ListTableRestoreStatus' {} a -> s {nextToken = a} :: ListTableRestoreStatus)

-- | The workgroup from which to list all of the statuses of
-- @RestoreTableFromSnapshot@ operations.
listTableRestoreStatus_workgroupName :: Lens.Lens' ListTableRestoreStatus (Prelude.Maybe Prelude.Text)
listTableRestoreStatus_workgroupName = Lens.lens (\ListTableRestoreStatus' {workgroupName} -> workgroupName) (\s@ListTableRestoreStatus' {} a -> s {workgroupName = a} :: ListTableRestoreStatus)

instance Core.AWSPager ListTableRestoreStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTableRestoreStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTableRestoreStatusResponse_tableRestoreStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTableRestoreStatus_nextToken
          Lens..~ rs
          Lens.^? listTableRestoreStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTableRestoreStatus where
  type
    AWSResponse ListTableRestoreStatus =
      ListTableRestoreStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableRestoreStatusResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "tableRestoreStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTableRestoreStatus where
  hashWithSalt _salt ListTableRestoreStatus' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData ListTableRestoreStatus where
  rnf ListTableRestoreStatus' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders ListTableRestoreStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.ListTableRestoreStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTableRestoreStatus where
  toJSON ListTableRestoreStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("namespaceName" Data..=) Prelude.<$> namespaceName,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("workgroupName" Data..=) Prelude.<$> workgroupName
          ]
      )

instance Data.ToPath ListTableRestoreStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTableRestoreStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTableRestoreStatusResponse' smart constructor.
data ListTableRestoreStatusResponse = ListTableRestoreStatusResponse'
  { -- | If your initial @ListTableRestoreStatus@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in following
    -- @ListTableRestoreStatus@ operations. This will returns results on the
    -- next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The array of returned @TableRestoreStatus@ objects.
    tableRestoreStatuses :: Prelude.Maybe [TableRestoreStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableRestoreStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableRestoreStatusResponse_nextToken' - If your initial @ListTableRestoreStatus@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in following
-- @ListTableRestoreStatus@ operations. This will returns results on the
-- next page.
--
-- 'tableRestoreStatuses', 'listTableRestoreStatusResponse_tableRestoreStatuses' - The array of returned @TableRestoreStatus@ objects.
--
-- 'httpStatus', 'listTableRestoreStatusResponse_httpStatus' - The response's http status code.
newListTableRestoreStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTableRestoreStatusResponse
newListTableRestoreStatusResponse pHttpStatus_ =
  ListTableRestoreStatusResponse'
    { nextToken =
        Prelude.Nothing,
      tableRestoreStatuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If your initial @ListTableRestoreStatus@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in following
-- @ListTableRestoreStatus@ operations. This will returns results on the
-- next page.
listTableRestoreStatusResponse_nextToken :: Lens.Lens' ListTableRestoreStatusResponse (Prelude.Maybe Prelude.Text)
listTableRestoreStatusResponse_nextToken = Lens.lens (\ListTableRestoreStatusResponse' {nextToken} -> nextToken) (\s@ListTableRestoreStatusResponse' {} a -> s {nextToken = a} :: ListTableRestoreStatusResponse)

-- | The array of returned @TableRestoreStatus@ objects.
listTableRestoreStatusResponse_tableRestoreStatuses :: Lens.Lens' ListTableRestoreStatusResponse (Prelude.Maybe [TableRestoreStatus])
listTableRestoreStatusResponse_tableRestoreStatuses = Lens.lens (\ListTableRestoreStatusResponse' {tableRestoreStatuses} -> tableRestoreStatuses) (\s@ListTableRestoreStatusResponse' {} a -> s {tableRestoreStatuses = a} :: ListTableRestoreStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTableRestoreStatusResponse_httpStatus :: Lens.Lens' ListTableRestoreStatusResponse Prelude.Int
listTableRestoreStatusResponse_httpStatus = Lens.lens (\ListTableRestoreStatusResponse' {httpStatus} -> httpStatus) (\s@ListTableRestoreStatusResponse' {} a -> s {httpStatus = a} :: ListTableRestoreStatusResponse)

instance
  Prelude.NFData
    ListTableRestoreStatusResponse
  where
  rnf ListTableRestoreStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tableRestoreStatuses
      `Prelude.seq` Prelude.rnf httpStatus
