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
-- Module      : Amazonka.RedshiftServerLess.ListRecoveryPoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of recovery points.
--
-- This operation returns paginated results.
module Amazonka.RedshiftServerLess.ListRecoveryPoints
  ( -- * Creating a Request
    ListRecoveryPoints (..),
    newListRecoveryPoints,

    -- * Request Lenses
    listRecoveryPoints_nextToken,
    listRecoveryPoints_namespaceName,
    listRecoveryPoints_endTime,
    listRecoveryPoints_maxResults,
    listRecoveryPoints_startTime,

    -- * Destructuring the Response
    ListRecoveryPointsResponse (..),
    newListRecoveryPointsResponse,

    -- * Response Lenses
    listRecoveryPointsResponse_nextToken,
    listRecoveryPointsResponse_recoveryPoints,
    listRecoveryPointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecoveryPoints' smart constructor.
data ListRecoveryPoints = ListRecoveryPoints'
  { -- | If your initial @ListRecoveryPoints@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListRecoveryPoints@ operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace to list recovery points for.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The time when creation of the recovery point finished.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The time when the recovery point\'s creation was initiated.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecoveryPoints_nextToken' - If your initial @ListRecoveryPoints@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListRecoveryPoints@ operations, which returns results in the next page.
--
-- 'namespaceName', 'listRecoveryPoints_namespaceName' - The name of the namespace to list recovery points for.
--
-- 'endTime', 'listRecoveryPoints_endTime' - The time when creation of the recovery point finished.
--
-- 'maxResults', 'listRecoveryPoints_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'startTime', 'listRecoveryPoints_startTime' - The time when the recovery point\'s creation was initiated.
newListRecoveryPoints ::
  ListRecoveryPoints
newListRecoveryPoints =
  ListRecoveryPoints'
    { nextToken = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | If your initial @ListRecoveryPoints@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListRecoveryPoints@ operations, which returns results in the next page.
listRecoveryPoints_nextToken :: Lens.Lens' ListRecoveryPoints (Prelude.Maybe Prelude.Text)
listRecoveryPoints_nextToken = Lens.lens (\ListRecoveryPoints' {nextToken} -> nextToken) (\s@ListRecoveryPoints' {} a -> s {nextToken = a} :: ListRecoveryPoints)

-- | The name of the namespace to list recovery points for.
listRecoveryPoints_namespaceName :: Lens.Lens' ListRecoveryPoints (Prelude.Maybe Prelude.Text)
listRecoveryPoints_namespaceName = Lens.lens (\ListRecoveryPoints' {namespaceName} -> namespaceName) (\s@ListRecoveryPoints' {} a -> s {namespaceName = a} :: ListRecoveryPoints)

-- | The time when creation of the recovery point finished.
listRecoveryPoints_endTime :: Lens.Lens' ListRecoveryPoints (Prelude.Maybe Prelude.UTCTime)
listRecoveryPoints_endTime = Lens.lens (\ListRecoveryPoints' {endTime} -> endTime) (\s@ListRecoveryPoints' {} a -> s {endTime = a} :: ListRecoveryPoints) Prelude.. Lens.mapping Core._Time

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listRecoveryPoints_maxResults :: Lens.Lens' ListRecoveryPoints (Prelude.Maybe Prelude.Natural)
listRecoveryPoints_maxResults = Lens.lens (\ListRecoveryPoints' {maxResults} -> maxResults) (\s@ListRecoveryPoints' {} a -> s {maxResults = a} :: ListRecoveryPoints)

-- | The time when the recovery point\'s creation was initiated.
listRecoveryPoints_startTime :: Lens.Lens' ListRecoveryPoints (Prelude.Maybe Prelude.UTCTime)
listRecoveryPoints_startTime = Lens.lens (\ListRecoveryPoints' {startTime} -> startTime) (\s@ListRecoveryPoints' {} a -> s {startTime = a} :: ListRecoveryPoints) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListRecoveryPoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecoveryPointsResponse_recoveryPoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecoveryPoints_nextToken
          Lens..~ rs
          Lens.^? listRecoveryPointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRecoveryPoints where
  type
    AWSResponse ListRecoveryPoints =
      ListRecoveryPointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryPointsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "recoveryPoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecoveryPoints where
  hashWithSalt _salt ListRecoveryPoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListRecoveryPoints where
  rnf ListRecoveryPoints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startTime

instance Core.ToHeaders ListRecoveryPoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftServerless.ListRecoveryPoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRecoveryPoints where
  toJSON ListRecoveryPoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("namespaceName" Core..=) Prelude.<$> namespaceName,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("startTime" Core..=) Prelude.<$> startTime
          ]
      )

instance Core.ToPath ListRecoveryPoints where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRecoveryPoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecoveryPointsResponse' smart constructor.
data ListRecoveryPointsResponse = ListRecoveryPointsResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of @nextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned recovery point objects.
    recoveryPoints :: Prelude.Maybe [RecoveryPoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryPointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecoveryPointsResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page.
--
-- 'recoveryPoints', 'listRecoveryPointsResponse_recoveryPoints' - The returned recovery point objects.
--
-- 'httpStatus', 'listRecoveryPointsResponse_httpStatus' - The response's http status code.
newListRecoveryPointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecoveryPointsResponse
newListRecoveryPointsResponse pHttpStatus_ =
  ListRecoveryPointsResponse'
    { nextToken =
        Prelude.Nothing,
      recoveryPoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of @nextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page.
listRecoveryPointsResponse_nextToken :: Lens.Lens' ListRecoveryPointsResponse (Prelude.Maybe Prelude.Text)
listRecoveryPointsResponse_nextToken = Lens.lens (\ListRecoveryPointsResponse' {nextToken} -> nextToken) (\s@ListRecoveryPointsResponse' {} a -> s {nextToken = a} :: ListRecoveryPointsResponse)

-- | The returned recovery point objects.
listRecoveryPointsResponse_recoveryPoints :: Lens.Lens' ListRecoveryPointsResponse (Prelude.Maybe [RecoveryPoint])
listRecoveryPointsResponse_recoveryPoints = Lens.lens (\ListRecoveryPointsResponse' {recoveryPoints} -> recoveryPoints) (\s@ListRecoveryPointsResponse' {} a -> s {recoveryPoints = a} :: ListRecoveryPointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecoveryPointsResponse_httpStatus :: Lens.Lens' ListRecoveryPointsResponse Prelude.Int
listRecoveryPointsResponse_httpStatus = Lens.lens (\ListRecoveryPointsResponse' {httpStatus} -> httpStatus) (\s@ListRecoveryPointsResponse' {} a -> s {httpStatus = a} :: ListRecoveryPointsResponse)

instance Prelude.NFData ListRecoveryPointsResponse where
  rnf ListRecoveryPointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recoveryPoints
      `Prelude.seq` Prelude.rnf httpStatus
