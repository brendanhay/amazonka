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
-- Module      : Amazonka.DeviceFarm.ListTestGridSessions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sessions for a TestGridProject.
module Amazonka.DeviceFarm.ListTestGridSessions
  ( -- * Creating a Request
    ListTestGridSessions (..),
    newListTestGridSessions,

    -- * Request Lenses
    listTestGridSessions_creationTimeAfter,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_maxResult,
    listTestGridSessions_nextToken,
    listTestGridSessions_status,
    listTestGridSessions_projectArn,

    -- * Destructuring the Response
    ListTestGridSessionsResponse (..),
    newListTestGridSessionsResponse,

    -- * Response Lenses
    listTestGridSessionsResponse_nextToken,
    listTestGridSessionsResponse_testGridSessions,
    listTestGridSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { -- | Return only sessions created after this time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Return only sessions created before this time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Return only sessions that ended after this time.
    endTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Return only sessions that ended before this time.
    endTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Return only this many results at a time.
    maxResult :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Return only sessions in this state.
    status :: Prelude.Maybe TestGridSessionStatus,
    -- | ARN of a TestGridProject.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listTestGridSessions_creationTimeAfter' - Return only sessions created after this time.
--
-- 'creationTimeBefore', 'listTestGridSessions_creationTimeBefore' - Return only sessions created before this time.
--
-- 'endTimeAfter', 'listTestGridSessions_endTimeAfter' - Return only sessions that ended after this time.
--
-- 'endTimeBefore', 'listTestGridSessions_endTimeBefore' - Return only sessions that ended before this time.
--
-- 'maxResult', 'listTestGridSessions_maxResult' - Return only this many results at a time.
--
-- 'nextToken', 'listTestGridSessions_nextToken' - Pagination token.
--
-- 'status', 'listTestGridSessions_status' - Return only sessions in this state.
--
-- 'projectArn', 'listTestGridSessions_projectArn' - ARN of a TestGridProject.
newListTestGridSessions ::
  -- | 'projectArn'
  Prelude.Text ->
  ListTestGridSessions
newListTestGridSessions pProjectArn_ =
  ListTestGridSessions'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      endTimeAfter = Prelude.Nothing,
      endTimeBefore = Prelude.Nothing,
      maxResult = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      projectArn = pProjectArn_
    }

-- | Return only sessions created after this time.
listTestGridSessions_creationTimeAfter :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_creationTimeAfter = Lens.lens (\ListTestGridSessions' {creationTimeAfter} -> creationTimeAfter) (\s@ListTestGridSessions' {} a -> s {creationTimeAfter = a} :: ListTestGridSessions) Prelude.. Lens.mapping Data._Time

-- | Return only sessions created before this time.
listTestGridSessions_creationTimeBefore :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_creationTimeBefore = Lens.lens (\ListTestGridSessions' {creationTimeBefore} -> creationTimeBefore) (\s@ListTestGridSessions' {} a -> s {creationTimeBefore = a} :: ListTestGridSessions) Prelude.. Lens.mapping Data._Time

-- | Return only sessions that ended after this time.
listTestGridSessions_endTimeAfter :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_endTimeAfter = Lens.lens (\ListTestGridSessions' {endTimeAfter} -> endTimeAfter) (\s@ListTestGridSessions' {} a -> s {endTimeAfter = a} :: ListTestGridSessions) Prelude.. Lens.mapping Data._Time

-- | Return only sessions that ended before this time.
listTestGridSessions_endTimeBefore :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_endTimeBefore = Lens.lens (\ListTestGridSessions' {endTimeBefore} -> endTimeBefore) (\s@ListTestGridSessions' {} a -> s {endTimeBefore = a} :: ListTestGridSessions) Prelude.. Lens.mapping Data._Time

-- | Return only this many results at a time.
listTestGridSessions_maxResult :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.Natural)
listTestGridSessions_maxResult = Lens.lens (\ListTestGridSessions' {maxResult} -> maxResult) (\s@ListTestGridSessions' {} a -> s {maxResult = a} :: ListTestGridSessions)

-- | Pagination token.
listTestGridSessions_nextToken :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.Text)
listTestGridSessions_nextToken = Lens.lens (\ListTestGridSessions' {nextToken} -> nextToken) (\s@ListTestGridSessions' {} a -> s {nextToken = a} :: ListTestGridSessions)

-- | Return only sessions in this state.
listTestGridSessions_status :: Lens.Lens' ListTestGridSessions (Prelude.Maybe TestGridSessionStatus)
listTestGridSessions_status = Lens.lens (\ListTestGridSessions' {status} -> status) (\s@ListTestGridSessions' {} a -> s {status = a} :: ListTestGridSessions)

-- | ARN of a TestGridProject.
listTestGridSessions_projectArn :: Lens.Lens' ListTestGridSessions Prelude.Text
listTestGridSessions_projectArn = Lens.lens (\ListTestGridSessions' {projectArn} -> projectArn) (\s@ListTestGridSessions' {} a -> s {projectArn = a} :: ListTestGridSessions)

instance Core.AWSRequest ListTestGridSessions where
  type
    AWSResponse ListTestGridSessions =
      ListTestGridSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "testGridSessions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestGridSessions where
  hashWithSalt _salt ListTestGridSessions' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` endTimeAfter
      `Prelude.hashWithSalt` endTimeBefore
      `Prelude.hashWithSalt` maxResult
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` projectArn

instance Prelude.NFData ListTestGridSessions where
  rnf ListTestGridSessions' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf endTimeAfter
      `Prelude.seq` Prelude.rnf endTimeBefore
      `Prelude.seq` Prelude.rnf maxResult
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf projectArn

instance Data.ToHeaders ListTestGridSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListTestGridSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTestGridSessions where
  toJSON ListTestGridSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("creationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("creationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("endTimeAfter" Data..=) Prelude.<$> endTimeAfter,
            ("endTimeBefore" Data..=) Prelude.<$> endTimeBefore,
            ("maxResult" Data..=) Prelude.<$> maxResult,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("status" Data..=) Prelude.<$> status,
            Prelude.Just ("projectArn" Data..= projectArn)
          ]
      )

instance Data.ToPath ListTestGridSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTestGridSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestGridSessionsResponse' smart constructor.
data ListTestGridSessionsResponse = ListTestGridSessionsResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sessions that match the criteria in a ListTestGridSessionsRequest.
    testGridSessions :: Prelude.Maybe [TestGridSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestGridSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridSessionsResponse_nextToken' - Pagination token.
--
-- 'testGridSessions', 'listTestGridSessionsResponse_testGridSessions' - The sessions that match the criteria in a ListTestGridSessionsRequest.
--
-- 'httpStatus', 'listTestGridSessionsResponse_httpStatus' - The response's http status code.
newListTestGridSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestGridSessionsResponse
newListTestGridSessionsResponse pHttpStatus_ =
  ListTestGridSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      testGridSessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listTestGridSessionsResponse_nextToken :: Lens.Lens' ListTestGridSessionsResponse (Prelude.Maybe Prelude.Text)
listTestGridSessionsResponse_nextToken = Lens.lens (\ListTestGridSessionsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionsResponse)

-- | The sessions that match the criteria in a ListTestGridSessionsRequest.
listTestGridSessionsResponse_testGridSessions :: Lens.Lens' ListTestGridSessionsResponse (Prelude.Maybe [TestGridSession])
listTestGridSessionsResponse_testGridSessions = Lens.lens (\ListTestGridSessionsResponse' {testGridSessions} -> testGridSessions) (\s@ListTestGridSessionsResponse' {} a -> s {testGridSessions = a} :: ListTestGridSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTestGridSessionsResponse_httpStatus :: Lens.Lens' ListTestGridSessionsResponse Prelude.Int
listTestGridSessionsResponse_httpStatus = Lens.lens (\ListTestGridSessionsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionsResponse)

instance Prelude.NFData ListTestGridSessionsResponse where
  rnf ListTestGridSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf testGridSessions
      `Prelude.seq` Prelude.rnf httpStatus
