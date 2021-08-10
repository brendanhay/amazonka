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
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sessions for a TestGridProject.
module Network.AWS.DeviceFarm.ListTestGridSessions
  ( -- * Creating a Request
    ListTestGridSessions (..),
    newListTestGridSessions,

    -- * Request Lenses
    listTestGridSessions_nextToken,
    listTestGridSessions_status,
    listTestGridSessions_maxResult,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_creationTimeAfter,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Return only sessions in this state.
    status :: Prelude.Maybe TestGridSessionStatus,
    -- | Return only this many results at a time.
    maxResult :: Prelude.Maybe Prelude.Natural,
    -- | Return only sessions created before this time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Return only sessions that ended after this time.
    endTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Return only sessions that ended before this time.
    endTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Return only sessions created after this time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
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
-- 'nextToken', 'listTestGridSessions_nextToken' - Pagination token.
--
-- 'status', 'listTestGridSessions_status' - Return only sessions in this state.
--
-- 'maxResult', 'listTestGridSessions_maxResult' - Return only this many results at a time.
--
-- 'creationTimeBefore', 'listTestGridSessions_creationTimeBefore' - Return only sessions created before this time.
--
-- 'endTimeAfter', 'listTestGridSessions_endTimeAfter' - Return only sessions that ended after this time.
--
-- 'endTimeBefore', 'listTestGridSessions_endTimeBefore' - Return only sessions that ended before this time.
--
-- 'creationTimeAfter', 'listTestGridSessions_creationTimeAfter' - Return only sessions created after this time.
--
-- 'projectArn', 'listTestGridSessions_projectArn' - ARN of a TestGridProject.
newListTestGridSessions ::
  -- | 'projectArn'
  Prelude.Text ->
  ListTestGridSessions
newListTestGridSessions pProjectArn_ =
  ListTestGridSessions'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResult = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      endTimeAfter = Prelude.Nothing,
      endTimeBefore = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      projectArn = pProjectArn_
    }

-- | Pagination token.
listTestGridSessions_nextToken :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.Text)
listTestGridSessions_nextToken = Lens.lens (\ListTestGridSessions' {nextToken} -> nextToken) (\s@ListTestGridSessions' {} a -> s {nextToken = a} :: ListTestGridSessions)

-- | Return only sessions in this state.
listTestGridSessions_status :: Lens.Lens' ListTestGridSessions (Prelude.Maybe TestGridSessionStatus)
listTestGridSessions_status = Lens.lens (\ListTestGridSessions' {status} -> status) (\s@ListTestGridSessions' {} a -> s {status = a} :: ListTestGridSessions)

-- | Return only this many results at a time.
listTestGridSessions_maxResult :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.Natural)
listTestGridSessions_maxResult = Lens.lens (\ListTestGridSessions' {maxResult} -> maxResult) (\s@ListTestGridSessions' {} a -> s {maxResult = a} :: ListTestGridSessions)

-- | Return only sessions created before this time.
listTestGridSessions_creationTimeBefore :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_creationTimeBefore = Lens.lens (\ListTestGridSessions' {creationTimeBefore} -> creationTimeBefore) (\s@ListTestGridSessions' {} a -> s {creationTimeBefore = a} :: ListTestGridSessions) Prelude.. Lens.mapping Core._Time

-- | Return only sessions that ended after this time.
listTestGridSessions_endTimeAfter :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_endTimeAfter = Lens.lens (\ListTestGridSessions' {endTimeAfter} -> endTimeAfter) (\s@ListTestGridSessions' {} a -> s {endTimeAfter = a} :: ListTestGridSessions) Prelude.. Lens.mapping Core._Time

-- | Return only sessions that ended before this time.
listTestGridSessions_endTimeBefore :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_endTimeBefore = Lens.lens (\ListTestGridSessions' {endTimeBefore} -> endTimeBefore) (\s@ListTestGridSessions' {} a -> s {endTimeBefore = a} :: ListTestGridSessions) Prelude.. Lens.mapping Core._Time

-- | Return only sessions created after this time.
listTestGridSessions_creationTimeAfter :: Lens.Lens' ListTestGridSessions (Prelude.Maybe Prelude.UTCTime)
listTestGridSessions_creationTimeAfter = Lens.lens (\ListTestGridSessions' {creationTimeAfter} -> creationTimeAfter) (\s@ListTestGridSessions' {} a -> s {creationTimeAfter = a} :: ListTestGridSessions) Prelude.. Lens.mapping Core._Time

-- | ARN of a TestGridProject.
listTestGridSessions_projectArn :: Lens.Lens' ListTestGridSessions Prelude.Text
listTestGridSessions_projectArn = Lens.lens (\ListTestGridSessions' {projectArn} -> projectArn) (\s@ListTestGridSessions' {} a -> s {projectArn = a} :: ListTestGridSessions)

instance Core.AWSRequest ListTestGridSessions where
  type
    AWSResponse ListTestGridSessions =
      ListTestGridSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "testGridSessions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestGridSessions

instance Prelude.NFData ListTestGridSessions

instance Core.ToHeaders ListTestGridSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTestGridSessions where
  toJSON ListTestGridSessions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("status" Core..=) Prelude.<$> status,
            ("maxResult" Core..=) Prelude.<$> maxResult,
            ("creationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("endTimeAfter" Core..=) Prelude.<$> endTimeAfter,
            ("endTimeBefore" Core..=) Prelude.<$> endTimeBefore,
            ("creationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            Prelude.Just ("projectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath ListTestGridSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTestGridSessions where
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
listTestGridSessionsResponse_testGridSessions = Lens.lens (\ListTestGridSessionsResponse' {testGridSessions} -> testGridSessions) (\s@ListTestGridSessionsResponse' {} a -> s {testGridSessions = a} :: ListTestGridSessionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestGridSessionsResponse_httpStatus :: Lens.Lens' ListTestGridSessionsResponse Prelude.Int
listTestGridSessionsResponse_httpStatus = Lens.lens (\ListTestGridSessionsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionsResponse)

instance Prelude.NFData ListTestGridSessionsResponse
