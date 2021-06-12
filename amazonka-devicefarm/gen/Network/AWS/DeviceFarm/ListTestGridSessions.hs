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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Return only sessions in this state.
    status :: Core.Maybe TestGridSessionStatus,
    -- | Return only this many results at a time.
    maxResult :: Core.Maybe Core.Natural,
    -- | Return only sessions created before this time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Return only sessions that ended after this time.
    endTimeAfter :: Core.Maybe Core.POSIX,
    -- | Return only sessions that ended before this time.
    endTimeBefore :: Core.Maybe Core.POSIX,
    -- | Return only sessions created after this time.
    creationTimeAfter :: Core.Maybe Core.POSIX,
    -- | ARN of a TestGridProject.
    projectArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListTestGridSessions
newListTestGridSessions pProjectArn_ =
  ListTestGridSessions'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      maxResult = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      endTimeAfter = Core.Nothing,
      endTimeBefore = Core.Nothing,
      creationTimeAfter = Core.Nothing,
      projectArn = pProjectArn_
    }

-- | Pagination token.
listTestGridSessions_nextToken :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.Text)
listTestGridSessions_nextToken = Lens.lens (\ListTestGridSessions' {nextToken} -> nextToken) (\s@ListTestGridSessions' {} a -> s {nextToken = a} :: ListTestGridSessions)

-- | Return only sessions in this state.
listTestGridSessions_status :: Lens.Lens' ListTestGridSessions (Core.Maybe TestGridSessionStatus)
listTestGridSessions_status = Lens.lens (\ListTestGridSessions' {status} -> status) (\s@ListTestGridSessions' {} a -> s {status = a} :: ListTestGridSessions)

-- | Return only this many results at a time.
listTestGridSessions_maxResult :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.Natural)
listTestGridSessions_maxResult = Lens.lens (\ListTestGridSessions' {maxResult} -> maxResult) (\s@ListTestGridSessions' {} a -> s {maxResult = a} :: ListTestGridSessions)

-- | Return only sessions created before this time.
listTestGridSessions_creationTimeBefore :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.UTCTime)
listTestGridSessions_creationTimeBefore = Lens.lens (\ListTestGridSessions' {creationTimeBefore} -> creationTimeBefore) (\s@ListTestGridSessions' {} a -> s {creationTimeBefore = a} :: ListTestGridSessions) Core.. Lens.mapping Core._Time

-- | Return only sessions that ended after this time.
listTestGridSessions_endTimeAfter :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.UTCTime)
listTestGridSessions_endTimeAfter = Lens.lens (\ListTestGridSessions' {endTimeAfter} -> endTimeAfter) (\s@ListTestGridSessions' {} a -> s {endTimeAfter = a} :: ListTestGridSessions) Core.. Lens.mapping Core._Time

-- | Return only sessions that ended before this time.
listTestGridSessions_endTimeBefore :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.UTCTime)
listTestGridSessions_endTimeBefore = Lens.lens (\ListTestGridSessions' {endTimeBefore} -> endTimeBefore) (\s@ListTestGridSessions' {} a -> s {endTimeBefore = a} :: ListTestGridSessions) Core.. Lens.mapping Core._Time

-- | Return only sessions created after this time.
listTestGridSessions_creationTimeAfter :: Lens.Lens' ListTestGridSessions (Core.Maybe Core.UTCTime)
listTestGridSessions_creationTimeAfter = Lens.lens (\ListTestGridSessions' {creationTimeAfter} -> creationTimeAfter) (\s@ListTestGridSessions' {} a -> s {creationTimeAfter = a} :: ListTestGridSessions) Core.. Lens.mapping Core._Time

-- | ARN of a TestGridProject.
listTestGridSessions_projectArn :: Lens.Lens' ListTestGridSessions Core.Text
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
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "testGridSessions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTestGridSessions

instance Core.NFData ListTestGridSessions

instance Core.ToHeaders ListTestGridSessions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridSessions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTestGridSessions where
  toJSON ListTestGridSessions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("maxResult" Core..=) Core.<$> maxResult,
            ("creationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("endTimeAfter" Core..=) Core.<$> endTimeAfter,
            ("endTimeBefore" Core..=) Core.<$> endTimeBefore,
            ("creationTimeAfter" Core..=)
              Core.<$> creationTimeAfter,
            Core.Just ("projectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath ListTestGridSessions where
  toPath = Core.const "/"

instance Core.ToQuery ListTestGridSessions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTestGridSessionsResponse' smart constructor.
data ListTestGridSessionsResponse = ListTestGridSessionsResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The sessions that match the criteria in a ListTestGridSessionsRequest.
    testGridSessions :: Core.Maybe [TestGridSession],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListTestGridSessionsResponse
newListTestGridSessionsResponse pHttpStatus_ =
  ListTestGridSessionsResponse'
    { nextToken =
        Core.Nothing,
      testGridSessions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listTestGridSessionsResponse_nextToken :: Lens.Lens' ListTestGridSessionsResponse (Core.Maybe Core.Text)
listTestGridSessionsResponse_nextToken = Lens.lens (\ListTestGridSessionsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionsResponse)

-- | The sessions that match the criteria in a ListTestGridSessionsRequest.
listTestGridSessionsResponse_testGridSessions :: Lens.Lens' ListTestGridSessionsResponse (Core.Maybe [TestGridSession])
listTestGridSessionsResponse_testGridSessions = Lens.lens (\ListTestGridSessionsResponse' {testGridSessions} -> testGridSessions) (\s@ListTestGridSessionsResponse' {} a -> s {testGridSessions = a} :: ListTestGridSessionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestGridSessionsResponse_httpStatus :: Lens.Lens' ListTestGridSessionsResponse Core.Int
listTestGridSessionsResponse_httpStatus = Lens.lens (\ListTestGridSessionsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionsResponse)

instance Core.NFData ListTestGridSessionsResponse
