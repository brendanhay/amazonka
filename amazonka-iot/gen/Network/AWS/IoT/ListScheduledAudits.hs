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
-- Module      : Network.AWS.IoT.ListScheduledAudits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your scheduled audits.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListScheduledAudits
  ( -- * Creating a Request
    ListScheduledAudits (..),
    newListScheduledAudits,

    -- * Request Lenses
    listScheduledAudits_nextToken,
    listScheduledAudits_maxResults,

    -- * Destructuring the Response
    ListScheduledAuditsResponse (..),
    newListScheduledAuditsResponse,

    -- * Response Lenses
    listScheduledAuditsResponse_nextToken,
    listScheduledAuditsResponse_scheduledAudits,
    listScheduledAuditsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListScheduledAudits' smart constructor.
data ListScheduledAudits = ListScheduledAudits'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListScheduledAudits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduledAudits_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listScheduledAudits_maxResults' - The maximum number of results to return at one time. The default is 25.
newListScheduledAudits ::
  ListScheduledAudits
newListScheduledAudits =
  ListScheduledAudits'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token for the next set of results.
listScheduledAudits_nextToken :: Lens.Lens' ListScheduledAudits (Core.Maybe Core.Text)
listScheduledAudits_nextToken = Lens.lens (\ListScheduledAudits' {nextToken} -> nextToken) (\s@ListScheduledAudits' {} a -> s {nextToken = a} :: ListScheduledAudits)

-- | The maximum number of results to return at one time. The default is 25.
listScheduledAudits_maxResults :: Lens.Lens' ListScheduledAudits (Core.Maybe Core.Natural)
listScheduledAudits_maxResults = Lens.lens (\ListScheduledAudits' {maxResults} -> maxResults) (\s@ListScheduledAudits' {} a -> s {maxResults = a} :: ListScheduledAudits)

instance Core.AWSPager ListScheduledAudits where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScheduledAuditsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listScheduledAuditsResponse_scheduledAudits
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listScheduledAudits_nextToken
          Lens..~ rs
          Lens.^? listScheduledAuditsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListScheduledAudits where
  type
    AWSResponse ListScheduledAudits =
      ListScheduledAuditsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduledAuditsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "scheduledAudits" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListScheduledAudits

instance Core.NFData ListScheduledAudits

instance Core.ToHeaders ListScheduledAudits where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListScheduledAudits where
  toPath = Core.const "/audit/scheduledaudits"

instance Core.ToQuery ListScheduledAudits where
  toQuery ListScheduledAudits' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListScheduledAuditsResponse' smart constructor.
data ListScheduledAuditsResponse = ListScheduledAuditsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of scheduled audits.
    scheduledAudits :: Core.Maybe [ScheduledAuditMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListScheduledAuditsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduledAuditsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'scheduledAudits', 'listScheduledAuditsResponse_scheduledAudits' - The list of scheduled audits.
--
-- 'httpStatus', 'listScheduledAuditsResponse_httpStatus' - The response's http status code.
newListScheduledAuditsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListScheduledAuditsResponse
newListScheduledAuditsResponse pHttpStatus_ =
  ListScheduledAuditsResponse'
    { nextToken =
        Core.Nothing,
      scheduledAudits = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listScheduledAuditsResponse_nextToken :: Lens.Lens' ListScheduledAuditsResponse (Core.Maybe Core.Text)
listScheduledAuditsResponse_nextToken = Lens.lens (\ListScheduledAuditsResponse' {nextToken} -> nextToken) (\s@ListScheduledAuditsResponse' {} a -> s {nextToken = a} :: ListScheduledAuditsResponse)

-- | The list of scheduled audits.
listScheduledAuditsResponse_scheduledAudits :: Lens.Lens' ListScheduledAuditsResponse (Core.Maybe [ScheduledAuditMetadata])
listScheduledAuditsResponse_scheduledAudits = Lens.lens (\ListScheduledAuditsResponse' {scheduledAudits} -> scheduledAudits) (\s@ListScheduledAuditsResponse' {} a -> s {scheduledAudits = a} :: ListScheduledAuditsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listScheduledAuditsResponse_httpStatus :: Lens.Lens' ListScheduledAuditsResponse Core.Int
listScheduledAuditsResponse_httpStatus = Lens.lens (\ListScheduledAuditsResponse' {httpStatus} -> httpStatus) (\s@ListScheduledAuditsResponse' {} a -> s {httpStatus = a} :: ListScheduledAuditsResponse)

instance Core.NFData ListScheduledAuditsResponse
