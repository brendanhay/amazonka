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
-- Module      : Network.AWS.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOTAUpdates
  ( -- * Creating a Request
    ListOTAUpdates (..),
    newListOTAUpdates,

    -- * Request Lenses
    listOTAUpdates_otaUpdateStatus,
    listOTAUpdates_nextToken,
    listOTAUpdates_maxResults,

    -- * Destructuring the Response
    ListOTAUpdatesResponse (..),
    newListOTAUpdatesResponse,

    -- * Response Lenses
    listOTAUpdatesResponse_nextToken,
    listOTAUpdatesResponse_otaUpdates,
    listOTAUpdatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { -- | The OTA update job status.
    otaUpdateStatus :: Core.Maybe OTAUpdateStatus,
    -- | A token used to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOTAUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateStatus', 'listOTAUpdates_otaUpdateStatus' - The OTA update job status.
--
-- 'nextToken', 'listOTAUpdates_nextToken' - A token used to retrieve the next set of results.
--
-- 'maxResults', 'listOTAUpdates_maxResults' - The maximum number of results to return at one time.
newListOTAUpdates ::
  ListOTAUpdates
newListOTAUpdates =
  ListOTAUpdates'
    { otaUpdateStatus = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The OTA update job status.
listOTAUpdates_otaUpdateStatus :: Lens.Lens' ListOTAUpdates (Core.Maybe OTAUpdateStatus)
listOTAUpdates_otaUpdateStatus = Lens.lens (\ListOTAUpdates' {otaUpdateStatus} -> otaUpdateStatus) (\s@ListOTAUpdates' {} a -> s {otaUpdateStatus = a} :: ListOTAUpdates)

-- | A token used to retrieve the next set of results.
listOTAUpdates_nextToken :: Lens.Lens' ListOTAUpdates (Core.Maybe Core.Text)
listOTAUpdates_nextToken = Lens.lens (\ListOTAUpdates' {nextToken} -> nextToken) (\s@ListOTAUpdates' {} a -> s {nextToken = a} :: ListOTAUpdates)

-- | The maximum number of results to return at one time.
listOTAUpdates_maxResults :: Lens.Lens' ListOTAUpdates (Core.Maybe Core.Natural)
listOTAUpdates_maxResults = Lens.lens (\ListOTAUpdates' {maxResults} -> maxResults) (\s@ListOTAUpdates' {} a -> s {maxResults = a} :: ListOTAUpdates)

instance Core.AWSPager ListOTAUpdates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_otaUpdates Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOTAUpdates_nextToken
          Lens..~ rs
          Lens.^? listOTAUpdatesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListOTAUpdates where
  type
    AWSResponse ListOTAUpdates =
      ListOTAUpdatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOTAUpdatesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "otaUpdates" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOTAUpdates

instance Core.NFData ListOTAUpdates

instance Core.ToHeaders ListOTAUpdates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListOTAUpdates where
  toPath = Core.const "/otaUpdates"

instance Core.ToQuery ListOTAUpdates where
  toQuery ListOTAUpdates' {..} =
    Core.mconcat
      [ "otaUpdateStatus" Core.=: otaUpdateStatus,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { -- | A token to use to get the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of OTA update jobs.
    otaUpdates :: Core.Maybe [OTAUpdateSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOTAUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOTAUpdatesResponse_nextToken' - A token to use to get the next set of results.
--
-- 'otaUpdates', 'listOTAUpdatesResponse_otaUpdates' - A list of OTA update jobs.
--
-- 'httpStatus', 'listOTAUpdatesResponse_httpStatus' - The response's http status code.
newListOTAUpdatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOTAUpdatesResponse
newListOTAUpdatesResponse pHttpStatus_ =
  ListOTAUpdatesResponse'
    { nextToken = Core.Nothing,
      otaUpdates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to use to get the next set of results.
listOTAUpdatesResponse_nextToken :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe Core.Text)
listOTAUpdatesResponse_nextToken = Lens.lens (\ListOTAUpdatesResponse' {nextToken} -> nextToken) (\s@ListOTAUpdatesResponse' {} a -> s {nextToken = a} :: ListOTAUpdatesResponse)

-- | A list of OTA update jobs.
listOTAUpdatesResponse_otaUpdates :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe [OTAUpdateSummary])
listOTAUpdatesResponse_otaUpdates = Lens.lens (\ListOTAUpdatesResponse' {otaUpdates} -> otaUpdates) (\s@ListOTAUpdatesResponse' {} a -> s {otaUpdates = a} :: ListOTAUpdatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOTAUpdatesResponse_httpStatus :: Lens.Lens' ListOTAUpdatesResponse Core.Int
listOTAUpdatesResponse_httpStatus = Lens.lens (\ListOTAUpdatesResponse' {httpStatus} -> httpStatus) (\s@ListOTAUpdatesResponse' {} a -> s {httpStatus = a} :: ListOTAUpdatesResponse)

instance Core.NFData ListOTAUpdatesResponse
