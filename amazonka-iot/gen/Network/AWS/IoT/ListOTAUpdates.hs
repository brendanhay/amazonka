{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { -- | The OTA update job status.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus,
    -- | A token used to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { otaUpdateStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The OTA update job status.
listOTAUpdates_otaUpdateStatus :: Lens.Lens' ListOTAUpdates (Prelude.Maybe OTAUpdateStatus)
listOTAUpdates_otaUpdateStatus = Lens.lens (\ListOTAUpdates' {otaUpdateStatus} -> otaUpdateStatus) (\s@ListOTAUpdates' {} a -> s {otaUpdateStatus = a} :: ListOTAUpdates)

-- | A token used to retrieve the next set of results.
listOTAUpdates_nextToken :: Lens.Lens' ListOTAUpdates (Prelude.Maybe Prelude.Text)
listOTAUpdates_nextToken = Lens.lens (\ListOTAUpdates' {nextToken} -> nextToken) (\s@ListOTAUpdates' {} a -> s {nextToken = a} :: ListOTAUpdates)

-- | The maximum number of results to return at one time.
listOTAUpdates_maxResults :: Lens.Lens' ListOTAUpdates (Prelude.Maybe Prelude.Natural)
listOTAUpdates_maxResults = Lens.lens (\ListOTAUpdates' {maxResults} -> maxResults) (\s@ListOTAUpdates' {} a -> s {maxResults = a} :: ListOTAUpdates)

instance Pager.AWSPager ListOTAUpdates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_otaUpdates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listOTAUpdates_nextToken
          Lens..~ rs
          Lens.^? listOTAUpdatesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListOTAUpdates where
  type Rs ListOTAUpdates = ListOTAUpdatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOTAUpdatesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "otaUpdates"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOTAUpdates

instance Prelude.NFData ListOTAUpdates

instance Prelude.ToHeaders ListOTAUpdates where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListOTAUpdates where
  toPath = Prelude.const "/otaUpdates"

instance Prelude.ToQuery ListOTAUpdates where
  toQuery ListOTAUpdates' {..} =
    Prelude.mconcat
      [ "otaUpdateStatus" Prelude.=: otaUpdateStatus,
        "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { -- | A token to use to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of OTA update jobs.
    otaUpdates :: Prelude.Maybe [OTAUpdateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListOTAUpdatesResponse
newListOTAUpdatesResponse pHttpStatus_ =
  ListOTAUpdatesResponse'
    { nextToken =
        Prelude.Nothing,
      otaUpdates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to use to get the next set of results.
listOTAUpdatesResponse_nextToken :: Lens.Lens' ListOTAUpdatesResponse (Prelude.Maybe Prelude.Text)
listOTAUpdatesResponse_nextToken = Lens.lens (\ListOTAUpdatesResponse' {nextToken} -> nextToken) (\s@ListOTAUpdatesResponse' {} a -> s {nextToken = a} :: ListOTAUpdatesResponse)

-- | A list of OTA update jobs.
listOTAUpdatesResponse_otaUpdates :: Lens.Lens' ListOTAUpdatesResponse (Prelude.Maybe [OTAUpdateSummary])
listOTAUpdatesResponse_otaUpdates = Lens.lens (\ListOTAUpdatesResponse' {otaUpdates} -> otaUpdates) (\s@ListOTAUpdatesResponse' {} a -> s {otaUpdates = a} :: ListOTAUpdatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listOTAUpdatesResponse_httpStatus :: Lens.Lens' ListOTAUpdatesResponse Prelude.Int
listOTAUpdatesResponse_httpStatus = Lens.lens (\ListOTAUpdatesResponse' {httpStatus} -> httpStatus) (\s@ListOTAUpdatesResponse' {} a -> s {httpStatus = a} :: ListOTAUpdatesResponse)

instance Prelude.NFData ListOTAUpdatesResponse
