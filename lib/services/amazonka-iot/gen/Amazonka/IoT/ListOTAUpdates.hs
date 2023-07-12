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
-- Module      : Amazonka.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListOTAUpdates>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListOTAUpdates
  ( -- * Creating a Request
    ListOTAUpdates (..),
    newListOTAUpdates,

    -- * Request Lenses
    listOTAUpdates_maxResults,
    listOTAUpdates_nextToken,
    listOTAUpdates_otaUpdateStatus,

    -- * Destructuring the Response
    ListOTAUpdatesResponse (..),
    newListOTAUpdatesResponse,

    -- * Response Lenses
    listOTAUpdatesResponse_nextToken,
    listOTAUpdatesResponse_otaUpdates,
    listOTAUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The OTA update job status.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOTAUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOTAUpdates_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listOTAUpdates_nextToken' - A token used to retrieve the next set of results.
--
-- 'otaUpdateStatus', 'listOTAUpdates_otaUpdateStatus' - The OTA update job status.
newListOTAUpdates ::
  ListOTAUpdates
newListOTAUpdates =
  ListOTAUpdates'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      otaUpdateStatus = Prelude.Nothing
    }

-- | The maximum number of results to return at one time.
listOTAUpdates_maxResults :: Lens.Lens' ListOTAUpdates (Prelude.Maybe Prelude.Natural)
listOTAUpdates_maxResults = Lens.lens (\ListOTAUpdates' {maxResults} -> maxResults) (\s@ListOTAUpdates' {} a -> s {maxResults = a} :: ListOTAUpdates)

-- | A token used to retrieve the next set of results.
listOTAUpdates_nextToken :: Lens.Lens' ListOTAUpdates (Prelude.Maybe Prelude.Text)
listOTAUpdates_nextToken = Lens.lens (\ListOTAUpdates' {nextToken} -> nextToken) (\s@ListOTAUpdates' {} a -> s {nextToken = a} :: ListOTAUpdates)

-- | The OTA update job status.
listOTAUpdates_otaUpdateStatus :: Lens.Lens' ListOTAUpdates (Prelude.Maybe OTAUpdateStatus)
listOTAUpdates_otaUpdateStatus = Lens.lens (\ListOTAUpdates' {otaUpdateStatus} -> otaUpdateStatus) (\s@ListOTAUpdates' {} a -> s {otaUpdateStatus = a} :: ListOTAUpdates)

instance Core.AWSPager ListOTAUpdates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOTAUpdatesResponse_otaUpdates
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listOTAUpdates_nextToken
          Lens..~ rs
          Lens.^? listOTAUpdatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListOTAUpdates where
  type
    AWSResponse ListOTAUpdates =
      ListOTAUpdatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOTAUpdatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "otaUpdates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOTAUpdates where
  hashWithSalt _salt ListOTAUpdates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` otaUpdateStatus

instance Prelude.NFData ListOTAUpdates where
  rnf ListOTAUpdates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf otaUpdateStatus

instance Data.ToHeaders ListOTAUpdates where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListOTAUpdates where
  toPath = Prelude.const "/otaUpdates"

instance Data.ToQuery ListOTAUpdates where
  toQuery ListOTAUpdates' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "otaUpdateStatus" Data.=: otaUpdateStatus
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listOTAUpdatesResponse_otaUpdates = Lens.lens (\ListOTAUpdatesResponse' {otaUpdates} -> otaUpdates) (\s@ListOTAUpdatesResponse' {} a -> s {otaUpdates = a} :: ListOTAUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOTAUpdatesResponse_httpStatus :: Lens.Lens' ListOTAUpdatesResponse Prelude.Int
listOTAUpdatesResponse_httpStatus = Lens.lens (\ListOTAUpdatesResponse' {httpStatus} -> httpStatus) (\s@ListOTAUpdatesResponse' {} a -> s {httpStatus = a} :: ListOTAUpdatesResponse)

instance Prelude.NFData ListOTAUpdatesResponse where
  rnf ListOTAUpdatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf otaUpdates
      `Prelude.seq` Prelude.rnf httpStatus
