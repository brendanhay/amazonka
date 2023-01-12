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
-- Module      : Amazonka.Panorama.ListDevicesJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of jobs.
module Amazonka.Panorama.ListDevicesJobs
  ( -- * Creating a Request
    ListDevicesJobs (..),
    newListDevicesJobs,

    -- * Request Lenses
    listDevicesJobs_deviceId,
    listDevicesJobs_maxResults,
    listDevicesJobs_nextToken,

    -- * Destructuring the Response
    ListDevicesJobsResponse (..),
    newListDevicesJobsResponse,

    -- * Response Lenses
    listDevicesJobsResponse_deviceJobs,
    listDevicesJobsResponse_nextToken,
    listDevicesJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevicesJobs' smart constructor.
data ListDevicesJobs = ListDevicesJobs'
  { -- | Filter results by the job\'s target device ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of device jobs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'listDevicesJobs_deviceId' - Filter results by the job\'s target device ID.
--
-- 'maxResults', 'listDevicesJobs_maxResults' - The maximum number of device jobs to return in one page of results.
--
-- 'nextToken', 'listDevicesJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListDevicesJobs ::
  ListDevicesJobs
newListDevicesJobs =
  ListDevicesJobs'
    { deviceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filter results by the job\'s target device ID.
listDevicesJobs_deviceId :: Lens.Lens' ListDevicesJobs (Prelude.Maybe Prelude.Text)
listDevicesJobs_deviceId = Lens.lens (\ListDevicesJobs' {deviceId} -> deviceId) (\s@ListDevicesJobs' {} a -> s {deviceId = a} :: ListDevicesJobs)

-- | The maximum number of device jobs to return in one page of results.
listDevicesJobs_maxResults :: Lens.Lens' ListDevicesJobs (Prelude.Maybe Prelude.Natural)
listDevicesJobs_maxResults = Lens.lens (\ListDevicesJobs' {maxResults} -> maxResults) (\s@ListDevicesJobs' {} a -> s {maxResults = a} :: ListDevicesJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listDevicesJobs_nextToken :: Lens.Lens' ListDevicesJobs (Prelude.Maybe Prelude.Text)
listDevicesJobs_nextToken = Lens.lens (\ListDevicesJobs' {nextToken} -> nextToken) (\s@ListDevicesJobs' {} a -> s {nextToken = a} :: ListDevicesJobs)

instance Core.AWSRequest ListDevicesJobs where
  type
    AWSResponse ListDevicesJobs =
      ListDevicesJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesJobsResponse'
            Prelude.<$> (x Data..?> "DeviceJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevicesJobs where
  hashWithSalt _salt ListDevicesJobs' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDevicesJobs where
  rnf ListDevicesJobs' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDevicesJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDevicesJobs where
  toPath = Prelude.const "/jobs"

instance Data.ToQuery ListDevicesJobs where
  toQuery ListDevicesJobs' {..} =
    Prelude.mconcat
      [ "DeviceId" Data.=: deviceId,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDevicesJobsResponse' smart constructor.
data ListDevicesJobsResponse = ListDevicesJobsResponse'
  { -- | A list of jobs.
    deviceJobs :: Prelude.Maybe [DeviceJob],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceJobs', 'listDevicesJobsResponse_deviceJobs' - A list of jobs.
--
-- 'nextToken', 'listDevicesJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listDevicesJobsResponse_httpStatus' - The response's http status code.
newListDevicesJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesJobsResponse
newListDevicesJobsResponse pHttpStatus_ =
  ListDevicesJobsResponse'
    { deviceJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of jobs.
listDevicesJobsResponse_deviceJobs :: Lens.Lens' ListDevicesJobsResponse (Prelude.Maybe [DeviceJob])
listDevicesJobsResponse_deviceJobs = Lens.lens (\ListDevicesJobsResponse' {deviceJobs} -> deviceJobs) (\s@ListDevicesJobsResponse' {} a -> s {deviceJobs = a} :: ListDevicesJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listDevicesJobsResponse_nextToken :: Lens.Lens' ListDevicesJobsResponse (Prelude.Maybe Prelude.Text)
listDevicesJobsResponse_nextToken = Lens.lens (\ListDevicesJobsResponse' {nextToken} -> nextToken) (\s@ListDevicesJobsResponse' {} a -> s {nextToken = a} :: ListDevicesJobsResponse)

-- | The response's http status code.
listDevicesJobsResponse_httpStatus :: Lens.Lens' ListDevicesJobsResponse Prelude.Int
listDevicesJobsResponse_httpStatus = Lens.lens (\ListDevicesJobsResponse' {httpStatus} -> httpStatus) (\s@ListDevicesJobsResponse' {} a -> s {httpStatus = a} :: ListDevicesJobsResponse)

instance Prelude.NFData ListDevicesJobsResponse where
  rnf ListDevicesJobsResponse' {..} =
    Prelude.rnf deviceJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
