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
-- Module      : Amazonka.Panorama.ListApplicationInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of application instances.
module Amazonka.Panorama.ListApplicationInstances
  ( -- * Creating a Request
    ListApplicationInstances (..),
    newListApplicationInstances,

    -- * Request Lenses
    listApplicationInstances_deviceId,
    listApplicationInstances_maxResults,
    listApplicationInstances_nextToken,
    listApplicationInstances_statusFilter,

    -- * Destructuring the Response
    ListApplicationInstancesResponse (..),
    newListApplicationInstancesResponse,

    -- * Response Lenses
    listApplicationInstancesResponse_applicationInstances,
    listApplicationInstancesResponse_nextToken,
    listApplicationInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplicationInstances' smart constructor.
data ListApplicationInstances = ListApplicationInstances'
  { -- | The application instances\' device ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of application instances to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Only include instances with a specific status.
    statusFilter :: Prelude.Maybe StatusFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'listApplicationInstances_deviceId' - The application instances\' device ID.
--
-- 'maxResults', 'listApplicationInstances_maxResults' - The maximum number of application instances to return in one page of
-- results.
--
-- 'nextToken', 'listApplicationInstances_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'statusFilter', 'listApplicationInstances_statusFilter' - Only include instances with a specific status.
newListApplicationInstances ::
  ListApplicationInstances
newListApplicationInstances =
  ListApplicationInstances'
    { deviceId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      statusFilter = Prelude.Nothing
    }

-- | The application instances\' device ID.
listApplicationInstances_deviceId :: Lens.Lens' ListApplicationInstances (Prelude.Maybe Prelude.Text)
listApplicationInstances_deviceId = Lens.lens (\ListApplicationInstances' {deviceId} -> deviceId) (\s@ListApplicationInstances' {} a -> s {deviceId = a} :: ListApplicationInstances)

-- | The maximum number of application instances to return in one page of
-- results.
listApplicationInstances_maxResults :: Lens.Lens' ListApplicationInstances (Prelude.Maybe Prelude.Natural)
listApplicationInstances_maxResults = Lens.lens (\ListApplicationInstances' {maxResults} -> maxResults) (\s@ListApplicationInstances' {} a -> s {maxResults = a} :: ListApplicationInstances)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listApplicationInstances_nextToken :: Lens.Lens' ListApplicationInstances (Prelude.Maybe Prelude.Text)
listApplicationInstances_nextToken = Lens.lens (\ListApplicationInstances' {nextToken} -> nextToken) (\s@ListApplicationInstances' {} a -> s {nextToken = a} :: ListApplicationInstances)

-- | Only include instances with a specific status.
listApplicationInstances_statusFilter :: Lens.Lens' ListApplicationInstances (Prelude.Maybe StatusFilter)
listApplicationInstances_statusFilter = Lens.lens (\ListApplicationInstances' {statusFilter} -> statusFilter) (\s@ListApplicationInstances' {} a -> s {statusFilter = a} :: ListApplicationInstances)

instance Core.AWSRequest ListApplicationInstances where
  type
    AWSResponse ListApplicationInstances =
      ListApplicationInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationInstancesResponse'
            Prelude.<$> ( x Data..?> "ApplicationInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationInstances where
  hashWithSalt _salt ListApplicationInstances' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` statusFilter

instance Prelude.NFData ListApplicationInstances where
  rnf ListApplicationInstances' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statusFilter

instance Data.ToHeaders ListApplicationInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApplicationInstances where
  toPath = Prelude.const "/application-instances"

instance Data.ToQuery ListApplicationInstances where
  toQuery ListApplicationInstances' {..} =
    Prelude.mconcat
      [ "deviceId" Data.=: deviceId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "statusFilter" Data.=: statusFilter
      ]

-- | /See:/ 'newListApplicationInstancesResponse' smart constructor.
data ListApplicationInstancesResponse = ListApplicationInstancesResponse'
  { -- | A list of application instances.
    applicationInstances :: Prelude.Maybe [ApplicationInstance],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstances', 'listApplicationInstancesResponse_applicationInstances' - A list of application instances.
--
-- 'nextToken', 'listApplicationInstancesResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listApplicationInstancesResponse_httpStatus' - The response's http status code.
newListApplicationInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationInstancesResponse
newListApplicationInstancesResponse pHttpStatus_ =
  ListApplicationInstancesResponse'
    { applicationInstances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of application instances.
listApplicationInstancesResponse_applicationInstances :: Lens.Lens' ListApplicationInstancesResponse (Prelude.Maybe [ApplicationInstance])
listApplicationInstancesResponse_applicationInstances = Lens.lens (\ListApplicationInstancesResponse' {applicationInstances} -> applicationInstances) (\s@ListApplicationInstancesResponse' {} a -> s {applicationInstances = a} :: ListApplicationInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listApplicationInstancesResponse_nextToken :: Lens.Lens' ListApplicationInstancesResponse (Prelude.Maybe Prelude.Text)
listApplicationInstancesResponse_nextToken = Lens.lens (\ListApplicationInstancesResponse' {nextToken} -> nextToken) (\s@ListApplicationInstancesResponse' {} a -> s {nextToken = a} :: ListApplicationInstancesResponse)

-- | The response's http status code.
listApplicationInstancesResponse_httpStatus :: Lens.Lens' ListApplicationInstancesResponse Prelude.Int
listApplicationInstancesResponse_httpStatus = Lens.lens (\ListApplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@ListApplicationInstancesResponse' {} a -> s {httpStatus = a} :: ListApplicationInstancesResponse)

instance
  Prelude.NFData
    ListApplicationInstancesResponse
  where
  rnf ListApplicationInstancesResponse' {..} =
    Prelude.rnf applicationInstances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
