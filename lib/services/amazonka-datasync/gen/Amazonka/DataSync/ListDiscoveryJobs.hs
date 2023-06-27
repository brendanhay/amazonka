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
-- Module      : Amazonka.DataSync.ListDiscoveryJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the existing discovery jobs in the Amazon Web
-- Services Region and Amazon Web Services account where you\'re using
-- DataSync Discovery.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListDiscoveryJobs
  ( -- * Creating a Request
    ListDiscoveryJobs (..),
    newListDiscoveryJobs,

    -- * Request Lenses
    listDiscoveryJobs_maxResults,
    listDiscoveryJobs_nextToken,
    listDiscoveryJobs_storageSystemArn,

    -- * Destructuring the Response
    ListDiscoveryJobsResponse (..),
    newListDiscoveryJobsResponse,

    -- * Response Lenses
    listDiscoveryJobsResponse_discoveryJobs,
    listDiscoveryJobsResponse_nextToken,
    listDiscoveryJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDiscoveryJobs' smart constructor.
data ListDiscoveryJobs = ListDiscoveryJobs'
  { -- | Specifies how many results you want in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an opaque string that indicates the position to begin the next
    -- list of results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Resource Name (ARN) of an on-premises storage
    -- system. Use this parameter if you only want to list the discovery jobs
    -- that are associated with a specific storage system.
    storageSystemArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveryJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDiscoveryJobs_maxResults' - Specifies how many results you want in the response.
--
-- 'nextToken', 'listDiscoveryJobs_nextToken' - Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
--
-- 'storageSystemArn', 'listDiscoveryJobs_storageSystemArn' - Specifies the Amazon Resource Name (ARN) of an on-premises storage
-- system. Use this parameter if you only want to list the discovery jobs
-- that are associated with a specific storage system.
newListDiscoveryJobs ::
  ListDiscoveryJobs
newListDiscoveryJobs =
  ListDiscoveryJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      storageSystemArn = Prelude.Nothing
    }

-- | Specifies how many results you want in the response.
listDiscoveryJobs_maxResults :: Lens.Lens' ListDiscoveryJobs (Prelude.Maybe Prelude.Natural)
listDiscoveryJobs_maxResults = Lens.lens (\ListDiscoveryJobs' {maxResults} -> maxResults) (\s@ListDiscoveryJobs' {} a -> s {maxResults = a} :: ListDiscoveryJobs)

-- | Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
listDiscoveryJobs_nextToken :: Lens.Lens' ListDiscoveryJobs (Prelude.Maybe Prelude.Text)
listDiscoveryJobs_nextToken = Lens.lens (\ListDiscoveryJobs' {nextToken} -> nextToken) (\s@ListDiscoveryJobs' {} a -> s {nextToken = a} :: ListDiscoveryJobs)

-- | Specifies the Amazon Resource Name (ARN) of an on-premises storage
-- system. Use this parameter if you only want to list the discovery jobs
-- that are associated with a specific storage system.
listDiscoveryJobs_storageSystemArn :: Lens.Lens' ListDiscoveryJobs (Prelude.Maybe Prelude.Text)
listDiscoveryJobs_storageSystemArn = Lens.lens (\ListDiscoveryJobs' {storageSystemArn} -> storageSystemArn) (\s@ListDiscoveryJobs' {} a -> s {storageSystemArn = a} :: ListDiscoveryJobs)

instance Core.AWSPager ListDiscoveryJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoveryJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoveryJobsResponse_discoveryJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDiscoveryJobs_nextToken
          Lens..~ rs
          Lens.^? listDiscoveryJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDiscoveryJobs where
  type
    AWSResponse ListDiscoveryJobs =
      ListDiscoveryJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveryJobsResponse'
            Prelude.<$> (x Data..?> "DiscoveryJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoveryJobs where
  hashWithSalt _salt ListDiscoveryJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` storageSystemArn

instance Prelude.NFData ListDiscoveryJobs where
  rnf ListDiscoveryJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storageSystemArn

instance Data.ToHeaders ListDiscoveryJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.ListDiscoveryJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDiscoveryJobs where
  toJSON ListDiscoveryJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StorageSystemArn" Data..=)
              Prelude.<$> storageSystemArn
          ]
      )

instance Data.ToPath ListDiscoveryJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDiscoveryJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDiscoveryJobsResponse' smart constructor.
data ListDiscoveryJobsResponse = ListDiscoveryJobsResponse'
  { -- | The discovery jobs that you\'ve run.
    discoveryJobs :: Prelude.Maybe [DiscoveryJobListEntry],
    -- | The opaque string that indicates the position to begin the next list of
    -- results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveryJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveryJobs', 'listDiscoveryJobsResponse_discoveryJobs' - The discovery jobs that you\'ve run.
--
-- 'nextToken', 'listDiscoveryJobsResponse_nextToken' - The opaque string that indicates the position to begin the next list of
-- results in the response.
--
-- 'httpStatus', 'listDiscoveryJobsResponse_httpStatus' - The response's http status code.
newListDiscoveryJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDiscoveryJobsResponse
newListDiscoveryJobsResponse pHttpStatus_ =
  ListDiscoveryJobsResponse'
    { discoveryJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The discovery jobs that you\'ve run.
listDiscoveryJobsResponse_discoveryJobs :: Lens.Lens' ListDiscoveryJobsResponse (Prelude.Maybe [DiscoveryJobListEntry])
listDiscoveryJobsResponse_discoveryJobs = Lens.lens (\ListDiscoveryJobsResponse' {discoveryJobs} -> discoveryJobs) (\s@ListDiscoveryJobsResponse' {} a -> s {discoveryJobs = a} :: ListDiscoveryJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The opaque string that indicates the position to begin the next list of
-- results in the response.
listDiscoveryJobsResponse_nextToken :: Lens.Lens' ListDiscoveryJobsResponse (Prelude.Maybe Prelude.Text)
listDiscoveryJobsResponse_nextToken = Lens.lens (\ListDiscoveryJobsResponse' {nextToken} -> nextToken) (\s@ListDiscoveryJobsResponse' {} a -> s {nextToken = a} :: ListDiscoveryJobsResponse)

-- | The response's http status code.
listDiscoveryJobsResponse_httpStatus :: Lens.Lens' ListDiscoveryJobsResponse Prelude.Int
listDiscoveryJobsResponse_httpStatus = Lens.lens (\ListDiscoveryJobsResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveryJobsResponse' {} a -> s {httpStatus = a} :: ListDiscoveryJobsResponse)

instance Prelude.NFData ListDiscoveryJobsResponse where
  rnf ListDiscoveryJobsResponse' {..} =
    Prelude.rnf discoveryJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
