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
-- Module      : Amazonka.Comprehend.ListFlywheels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the flywheels that you have created.
module Amazonka.Comprehend.ListFlywheels
  ( -- * Creating a Request
    ListFlywheels (..),
    newListFlywheels,

    -- * Request Lenses
    listFlywheels_filter,
    listFlywheels_maxResults,
    listFlywheels_nextToken,

    -- * Destructuring the Response
    ListFlywheelsResponse (..),
    newListFlywheelsResponse,

    -- * Response Lenses
    listFlywheelsResponse_flywheelSummaryList,
    listFlywheelsResponse_nextToken,
    listFlywheelsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFlywheels' smart constructor.
data ListFlywheels = ListFlywheels'
  { -- | Filters the flywheels that are returned. You can filter flywheels on
    -- their status, or the date and time that they were submitted. You can
    -- only set one filter at a time.
    filter' :: Prelude.Maybe FlywheelFilter,
    -- | Maximum number of results to return in a response. The default is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlywheels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listFlywheels_filter' - Filters the flywheels that are returned. You can filter flywheels on
-- their status, or the date and time that they were submitted. You can
-- only set one filter at a time.
--
-- 'maxResults', 'listFlywheels_maxResults' - Maximum number of results to return in a response. The default is 100.
--
-- 'nextToken', 'listFlywheels_nextToken' - Identifies the next page of results to return.
newListFlywheels ::
  ListFlywheels
newListFlywheels =
  ListFlywheels'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the flywheels that are returned. You can filter flywheels on
-- their status, or the date and time that they were submitted. You can
-- only set one filter at a time.
listFlywheels_filter :: Lens.Lens' ListFlywheels (Prelude.Maybe FlywheelFilter)
listFlywheels_filter = Lens.lens (\ListFlywheels' {filter'} -> filter') (\s@ListFlywheels' {} a -> s {filter' = a} :: ListFlywheels)

-- | Maximum number of results to return in a response. The default is 100.
listFlywheels_maxResults :: Lens.Lens' ListFlywheels (Prelude.Maybe Prelude.Natural)
listFlywheels_maxResults = Lens.lens (\ListFlywheels' {maxResults} -> maxResults) (\s@ListFlywheels' {} a -> s {maxResults = a} :: ListFlywheels)

-- | Identifies the next page of results to return.
listFlywheels_nextToken :: Lens.Lens' ListFlywheels (Prelude.Maybe Prelude.Text)
listFlywheels_nextToken = Lens.lens (\ListFlywheels' {nextToken} -> nextToken) (\s@ListFlywheels' {} a -> s {nextToken = a} :: ListFlywheels)

instance Core.AWSRequest ListFlywheels where
  type
    AWSResponse ListFlywheels =
      ListFlywheelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlywheelsResponse'
            Prelude.<$> ( x
                            Data..?> "FlywheelSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFlywheels where
  hashWithSalt _salt ListFlywheels' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFlywheels where
  rnf ListFlywheels' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFlywheels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListFlywheels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFlywheels where
  toJSON ListFlywheels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListFlywheels where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFlywheels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFlywheelsResponse' smart constructor.
data ListFlywheelsResponse = ListFlywheelsResponse'
  { -- | A list of flywheel properties retrieved by the service in response to
    -- the request.
    flywheelSummaryList :: Prelude.Maybe [FlywheelSummary],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlywheelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelSummaryList', 'listFlywheelsResponse_flywheelSummaryList' - A list of flywheel properties retrieved by the service in response to
-- the request.
--
-- 'nextToken', 'listFlywheelsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listFlywheelsResponse_httpStatus' - The response's http status code.
newListFlywheelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlywheelsResponse
newListFlywheelsResponse pHttpStatus_ =
  ListFlywheelsResponse'
    { flywheelSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of flywheel properties retrieved by the service in response to
-- the request.
listFlywheelsResponse_flywheelSummaryList :: Lens.Lens' ListFlywheelsResponse (Prelude.Maybe [FlywheelSummary])
listFlywheelsResponse_flywheelSummaryList = Lens.lens (\ListFlywheelsResponse' {flywheelSummaryList} -> flywheelSummaryList) (\s@ListFlywheelsResponse' {} a -> s {flywheelSummaryList = a} :: ListFlywheelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listFlywheelsResponse_nextToken :: Lens.Lens' ListFlywheelsResponse (Prelude.Maybe Prelude.Text)
listFlywheelsResponse_nextToken = Lens.lens (\ListFlywheelsResponse' {nextToken} -> nextToken) (\s@ListFlywheelsResponse' {} a -> s {nextToken = a} :: ListFlywheelsResponse)

-- | The response's http status code.
listFlywheelsResponse_httpStatus :: Lens.Lens' ListFlywheelsResponse Prelude.Int
listFlywheelsResponse_httpStatus = Lens.lens (\ListFlywheelsResponse' {httpStatus} -> httpStatus) (\s@ListFlywheelsResponse' {} a -> s {httpStatus = a} :: ListFlywheelsResponse)

instance Prelude.NFData ListFlywheelsResponse where
  rnf ListFlywheelsResponse' {..} =
    Prelude.rnf flywheelSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
