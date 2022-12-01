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
-- Module      : Amazonka.SSMContacts.ListEngagements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all engagements that have happened in an incident.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListEngagements
  ( -- * Creating a Request
    ListEngagements (..),
    newListEngagements,

    -- * Request Lenses
    listEngagements_nextToken,
    listEngagements_timeRangeValue,
    listEngagements_maxResults,
    listEngagements_incidentId,

    -- * Destructuring the Response
    ListEngagementsResponse (..),
    newListEngagementsResponse,

    -- * Response Lenses
    listEngagementsResponse_nextToken,
    listEngagementsResponse_httpStatus,
    listEngagementsResponse_engagements,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListEngagements' smart constructor.
data ListEngagements = ListEngagements'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time range to lists engagements for an incident.
    timeRangeValue :: Prelude.Maybe TimeRange,
    -- | The maximum number of engagements per page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the incident you\'re listing
    -- engagements for.
    incidentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEngagements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEngagements_nextToken' - The pagination token to continue to the next page of results.
--
-- 'timeRangeValue', 'listEngagements_timeRangeValue' - The time range to lists engagements for an incident.
--
-- 'maxResults', 'listEngagements_maxResults' - The maximum number of engagements per page of results.
--
-- 'incidentId', 'listEngagements_incidentId' - The Amazon Resource Name (ARN) of the incident you\'re listing
-- engagements for.
newListEngagements ::
  ListEngagements
newListEngagements =
  ListEngagements'
    { nextToken = Prelude.Nothing,
      timeRangeValue = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      incidentId = Prelude.Nothing
    }

-- | The pagination token to continue to the next page of results.
listEngagements_nextToken :: Lens.Lens' ListEngagements (Prelude.Maybe Prelude.Text)
listEngagements_nextToken = Lens.lens (\ListEngagements' {nextToken} -> nextToken) (\s@ListEngagements' {} a -> s {nextToken = a} :: ListEngagements)

-- | The time range to lists engagements for an incident.
listEngagements_timeRangeValue :: Lens.Lens' ListEngagements (Prelude.Maybe TimeRange)
listEngagements_timeRangeValue = Lens.lens (\ListEngagements' {timeRangeValue} -> timeRangeValue) (\s@ListEngagements' {} a -> s {timeRangeValue = a} :: ListEngagements)

-- | The maximum number of engagements per page of results.
listEngagements_maxResults :: Lens.Lens' ListEngagements (Prelude.Maybe Prelude.Natural)
listEngagements_maxResults = Lens.lens (\ListEngagements' {maxResults} -> maxResults) (\s@ListEngagements' {} a -> s {maxResults = a} :: ListEngagements)

-- | The Amazon Resource Name (ARN) of the incident you\'re listing
-- engagements for.
listEngagements_incidentId :: Lens.Lens' ListEngagements (Prelude.Maybe Prelude.Text)
listEngagements_incidentId = Lens.lens (\ListEngagements' {incidentId} -> incidentId) (\s@ListEngagements' {} a -> s {incidentId = a} :: ListEngagements)

instance Core.AWSPager ListEngagements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEngagementsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listEngagementsResponse_engagements) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEngagements_nextToken
          Lens..~ rs
          Lens.^? listEngagementsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEngagements where
  type
    AWSResponse ListEngagements =
      ListEngagementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEngagementsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Engagements" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEngagements where
  hashWithSalt _salt ListEngagements' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` timeRangeValue
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` incidentId

instance Prelude.NFData ListEngagements where
  rnf ListEngagements' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeRangeValue
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf incidentId

instance Core.ToHeaders ListEngagements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SSMContacts.ListEngagements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEngagements where
  toJSON ListEngagements' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("TimeRangeValue" Core..=)
              Prelude.<$> timeRangeValue,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("IncidentId" Core..=) Prelude.<$> incidentId
          ]
      )

instance Core.ToPath ListEngagements where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEngagements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEngagementsResponse' smart constructor.
data ListEngagementsResponse = ListEngagementsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of each engagement that occurred during the specified time range
    -- of an incident.
    engagements :: [Engagement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEngagementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEngagementsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listEngagementsResponse_httpStatus' - The response's http status code.
--
-- 'engagements', 'listEngagementsResponse_engagements' - A list of each engagement that occurred during the specified time range
-- of an incident.
newListEngagementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEngagementsResponse
newListEngagementsResponse pHttpStatus_ =
  ListEngagementsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      engagements = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listEngagementsResponse_nextToken :: Lens.Lens' ListEngagementsResponse (Prelude.Maybe Prelude.Text)
listEngagementsResponse_nextToken = Lens.lens (\ListEngagementsResponse' {nextToken} -> nextToken) (\s@ListEngagementsResponse' {} a -> s {nextToken = a} :: ListEngagementsResponse)

-- | The response's http status code.
listEngagementsResponse_httpStatus :: Lens.Lens' ListEngagementsResponse Prelude.Int
listEngagementsResponse_httpStatus = Lens.lens (\ListEngagementsResponse' {httpStatus} -> httpStatus) (\s@ListEngagementsResponse' {} a -> s {httpStatus = a} :: ListEngagementsResponse)

-- | A list of each engagement that occurred during the specified time range
-- of an incident.
listEngagementsResponse_engagements :: Lens.Lens' ListEngagementsResponse [Engagement]
listEngagementsResponse_engagements = Lens.lens (\ListEngagementsResponse' {engagements} -> engagements) (\s@ListEngagementsResponse' {} a -> s {engagements = a} :: ListEngagementsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEngagementsResponse where
  rnf ListEngagementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf engagements
