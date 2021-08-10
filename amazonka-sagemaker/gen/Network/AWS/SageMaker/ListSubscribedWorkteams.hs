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
-- Module      : Network.AWS.SageMaker.ListSubscribedWorkteams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the work teams that you are subscribed to in the AWS
-- Marketplace. The list may be empty if no work team satisfies the filter
-- specified in the @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListSubscribedWorkteams
  ( -- * Creating a Request
    ListSubscribedWorkteams (..),
    newListSubscribedWorkteams,

    -- * Request Lenses
    listSubscribedWorkteams_nextToken,
    listSubscribedWorkteams_nameContains,
    listSubscribedWorkteams_maxResults,

    -- * Destructuring the Response
    ListSubscribedWorkteamsResponse (..),
    newListSubscribedWorkteamsResponse,

    -- * Response Lenses
    listSubscribedWorkteamsResponse_nextToken,
    listSubscribedWorkteamsResponse_httpStatus,
    listSubscribedWorkteamsResponse_subscribedWorkteams,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListSubscribedWorkteams' smart constructor.
data ListSubscribedWorkteams = ListSubscribedWorkteams'
  { -- | If the result of the previous @ListSubscribedWorkteams@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of labeling jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the work team name. This filter returns only work teams
    -- whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscribedWorkteams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscribedWorkteams_nextToken' - If the result of the previous @ListSubscribedWorkteams@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of labeling jobs, use the token in the next request.
--
-- 'nameContains', 'listSubscribedWorkteams_nameContains' - A string in the work team name. This filter returns only work teams
-- whose name contains the specified string.
--
-- 'maxResults', 'listSubscribedWorkteams_maxResults' - The maximum number of work teams to return in each page of the response.
newListSubscribedWorkteams ::
  ListSubscribedWorkteams
newListSubscribedWorkteams =
  ListSubscribedWorkteams'
    { nextToken =
        Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the result of the previous @ListSubscribedWorkteams@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of labeling jobs, use the token in the next request.
listSubscribedWorkteams_nextToken :: Lens.Lens' ListSubscribedWorkteams (Prelude.Maybe Prelude.Text)
listSubscribedWorkteams_nextToken = Lens.lens (\ListSubscribedWorkteams' {nextToken} -> nextToken) (\s@ListSubscribedWorkteams' {} a -> s {nextToken = a} :: ListSubscribedWorkteams)

-- | A string in the work team name. This filter returns only work teams
-- whose name contains the specified string.
listSubscribedWorkteams_nameContains :: Lens.Lens' ListSubscribedWorkteams (Prelude.Maybe Prelude.Text)
listSubscribedWorkteams_nameContains = Lens.lens (\ListSubscribedWorkteams' {nameContains} -> nameContains) (\s@ListSubscribedWorkteams' {} a -> s {nameContains = a} :: ListSubscribedWorkteams)

-- | The maximum number of work teams to return in each page of the response.
listSubscribedWorkteams_maxResults :: Lens.Lens' ListSubscribedWorkteams (Prelude.Maybe Prelude.Natural)
listSubscribedWorkteams_maxResults = Lens.lens (\ListSubscribedWorkteams' {maxResults} -> maxResults) (\s@ListSubscribedWorkteams' {} a -> s {maxResults = a} :: ListSubscribedWorkteams)

instance Core.AWSPager ListSubscribedWorkteams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscribedWorkteamsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSubscribedWorkteamsResponse_subscribedWorkteams
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscribedWorkteams_nextToken
          Lens..~ rs
          Lens.^? listSubscribedWorkteamsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSubscribedWorkteams where
  type
    AWSResponse ListSubscribedWorkteams =
      ListSubscribedWorkteamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscribedWorkteamsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "SubscribedWorkteams"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSubscribedWorkteams

instance Prelude.NFData ListSubscribedWorkteams

instance Core.ToHeaders ListSubscribedWorkteams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListSubscribedWorkteams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSubscribedWorkteams where
  toJSON ListSubscribedWorkteams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListSubscribedWorkteams where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSubscribedWorkteams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSubscribedWorkteamsResponse' smart constructor.
data ListSubscribedWorkteamsResponse = ListSubscribedWorkteamsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @Workteam@ objects, each describing a work team.
    subscribedWorkteams :: [SubscribedWorkteam]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscribedWorkteamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscribedWorkteamsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of work teams, use it in the subsequent request.
--
-- 'httpStatus', 'listSubscribedWorkteamsResponse_httpStatus' - The response's http status code.
--
-- 'subscribedWorkteams', 'listSubscribedWorkteamsResponse_subscribedWorkteams' - An array of @Workteam@ objects, each describing a work team.
newListSubscribedWorkteamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscribedWorkteamsResponse
newListSubscribedWorkteamsResponse pHttpStatus_ =
  ListSubscribedWorkteamsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      subscribedWorkteams = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of work teams, use it in the subsequent request.
listSubscribedWorkteamsResponse_nextToken :: Lens.Lens' ListSubscribedWorkteamsResponse (Prelude.Maybe Prelude.Text)
listSubscribedWorkteamsResponse_nextToken = Lens.lens (\ListSubscribedWorkteamsResponse' {nextToken} -> nextToken) (\s@ListSubscribedWorkteamsResponse' {} a -> s {nextToken = a} :: ListSubscribedWorkteamsResponse)

-- | The response's http status code.
listSubscribedWorkteamsResponse_httpStatus :: Lens.Lens' ListSubscribedWorkteamsResponse Prelude.Int
listSubscribedWorkteamsResponse_httpStatus = Lens.lens (\ListSubscribedWorkteamsResponse' {httpStatus} -> httpStatus) (\s@ListSubscribedWorkteamsResponse' {} a -> s {httpStatus = a} :: ListSubscribedWorkteamsResponse)

-- | An array of @Workteam@ objects, each describing a work team.
listSubscribedWorkteamsResponse_subscribedWorkteams :: Lens.Lens' ListSubscribedWorkteamsResponse [SubscribedWorkteam]
listSubscribedWorkteamsResponse_subscribedWorkteams = Lens.lens (\ListSubscribedWorkteamsResponse' {subscribedWorkteams} -> subscribedWorkteams) (\s@ListSubscribedWorkteamsResponse' {} a -> s {subscribedWorkteams = a} :: ListSubscribedWorkteamsResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    ListSubscribedWorkteamsResponse
