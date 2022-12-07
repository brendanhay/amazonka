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
-- Module      : Amazonka.Inspector.ListAssessmentRunAgents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the agents of the assessment runs that are specified by the ARNs
-- of the assessment runs.
--
-- This operation returns paginated results.
module Amazonka.Inspector.ListAssessmentRunAgents
  ( -- * Creating a Request
    ListAssessmentRunAgents (..),
    newListAssessmentRunAgents,

    -- * Request Lenses
    listAssessmentRunAgents_nextToken,
    listAssessmentRunAgents_filter,
    listAssessmentRunAgents_maxResults,
    listAssessmentRunAgents_assessmentRunArn,

    -- * Destructuring the Response
    ListAssessmentRunAgentsResponse (..),
    newListAssessmentRunAgentsResponse,

    -- * Response Lenses
    listAssessmentRunAgentsResponse_nextToken,
    listAssessmentRunAgentsResponse_httpStatus,
    listAssessmentRunAgentsResponse_assessmentRunAgents,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssessmentRunAgents' smart constructor.
data ListAssessmentRunAgents = ListAssessmentRunAgents'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to specify a subset of data to be included in
    -- the action\'s response.
    --
    -- For a record to match a filter, all specified filter attributes must
    -- match. When multiple values are specified for a filter attribute, any of
    -- the values can match.
    filter' :: Prelude.Maybe AgentFilter,
    -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response. The default value is 10. The maximum value is
    -- 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ARN that specifies the assessment run whose agents you want to list.
    assessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentRunAgents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRunAgents_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'filter'', 'listAssessmentRunAgents_filter' - You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
--
-- 'maxResults', 'listAssessmentRunAgents_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
--
-- 'assessmentRunArn', 'listAssessmentRunAgents_assessmentRunArn' - The ARN that specifies the assessment run whose agents you want to list.
newListAssessmentRunAgents ::
  -- | 'assessmentRunArn'
  Prelude.Text ->
  ListAssessmentRunAgents
newListAssessmentRunAgents pAssessmentRunArn_ =
  ListAssessmentRunAgents'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assessmentRunArn = pAssessmentRunArn_
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListAssessmentRunAgents__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listAssessmentRunAgents_nextToken :: Lens.Lens' ListAssessmentRunAgents (Prelude.Maybe Prelude.Text)
listAssessmentRunAgents_nextToken = Lens.lens (\ListAssessmentRunAgents' {nextToken} -> nextToken) (\s@ListAssessmentRunAgents' {} a -> s {nextToken = a} :: ListAssessmentRunAgents)

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
listAssessmentRunAgents_filter :: Lens.Lens' ListAssessmentRunAgents (Prelude.Maybe AgentFilter)
listAssessmentRunAgents_filter = Lens.lens (\ListAssessmentRunAgents' {filter'} -> filter') (\s@ListAssessmentRunAgents' {} a -> s {filter' = a} :: ListAssessmentRunAgents)

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response. The default value is 10. The maximum value is
-- 500.
listAssessmentRunAgents_maxResults :: Lens.Lens' ListAssessmentRunAgents (Prelude.Maybe Prelude.Int)
listAssessmentRunAgents_maxResults = Lens.lens (\ListAssessmentRunAgents' {maxResults} -> maxResults) (\s@ListAssessmentRunAgents' {} a -> s {maxResults = a} :: ListAssessmentRunAgents)

-- | The ARN that specifies the assessment run whose agents you want to list.
listAssessmentRunAgents_assessmentRunArn :: Lens.Lens' ListAssessmentRunAgents Prelude.Text
listAssessmentRunAgents_assessmentRunArn = Lens.lens (\ListAssessmentRunAgents' {assessmentRunArn} -> assessmentRunArn) (\s@ListAssessmentRunAgents' {} a -> s {assessmentRunArn = a} :: ListAssessmentRunAgents)

instance Core.AWSPager ListAssessmentRunAgents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssessmentRunAgentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssessmentRunAgentsResponse_assessmentRunAgents
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssessmentRunAgents_nextToken
          Lens..~ rs
          Lens.^? listAssessmentRunAgentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssessmentRunAgents where
  type
    AWSResponse ListAssessmentRunAgents =
      ListAssessmentRunAgentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssessmentRunAgentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "assessmentRunAgents"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssessmentRunAgents where
  hashWithSalt _salt ListAssessmentRunAgents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` assessmentRunArn

instance Prelude.NFData ListAssessmentRunAgents where
  rnf ListAssessmentRunAgents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf assessmentRunArn

instance Data.ToHeaders ListAssessmentRunAgents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.ListAssessmentRunAgents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssessmentRunAgents where
  toJSON ListAssessmentRunAgents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("assessmentRunArn" Data..= assessmentRunArn)
          ]
      )

instance Data.ToPath ListAssessmentRunAgents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssessmentRunAgents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssessmentRunAgentsResponse' smart constructor.
data ListAssessmentRunAgentsResponse = ListAssessmentRunAgentsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of ARNs that specifies the agents returned by the action.
    assessmentRunAgents :: [AssessmentRunAgent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssessmentRunAgentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssessmentRunAgentsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listAssessmentRunAgentsResponse_httpStatus' - The response's http status code.
--
-- 'assessmentRunAgents', 'listAssessmentRunAgentsResponse_assessmentRunAgents' - A list of ARNs that specifies the agents returned by the action.
newListAssessmentRunAgentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssessmentRunAgentsResponse
newListAssessmentRunAgentsResponse pHttpStatus_ =
  ListAssessmentRunAgentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assessmentRunAgents = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listAssessmentRunAgentsResponse_nextToken :: Lens.Lens' ListAssessmentRunAgentsResponse (Prelude.Maybe Prelude.Text)
listAssessmentRunAgentsResponse_nextToken = Lens.lens (\ListAssessmentRunAgentsResponse' {nextToken} -> nextToken) (\s@ListAssessmentRunAgentsResponse' {} a -> s {nextToken = a} :: ListAssessmentRunAgentsResponse)

-- | The response's http status code.
listAssessmentRunAgentsResponse_httpStatus :: Lens.Lens' ListAssessmentRunAgentsResponse Prelude.Int
listAssessmentRunAgentsResponse_httpStatus = Lens.lens (\ListAssessmentRunAgentsResponse' {httpStatus} -> httpStatus) (\s@ListAssessmentRunAgentsResponse' {} a -> s {httpStatus = a} :: ListAssessmentRunAgentsResponse)

-- | A list of ARNs that specifies the agents returned by the action.
listAssessmentRunAgentsResponse_assessmentRunAgents :: Lens.Lens' ListAssessmentRunAgentsResponse [AssessmentRunAgent]
listAssessmentRunAgentsResponse_assessmentRunAgents = Lens.lens (\ListAssessmentRunAgentsResponse' {assessmentRunAgents} -> assessmentRunAgents) (\s@ListAssessmentRunAgentsResponse' {} a -> s {assessmentRunAgents = a} :: ListAssessmentRunAgentsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAssessmentRunAgentsResponse
  where
  rnf ListAssessmentRunAgentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessmentRunAgents
