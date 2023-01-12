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
-- Module      : Amazonka.ResilienceHub.ListAlarmRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the alarm recommendations for a AWS Resilience Hub application.
module Amazonka.ResilienceHub.ListAlarmRecommendations
  ( -- * Creating a Request
    ListAlarmRecommendations (..),
    newListAlarmRecommendations,

    -- * Request Lenses
    listAlarmRecommendations_maxResults,
    listAlarmRecommendations_nextToken,
    listAlarmRecommendations_assessmentArn,

    -- * Destructuring the Response
    ListAlarmRecommendationsResponse (..),
    newListAlarmRecommendationsResponse,

    -- * Response Lenses
    listAlarmRecommendationsResponse_nextToken,
    listAlarmRecommendationsResponse_httpStatus,
    listAlarmRecommendationsResponse_alarmRecommendations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAlarmRecommendations' smart constructor.
data ListAlarmRecommendations = ListAlarmRecommendations'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the assessment. The format for this
    -- ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    assessmentArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAlarmRecommendations_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listAlarmRecommendations_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'assessmentArn', 'listAlarmRecommendations_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListAlarmRecommendations ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListAlarmRecommendations
newListAlarmRecommendations pAssessmentArn_ =
  ListAlarmRecommendations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAlarmRecommendations_maxResults :: Lens.Lens' ListAlarmRecommendations (Prelude.Maybe Prelude.Natural)
listAlarmRecommendations_maxResults = Lens.lens (\ListAlarmRecommendations' {maxResults} -> maxResults) (\s@ListAlarmRecommendations' {} a -> s {maxResults = a} :: ListAlarmRecommendations)

-- | Null, or the token from a previous call to get the next set of results.
listAlarmRecommendations_nextToken :: Lens.Lens' ListAlarmRecommendations (Prelude.Maybe Prelude.Text)
listAlarmRecommendations_nextToken = Lens.lens (\ListAlarmRecommendations' {nextToken} -> nextToken) (\s@ListAlarmRecommendations' {} a -> s {nextToken = a} :: ListAlarmRecommendations)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listAlarmRecommendations_assessmentArn :: Lens.Lens' ListAlarmRecommendations Prelude.Text
listAlarmRecommendations_assessmentArn = Lens.lens (\ListAlarmRecommendations' {assessmentArn} -> assessmentArn) (\s@ListAlarmRecommendations' {} a -> s {assessmentArn = a} :: ListAlarmRecommendations)

instance Core.AWSRequest ListAlarmRecommendations where
  type
    AWSResponse ListAlarmRecommendations =
      ListAlarmRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlarmRecommendationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "alarmRecommendations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAlarmRecommendations where
  hashWithSalt _salt ListAlarmRecommendations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData ListAlarmRecommendations where
  rnf ListAlarmRecommendations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders ListAlarmRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAlarmRecommendations where
  toJSON ListAlarmRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath ListAlarmRecommendations where
  toPath = Prelude.const "/list-alarm-recommendations"

instance Data.ToQuery ListAlarmRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAlarmRecommendationsResponse' smart constructor.
data ListAlarmRecommendationsResponse = ListAlarmRecommendationsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The alarm recommendations for an AWS Resilience Hub application,
    -- returned as an object. This object includes application component names,
    -- descriptions, information about whether a recommendation has already
    -- been implemented or not, prerequisites, and more.
    alarmRecommendations :: [AlarmRecommendation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlarmRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlarmRecommendationsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAlarmRecommendationsResponse_httpStatus' - The response's http status code.
--
-- 'alarmRecommendations', 'listAlarmRecommendationsResponse_alarmRecommendations' - The alarm recommendations for an AWS Resilience Hub application,
-- returned as an object. This object includes application component names,
-- descriptions, information about whether a recommendation has already
-- been implemented or not, prerequisites, and more.
newListAlarmRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlarmRecommendationsResponse
newListAlarmRecommendationsResponse pHttpStatus_ =
  ListAlarmRecommendationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      alarmRecommendations = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listAlarmRecommendationsResponse_nextToken :: Lens.Lens' ListAlarmRecommendationsResponse (Prelude.Maybe Prelude.Text)
listAlarmRecommendationsResponse_nextToken = Lens.lens (\ListAlarmRecommendationsResponse' {nextToken} -> nextToken) (\s@ListAlarmRecommendationsResponse' {} a -> s {nextToken = a} :: ListAlarmRecommendationsResponse)

-- | The response's http status code.
listAlarmRecommendationsResponse_httpStatus :: Lens.Lens' ListAlarmRecommendationsResponse Prelude.Int
listAlarmRecommendationsResponse_httpStatus = Lens.lens (\ListAlarmRecommendationsResponse' {httpStatus} -> httpStatus) (\s@ListAlarmRecommendationsResponse' {} a -> s {httpStatus = a} :: ListAlarmRecommendationsResponse)

-- | The alarm recommendations for an AWS Resilience Hub application,
-- returned as an object. This object includes application component names,
-- descriptions, information about whether a recommendation has already
-- been implemented or not, prerequisites, and more.
listAlarmRecommendationsResponse_alarmRecommendations :: Lens.Lens' ListAlarmRecommendationsResponse [AlarmRecommendation]
listAlarmRecommendationsResponse_alarmRecommendations = Lens.lens (\ListAlarmRecommendationsResponse' {alarmRecommendations} -> alarmRecommendations) (\s@ListAlarmRecommendationsResponse' {} a -> s {alarmRecommendations = a} :: ListAlarmRecommendationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAlarmRecommendationsResponse
  where
  rnf ListAlarmRecommendationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf alarmRecommendations
