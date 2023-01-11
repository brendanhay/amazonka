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
-- Module      : Amazonka.ResilienceHub.ListRecommendationTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recommendation templates for the Resilience Hub applications.
module Amazonka.ResilienceHub.ListRecommendationTemplates
  ( -- * Creating a Request
    ListRecommendationTemplates (..),
    newListRecommendationTemplates,

    -- * Request Lenses
    listRecommendationTemplates_maxResults,
    listRecommendationTemplates_name,
    listRecommendationTemplates_nextToken,
    listRecommendationTemplates_recommendationTemplateArn,
    listRecommendationTemplates_reverseOrder,
    listRecommendationTemplates_status,
    listRecommendationTemplates_assessmentArn,

    -- * Destructuring the Response
    ListRecommendationTemplatesResponse (..),
    newListRecommendationTemplatesResponse,

    -- * Response Lenses
    listRecommendationTemplatesResponse_nextToken,
    listRecommendationTemplatesResponse_recommendationTemplates,
    listRecommendationTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecommendationTemplates' smart constructor.
data ListRecommendationTemplates = ListRecommendationTemplates'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name for one of the listed recommendation templates.
    name :: Prelude.Maybe Prelude.Text,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a recommendation template.
    recommendationTemplateArn :: Prelude.Maybe Prelude.Text,
    -- | The default is to sort by ascending __startTime__. To sort by descending
    -- __startTime__, set reverseOrder to @true@.
    reverseOrder :: Prelude.Maybe Prelude.Bool,
    -- | The status of the action.
    status :: Prelude.Maybe (Prelude.NonEmpty RecommendationTemplateStatus),
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
-- Create a value of 'ListRecommendationTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecommendationTemplates_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'name', 'listRecommendationTemplates_name' - The name for one of the listed recommendation templates.
--
-- 'nextToken', 'listRecommendationTemplates_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'recommendationTemplateArn', 'listRecommendationTemplates_recommendationTemplateArn' - The Amazon Resource Name (ARN) for a recommendation template.
--
-- 'reverseOrder', 'listRecommendationTemplates_reverseOrder' - The default is to sort by ascending __startTime__. To sort by descending
-- __startTime__, set reverseOrder to @true@.
--
-- 'status', 'listRecommendationTemplates_status' - The status of the action.
--
-- 'assessmentArn', 'listRecommendationTemplates_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListRecommendationTemplates ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListRecommendationTemplates
newListRecommendationTemplates pAssessmentArn_ =
  ListRecommendationTemplates'
    { maxResults =
        Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recommendationTemplateArn = Prelude.Nothing,
      reverseOrder = Prelude.Nothing,
      status = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listRecommendationTemplates_maxResults :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe Prelude.Natural)
listRecommendationTemplates_maxResults = Lens.lens (\ListRecommendationTemplates' {maxResults} -> maxResults) (\s@ListRecommendationTemplates' {} a -> s {maxResults = a} :: ListRecommendationTemplates)

-- | The name for one of the listed recommendation templates.
listRecommendationTemplates_name :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe Prelude.Text)
listRecommendationTemplates_name = Lens.lens (\ListRecommendationTemplates' {name} -> name) (\s@ListRecommendationTemplates' {} a -> s {name = a} :: ListRecommendationTemplates)

-- | Null, or the token from a previous call to get the next set of results.
listRecommendationTemplates_nextToken :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe Prelude.Text)
listRecommendationTemplates_nextToken = Lens.lens (\ListRecommendationTemplates' {nextToken} -> nextToken) (\s@ListRecommendationTemplates' {} a -> s {nextToken = a} :: ListRecommendationTemplates)

-- | The Amazon Resource Name (ARN) for a recommendation template.
listRecommendationTemplates_recommendationTemplateArn :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe Prelude.Text)
listRecommendationTemplates_recommendationTemplateArn = Lens.lens (\ListRecommendationTemplates' {recommendationTemplateArn} -> recommendationTemplateArn) (\s@ListRecommendationTemplates' {} a -> s {recommendationTemplateArn = a} :: ListRecommendationTemplates)

-- | The default is to sort by ascending __startTime__. To sort by descending
-- __startTime__, set reverseOrder to @true@.
listRecommendationTemplates_reverseOrder :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe Prelude.Bool)
listRecommendationTemplates_reverseOrder = Lens.lens (\ListRecommendationTemplates' {reverseOrder} -> reverseOrder) (\s@ListRecommendationTemplates' {} a -> s {reverseOrder = a} :: ListRecommendationTemplates)

-- | The status of the action.
listRecommendationTemplates_status :: Lens.Lens' ListRecommendationTemplates (Prelude.Maybe (Prelude.NonEmpty RecommendationTemplateStatus))
listRecommendationTemplates_status = Lens.lens (\ListRecommendationTemplates' {status} -> status) (\s@ListRecommendationTemplates' {} a -> s {status = a} :: ListRecommendationTemplates) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listRecommendationTemplates_assessmentArn :: Lens.Lens' ListRecommendationTemplates Prelude.Text
listRecommendationTemplates_assessmentArn = Lens.lens (\ListRecommendationTemplates' {assessmentArn} -> assessmentArn) (\s@ListRecommendationTemplates' {} a -> s {assessmentArn = a} :: ListRecommendationTemplates)

instance Core.AWSRequest ListRecommendationTemplates where
  type
    AWSResponse ListRecommendationTemplates =
      ListRecommendationTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecommendationTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "recommendationTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecommendationTemplates where
  hashWithSalt _salt ListRecommendationTemplates' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recommendationTemplateArn
      `Prelude.hashWithSalt` reverseOrder
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData ListRecommendationTemplates where
  rnf ListRecommendationTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationTemplateArn
      `Prelude.seq` Prelude.rnf reverseOrder
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders ListRecommendationTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecommendationTemplates where
  toPath =
    Prelude.const "/list-recommendation-templates"

instance Data.ToQuery ListRecommendationTemplates where
  toQuery ListRecommendationTemplates' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken,
        "recommendationTemplateArn"
          Data.=: recommendationTemplateArn,
        "reverseOrder" Data.=: reverseOrder,
        "status"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> status),
        "assessmentArn" Data.=: assessmentArn
      ]

-- | /See:/ 'newListRecommendationTemplatesResponse' smart constructor.
data ListRecommendationTemplatesResponse = ListRecommendationTemplatesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The recommendation templates for the Resilience Hub applications.
    recommendationTemplates :: Prelude.Maybe [RecommendationTemplate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecommendationTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecommendationTemplatesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'recommendationTemplates', 'listRecommendationTemplatesResponse_recommendationTemplates' - The recommendation templates for the Resilience Hub applications.
--
-- 'httpStatus', 'listRecommendationTemplatesResponse_httpStatus' - The response's http status code.
newListRecommendationTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecommendationTemplatesResponse
newListRecommendationTemplatesResponse pHttpStatus_ =
  ListRecommendationTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      recommendationTemplates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listRecommendationTemplatesResponse_nextToken :: Lens.Lens' ListRecommendationTemplatesResponse (Prelude.Maybe Prelude.Text)
listRecommendationTemplatesResponse_nextToken = Lens.lens (\ListRecommendationTemplatesResponse' {nextToken} -> nextToken) (\s@ListRecommendationTemplatesResponse' {} a -> s {nextToken = a} :: ListRecommendationTemplatesResponse)

-- | The recommendation templates for the Resilience Hub applications.
listRecommendationTemplatesResponse_recommendationTemplates :: Lens.Lens' ListRecommendationTemplatesResponse (Prelude.Maybe [RecommendationTemplate])
listRecommendationTemplatesResponse_recommendationTemplates = Lens.lens (\ListRecommendationTemplatesResponse' {recommendationTemplates} -> recommendationTemplates) (\s@ListRecommendationTemplatesResponse' {} a -> s {recommendationTemplates = a} :: ListRecommendationTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecommendationTemplatesResponse_httpStatus :: Lens.Lens' ListRecommendationTemplatesResponse Prelude.Int
listRecommendationTemplatesResponse_httpStatus = Lens.lens (\ListRecommendationTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListRecommendationTemplatesResponse' {} a -> s {httpStatus = a} :: ListRecommendationTemplatesResponse)

instance
  Prelude.NFData
    ListRecommendationTemplatesResponse
  where
  rnf ListRecommendationTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recommendationTemplates
      `Prelude.seq` Prelude.rnf httpStatus
