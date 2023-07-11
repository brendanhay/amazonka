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
-- Module      : Amazonka.ResilienceHub.ListAppComponentCompliances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the compliances for an AWS Resilience Hub component.
module Amazonka.ResilienceHub.ListAppComponentCompliances
  ( -- * Creating a Request
    ListAppComponentCompliances (..),
    newListAppComponentCompliances,

    -- * Request Lenses
    listAppComponentCompliances_maxResults,
    listAppComponentCompliances_nextToken,
    listAppComponentCompliances_assessmentArn,

    -- * Destructuring the Response
    ListAppComponentCompliancesResponse (..),
    newListAppComponentCompliancesResponse,

    -- * Response Lenses
    listAppComponentCompliancesResponse_nextToken,
    listAppComponentCompliancesResponse_httpStatus,
    listAppComponentCompliancesResponse_componentCompliances,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppComponentCompliances' smart constructor.
data ListAppComponentCompliances = ListAppComponentCompliances'
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
-- Create a value of 'ListAppComponentCompliances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppComponentCompliances_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listAppComponentCompliances_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'assessmentArn', 'listAppComponentCompliances_assessmentArn' - The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListAppComponentCompliances ::
  -- | 'assessmentArn'
  Prelude.Text ->
  ListAppComponentCompliances
newListAppComponentCompliances pAssessmentArn_ =
  ListAppComponentCompliances'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assessmentArn = pAssessmentArn_
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAppComponentCompliances_maxResults :: Lens.Lens' ListAppComponentCompliances (Prelude.Maybe Prelude.Natural)
listAppComponentCompliances_maxResults = Lens.lens (\ListAppComponentCompliances' {maxResults} -> maxResults) (\s@ListAppComponentCompliances' {} a -> s {maxResults = a} :: ListAppComponentCompliances)

-- | Null, or the token from a previous call to get the next set of results.
listAppComponentCompliances_nextToken :: Lens.Lens' ListAppComponentCompliances (Prelude.Maybe Prelude.Text)
listAppComponentCompliances_nextToken = Lens.lens (\ListAppComponentCompliances' {nextToken} -> nextToken) (\s@ListAppComponentCompliances' {} a -> s {nextToken = a} :: ListAppComponentCompliances)

-- | The Amazon Resource Name (ARN) of the assessment. The format for this
-- ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app-assessment\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listAppComponentCompliances_assessmentArn :: Lens.Lens' ListAppComponentCompliances Prelude.Text
listAppComponentCompliances_assessmentArn = Lens.lens (\ListAppComponentCompliances' {assessmentArn} -> assessmentArn) (\s@ListAppComponentCompliances' {} a -> s {assessmentArn = a} :: ListAppComponentCompliances)

instance Core.AWSRequest ListAppComponentCompliances where
  type
    AWSResponse ListAppComponentCompliances =
      ListAppComponentCompliancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppComponentCompliancesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "componentCompliances"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAppComponentCompliances where
  hashWithSalt _salt ListAppComponentCompliances' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assessmentArn

instance Prelude.NFData ListAppComponentCompliances where
  rnf ListAppComponentCompliances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assessmentArn

instance Data.ToHeaders ListAppComponentCompliances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppComponentCompliances where
  toJSON ListAppComponentCompliances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("assessmentArn" Data..= assessmentArn)
          ]
      )

instance Data.ToPath ListAppComponentCompliances where
  toPath =
    Prelude.const "/list-app-component-compliances"

instance Data.ToQuery ListAppComponentCompliances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppComponentCompliancesResponse' smart constructor.
data ListAppComponentCompliancesResponse = ListAppComponentCompliancesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The compliances for an AWS Resilience Hub application component,
    -- returned as an object. This object contains component names,
    -- compliances, costs, resiliency scores, outage scores, and more.
    componentCompliances :: [AppComponentCompliance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppComponentCompliancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppComponentCompliancesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppComponentCompliancesResponse_httpStatus' - The response's http status code.
--
-- 'componentCompliances', 'listAppComponentCompliancesResponse_componentCompliances' - The compliances for an AWS Resilience Hub application component,
-- returned as an object. This object contains component names,
-- compliances, costs, resiliency scores, outage scores, and more.
newListAppComponentCompliancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppComponentCompliancesResponse
newListAppComponentCompliancesResponse pHttpStatus_ =
  ListAppComponentCompliancesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      componentCompliances = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppComponentCompliancesResponse_nextToken :: Lens.Lens' ListAppComponentCompliancesResponse (Prelude.Maybe Prelude.Text)
listAppComponentCompliancesResponse_nextToken = Lens.lens (\ListAppComponentCompliancesResponse' {nextToken} -> nextToken) (\s@ListAppComponentCompliancesResponse' {} a -> s {nextToken = a} :: ListAppComponentCompliancesResponse)

-- | The response's http status code.
listAppComponentCompliancesResponse_httpStatus :: Lens.Lens' ListAppComponentCompliancesResponse Prelude.Int
listAppComponentCompliancesResponse_httpStatus = Lens.lens (\ListAppComponentCompliancesResponse' {httpStatus} -> httpStatus) (\s@ListAppComponentCompliancesResponse' {} a -> s {httpStatus = a} :: ListAppComponentCompliancesResponse)

-- | The compliances for an AWS Resilience Hub application component,
-- returned as an object. This object contains component names,
-- compliances, costs, resiliency scores, outage scores, and more.
listAppComponentCompliancesResponse_componentCompliances :: Lens.Lens' ListAppComponentCompliancesResponse [AppComponentCompliance]
listAppComponentCompliancesResponse_componentCompliances = Lens.lens (\ListAppComponentCompliancesResponse' {componentCompliances} -> componentCompliances) (\s@ListAppComponentCompliancesResponse' {} a -> s {componentCompliances = a} :: ListAppComponentCompliancesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAppComponentCompliancesResponse
  where
  rnf ListAppComponentCompliancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf componentCompliances
