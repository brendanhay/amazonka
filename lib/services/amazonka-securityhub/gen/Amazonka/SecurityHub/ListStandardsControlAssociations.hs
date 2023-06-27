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
-- Module      : Amazonka.SecurityHub.ListStandardsControlAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies whether a control is currently enabled or disabled in each
-- enabled standard in the calling account.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.ListStandardsControlAssociations
  ( -- * Creating a Request
    ListStandardsControlAssociations (..),
    newListStandardsControlAssociations,

    -- * Request Lenses
    listStandardsControlAssociations_maxResults,
    listStandardsControlAssociations_nextToken,
    listStandardsControlAssociations_securityControlId,

    -- * Destructuring the Response
    ListStandardsControlAssociationsResponse (..),
    newListStandardsControlAssociationsResponse,

    -- * Response Lenses
    listStandardsControlAssociationsResponse_nextToken,
    listStandardsControlAssociationsResponse_httpStatus,
    listStandardsControlAssociationsResponse_standardsControlAssociationSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListStandardsControlAssociations' smart constructor.
data ListStandardsControlAssociations = ListStandardsControlAssociations'
  { -- | An optional parameter that limits the total results of the API response
    -- to the specified number. If this parameter isn\'t provided in the
    -- request, the results include the first 25 standard and control
    -- associations. The results also include a @NextToken@ parameter that you
    -- can use in a subsequent API call to get the next 25 associations. This
    -- repeats until all associations for the specified control are returned.
    -- The number of results is limited by the number of supported Security Hub
    -- standards that you\'ve enabled in the calling account.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional pagination parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the control (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters) that you want to
    -- determine the enablement status of in each enabled standard.
    securityControlId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStandardsControlAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStandardsControlAssociations_maxResults' - An optional parameter that limits the total results of the API response
-- to the specified number. If this parameter isn\'t provided in the
-- request, the results include the first 25 standard and control
-- associations. The results also include a @NextToken@ parameter that you
-- can use in a subsequent API call to get the next 25 associations. This
-- repeats until all associations for the specified control are returned.
-- The number of results is limited by the number of supported Security Hub
-- standards that you\'ve enabled in the calling account.
--
-- 'nextToken', 'listStandardsControlAssociations_nextToken' - Optional pagination parameter.
--
-- 'securityControlId', 'listStandardsControlAssociations_securityControlId' - The identifier of the control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) that you want to
-- determine the enablement status of in each enabled standard.
newListStandardsControlAssociations ::
  -- | 'securityControlId'
  Prelude.Text ->
  ListStandardsControlAssociations
newListStandardsControlAssociations
  pSecurityControlId_ =
    ListStandardsControlAssociations'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        securityControlId = pSecurityControlId_
      }

-- | An optional parameter that limits the total results of the API response
-- to the specified number. If this parameter isn\'t provided in the
-- request, the results include the first 25 standard and control
-- associations. The results also include a @NextToken@ parameter that you
-- can use in a subsequent API call to get the next 25 associations. This
-- repeats until all associations for the specified control are returned.
-- The number of results is limited by the number of supported Security Hub
-- standards that you\'ve enabled in the calling account.
listStandardsControlAssociations_maxResults :: Lens.Lens' ListStandardsControlAssociations (Prelude.Maybe Prelude.Natural)
listStandardsControlAssociations_maxResults = Lens.lens (\ListStandardsControlAssociations' {maxResults} -> maxResults) (\s@ListStandardsControlAssociations' {} a -> s {maxResults = a} :: ListStandardsControlAssociations)

-- | Optional pagination parameter.
listStandardsControlAssociations_nextToken :: Lens.Lens' ListStandardsControlAssociations (Prelude.Maybe Prelude.Text)
listStandardsControlAssociations_nextToken = Lens.lens (\ListStandardsControlAssociations' {nextToken} -> nextToken) (\s@ListStandardsControlAssociations' {} a -> s {nextToken = a} :: ListStandardsControlAssociations)

-- | The identifier of the control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) that you want to
-- determine the enablement status of in each enabled standard.
listStandardsControlAssociations_securityControlId :: Lens.Lens' ListStandardsControlAssociations Prelude.Text
listStandardsControlAssociations_securityControlId = Lens.lens (\ListStandardsControlAssociations' {securityControlId} -> securityControlId) (\s@ListStandardsControlAssociations' {} a -> s {securityControlId = a} :: ListStandardsControlAssociations)

instance
  Core.AWSPager
    ListStandardsControlAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStandardsControlAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listStandardsControlAssociationsResponse_standardsControlAssociationSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStandardsControlAssociations_nextToken
          Lens..~ rs
          Lens.^? listStandardsControlAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListStandardsControlAssociations
  where
  type
    AWSResponse ListStandardsControlAssociations =
      ListStandardsControlAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStandardsControlAssociationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "StandardsControlAssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListStandardsControlAssociations
  where
  hashWithSalt
    _salt
    ListStandardsControlAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` securityControlId

instance
  Prelude.NFData
    ListStandardsControlAssociations
  where
  rnf ListStandardsControlAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityControlId

instance
  Data.ToHeaders
    ListStandardsControlAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStandardsControlAssociations where
  toPath = Prelude.const "/associations"

instance
  Data.ToQuery
    ListStandardsControlAssociations
  where
  toQuery ListStandardsControlAssociations' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SecurityControlId" Data.=: securityControlId
      ]

-- | /See:/ 'newListStandardsControlAssociationsResponse' smart constructor.
data ListStandardsControlAssociationsResponse = ListStandardsControlAssociationsResponse'
  { -- | A pagination parameter that\'s included in the response only if it was
    -- included in the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array that provides the enablement status and other details for each
    -- security control that applies to each enabled standard.
    standardsControlAssociationSummaries :: [StandardsControlAssociationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStandardsControlAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStandardsControlAssociationsResponse_nextToken' - A pagination parameter that\'s included in the response only if it was
-- included in the request.
--
-- 'httpStatus', 'listStandardsControlAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'standardsControlAssociationSummaries', 'listStandardsControlAssociationsResponse_standardsControlAssociationSummaries' - An array that provides the enablement status and other details for each
-- security control that applies to each enabled standard.
newListStandardsControlAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStandardsControlAssociationsResponse
newListStandardsControlAssociationsResponse
  pHttpStatus_ =
    ListStandardsControlAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        standardsControlAssociationSummaries =
          Prelude.mempty
      }

-- | A pagination parameter that\'s included in the response only if it was
-- included in the request.
listStandardsControlAssociationsResponse_nextToken :: Lens.Lens' ListStandardsControlAssociationsResponse (Prelude.Maybe Prelude.Text)
listStandardsControlAssociationsResponse_nextToken = Lens.lens (\ListStandardsControlAssociationsResponse' {nextToken} -> nextToken) (\s@ListStandardsControlAssociationsResponse' {} a -> s {nextToken = a} :: ListStandardsControlAssociationsResponse)

-- | The response's http status code.
listStandardsControlAssociationsResponse_httpStatus :: Lens.Lens' ListStandardsControlAssociationsResponse Prelude.Int
listStandardsControlAssociationsResponse_httpStatus = Lens.lens (\ListStandardsControlAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListStandardsControlAssociationsResponse' {} a -> s {httpStatus = a} :: ListStandardsControlAssociationsResponse)

-- | An array that provides the enablement status and other details for each
-- security control that applies to each enabled standard.
listStandardsControlAssociationsResponse_standardsControlAssociationSummaries :: Lens.Lens' ListStandardsControlAssociationsResponse [StandardsControlAssociationSummary]
listStandardsControlAssociationsResponse_standardsControlAssociationSummaries = Lens.lens (\ListStandardsControlAssociationsResponse' {standardsControlAssociationSummaries} -> standardsControlAssociationSummaries) (\s@ListStandardsControlAssociationsResponse' {} a -> s {standardsControlAssociationSummaries = a} :: ListStandardsControlAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListStandardsControlAssociationsResponse
  where
  rnf ListStandardsControlAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf standardsControlAssociationSummaries
