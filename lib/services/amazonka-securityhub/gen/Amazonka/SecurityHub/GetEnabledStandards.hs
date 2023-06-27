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
-- Module      : Amazonka.SecurityHub.GetEnabledStandards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the standards that are currently enabled.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.GetEnabledStandards
  ( -- * Creating a Request
    GetEnabledStandards (..),
    newGetEnabledStandards,

    -- * Request Lenses
    getEnabledStandards_maxResults,
    getEnabledStandards_nextToken,
    getEnabledStandards_standardsSubscriptionArns,

    -- * Destructuring the Response
    GetEnabledStandardsResponse (..),
    newGetEnabledStandardsResponse,

    -- * Response Lenses
    getEnabledStandardsResponse_nextToken,
    getEnabledStandardsResponse_standardsSubscriptions,
    getEnabledStandardsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newGetEnabledStandards' smart constructor.
data GetEnabledStandards = GetEnabledStandards'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that is required for pagination. On your first call to the
    -- @GetEnabledStandards@ operation, set the value of this parameter to
    -- @NULL@.
    --
    -- For subsequent calls to the operation, to continue listing data, set the
    -- value of this parameter to the value returned from the previous
    -- response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of the standards subscription ARNs for the standards to
    -- retrieve.
    standardsSubscriptionArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnabledStandards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getEnabledStandards_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'getEnabledStandards_nextToken' - The token that is required for pagination. On your first call to the
-- @GetEnabledStandards@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
--
-- 'standardsSubscriptionArns', 'getEnabledStandards_standardsSubscriptionArns' - The list of the standards subscription ARNs for the standards to
-- retrieve.
newGetEnabledStandards ::
  GetEnabledStandards
newGetEnabledStandards =
  GetEnabledStandards'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      standardsSubscriptionArns = Prelude.Nothing
    }

-- | The maximum number of results to return in the response.
getEnabledStandards_maxResults :: Lens.Lens' GetEnabledStandards (Prelude.Maybe Prelude.Natural)
getEnabledStandards_maxResults = Lens.lens (\GetEnabledStandards' {maxResults} -> maxResults) (\s@GetEnabledStandards' {} a -> s {maxResults = a} :: GetEnabledStandards)

-- | The token that is required for pagination. On your first call to the
-- @GetEnabledStandards@ operation, set the value of this parameter to
-- @NULL@.
--
-- For subsequent calls to the operation, to continue listing data, set the
-- value of this parameter to the value returned from the previous
-- response.
getEnabledStandards_nextToken :: Lens.Lens' GetEnabledStandards (Prelude.Maybe Prelude.Text)
getEnabledStandards_nextToken = Lens.lens (\GetEnabledStandards' {nextToken} -> nextToken) (\s@GetEnabledStandards' {} a -> s {nextToken = a} :: GetEnabledStandards)

-- | The list of the standards subscription ARNs for the standards to
-- retrieve.
getEnabledStandards_standardsSubscriptionArns :: Lens.Lens' GetEnabledStandards (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getEnabledStandards_standardsSubscriptionArns = Lens.lens (\GetEnabledStandards' {standardsSubscriptionArns} -> standardsSubscriptionArns) (\s@GetEnabledStandards' {} a -> s {standardsSubscriptionArns = a} :: GetEnabledStandards) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager GetEnabledStandards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getEnabledStandardsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getEnabledStandardsResponse_standardsSubscriptions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getEnabledStandards_nextToken
          Lens..~ rs
          Lens.^? getEnabledStandardsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetEnabledStandards where
  type
    AWSResponse GetEnabledStandards =
      GetEnabledStandardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnabledStandardsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "StandardsSubscriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEnabledStandards where
  hashWithSalt _salt GetEnabledStandards' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` standardsSubscriptionArns

instance Prelude.NFData GetEnabledStandards where
  rnf GetEnabledStandards' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf standardsSubscriptionArns

instance Data.ToHeaders GetEnabledStandards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEnabledStandards where
  toJSON GetEnabledStandards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StandardsSubscriptionArns" Data..=)
              Prelude.<$> standardsSubscriptionArns
          ]
      )

instance Data.ToPath GetEnabledStandards where
  toPath = Prelude.const "/standards/get"

instance Data.ToQuery GetEnabledStandards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnabledStandardsResponse' smart constructor.
data GetEnabledStandardsResponse = GetEnabledStandardsResponse'
  { -- | The pagination token to use to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of @StandardsSubscriptions@ objects that include information
    -- about the enabled standards.
    standardsSubscriptions :: Prelude.Maybe [StandardsSubscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnabledStandardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEnabledStandardsResponse_nextToken' - The pagination token to use to request the next page of results.
--
-- 'standardsSubscriptions', 'getEnabledStandardsResponse_standardsSubscriptions' - The list of @StandardsSubscriptions@ objects that include information
-- about the enabled standards.
--
-- 'httpStatus', 'getEnabledStandardsResponse_httpStatus' - The response's http status code.
newGetEnabledStandardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEnabledStandardsResponse
newGetEnabledStandardsResponse pHttpStatus_ =
  GetEnabledStandardsResponse'
    { nextToken =
        Prelude.Nothing,
      standardsSubscriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to request the next page of results.
getEnabledStandardsResponse_nextToken :: Lens.Lens' GetEnabledStandardsResponse (Prelude.Maybe Prelude.Text)
getEnabledStandardsResponse_nextToken = Lens.lens (\GetEnabledStandardsResponse' {nextToken} -> nextToken) (\s@GetEnabledStandardsResponse' {} a -> s {nextToken = a} :: GetEnabledStandardsResponse)

-- | The list of @StandardsSubscriptions@ objects that include information
-- about the enabled standards.
getEnabledStandardsResponse_standardsSubscriptions :: Lens.Lens' GetEnabledStandardsResponse (Prelude.Maybe [StandardsSubscription])
getEnabledStandardsResponse_standardsSubscriptions = Lens.lens (\GetEnabledStandardsResponse' {standardsSubscriptions} -> standardsSubscriptions) (\s@GetEnabledStandardsResponse' {} a -> s {standardsSubscriptions = a} :: GetEnabledStandardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEnabledStandardsResponse_httpStatus :: Lens.Lens' GetEnabledStandardsResponse Prelude.Int
getEnabledStandardsResponse_httpStatus = Lens.lens (\GetEnabledStandardsResponse' {httpStatus} -> httpStatus) (\s@GetEnabledStandardsResponse' {} a -> s {httpStatus = a} :: GetEnabledStandardsResponse)

instance Prelude.NFData GetEnabledStandardsResponse where
  rnf GetEnabledStandardsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf standardsSubscriptions
      `Prelude.seq` Prelude.rnf httpStatus
