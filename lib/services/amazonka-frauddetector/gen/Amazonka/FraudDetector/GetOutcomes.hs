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
-- Module      : Amazonka.FraudDetector.GetOutcomes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets one or more outcomes. This is a paginated API. If you provide a
-- null @maxResults@, this actions retrieves a maximum of 100 records per
-- page. If you provide a @maxResults@, the value must be between 50 and
-- 100. To get the next page results, provide the pagination token from the
-- @GetOutcomesResult@ as part of your request. A null pagination token
-- fetches the records from the beginning.
module Amazonka.FraudDetector.GetOutcomes
  ( -- * Creating a Request
    GetOutcomes (..),
    newGetOutcomes,

    -- * Request Lenses
    getOutcomes_name,
    getOutcomes_nextToken,
    getOutcomes_maxResults,

    -- * Destructuring the Response
    GetOutcomesResponse (..),
    newGetOutcomesResponse,

    -- * Response Lenses
    getOutcomesResponse_nextToken,
    getOutcomesResponse_outcomes,
    getOutcomesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOutcomes' smart constructor.
data GetOutcomes = GetOutcomes'
  { -- | The name of the outcome or outcomes to get.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next page token for the request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutcomes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getOutcomes_name' - The name of the outcome or outcomes to get.
--
-- 'nextToken', 'getOutcomes_nextToken' - The next page token for the request.
--
-- 'maxResults', 'getOutcomes_maxResults' - The maximum number of objects to return for the request.
newGetOutcomes ::
  GetOutcomes
newGetOutcomes =
  GetOutcomes'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name of the outcome or outcomes to get.
getOutcomes_name :: Lens.Lens' GetOutcomes (Prelude.Maybe Prelude.Text)
getOutcomes_name = Lens.lens (\GetOutcomes' {name} -> name) (\s@GetOutcomes' {} a -> s {name = a} :: GetOutcomes)

-- | The next page token for the request.
getOutcomes_nextToken :: Lens.Lens' GetOutcomes (Prelude.Maybe Prelude.Text)
getOutcomes_nextToken = Lens.lens (\GetOutcomes' {nextToken} -> nextToken) (\s@GetOutcomes' {} a -> s {nextToken = a} :: GetOutcomes)

-- | The maximum number of objects to return for the request.
getOutcomes_maxResults :: Lens.Lens' GetOutcomes (Prelude.Maybe Prelude.Natural)
getOutcomes_maxResults = Lens.lens (\GetOutcomes' {maxResults} -> maxResults) (\s@GetOutcomes' {} a -> s {maxResults = a} :: GetOutcomes)

instance Core.AWSRequest GetOutcomes where
  type AWSResponse GetOutcomes = GetOutcomesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOutcomesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "outcomes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOutcomes where
  hashWithSalt _salt GetOutcomes' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetOutcomes where
  rnf GetOutcomes' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetOutcomes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetOutcomes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetOutcomes where
  toJSON GetOutcomes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetOutcomes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetOutcomes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOutcomesResponse' smart constructor.
data GetOutcomesResponse = GetOutcomesResponse'
  { -- | The next page token for subsequent requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The outcomes.
    outcomes :: Prelude.Maybe [Outcome],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutcomesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getOutcomesResponse_nextToken' - The next page token for subsequent requests.
--
-- 'outcomes', 'getOutcomesResponse_outcomes' - The outcomes.
--
-- 'httpStatus', 'getOutcomesResponse_httpStatus' - The response's http status code.
newGetOutcomesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOutcomesResponse
newGetOutcomesResponse pHttpStatus_ =
  GetOutcomesResponse'
    { nextToken = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next page token for subsequent requests.
getOutcomesResponse_nextToken :: Lens.Lens' GetOutcomesResponse (Prelude.Maybe Prelude.Text)
getOutcomesResponse_nextToken = Lens.lens (\GetOutcomesResponse' {nextToken} -> nextToken) (\s@GetOutcomesResponse' {} a -> s {nextToken = a} :: GetOutcomesResponse)

-- | The outcomes.
getOutcomesResponse_outcomes :: Lens.Lens' GetOutcomesResponse (Prelude.Maybe [Outcome])
getOutcomesResponse_outcomes = Lens.lens (\GetOutcomesResponse' {outcomes} -> outcomes) (\s@GetOutcomesResponse' {} a -> s {outcomes = a} :: GetOutcomesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOutcomesResponse_httpStatus :: Lens.Lens' GetOutcomesResponse Prelude.Int
getOutcomesResponse_httpStatus = Lens.lens (\GetOutcomesResponse' {httpStatus} -> httpStatus) (\s@GetOutcomesResponse' {} a -> s {httpStatus = a} :: GetOutcomesResponse)

instance Prelude.NFData GetOutcomesResponse where
  rnf GetOutcomesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf httpStatus
