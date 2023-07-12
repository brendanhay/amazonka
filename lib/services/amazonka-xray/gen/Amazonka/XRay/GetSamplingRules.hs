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
-- Module      : Amazonka.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetSamplingRules
  ( -- * Creating a Request
    GetSamplingRules (..),
    newGetSamplingRules,

    -- * Request Lenses
    getSamplingRules_nextToken,

    -- * Destructuring the Response
    GetSamplingRulesResponse (..),
    newGetSamplingRulesResponse,

    -- * Response Lenses
    getSamplingRulesResponse_nextToken,
    getSamplingRulesResponse_samplingRuleRecords,
    getSamplingRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetSamplingRules' smart constructor.
data GetSamplingRules = GetSamplingRules'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSamplingRules_nextToken' - Pagination token.
newGetSamplingRules ::
  GetSamplingRules
newGetSamplingRules =
  GetSamplingRules' {nextToken = Prelude.Nothing}

-- | Pagination token.
getSamplingRules_nextToken :: Lens.Lens' GetSamplingRules (Prelude.Maybe Prelude.Text)
getSamplingRules_nextToken = Lens.lens (\GetSamplingRules' {nextToken} -> nextToken) (\s@GetSamplingRules' {} a -> s {nextToken = a} :: GetSamplingRules)

instance Core.AWSPager GetSamplingRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSamplingRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSamplingRulesResponse_samplingRuleRecords
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getSamplingRules_nextToken
          Lens..~ rs
          Lens.^? getSamplingRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetSamplingRules where
  type
    AWSResponse GetSamplingRules =
      GetSamplingRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SamplingRuleRecords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSamplingRules where
  hashWithSalt _salt GetSamplingRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetSamplingRules where
  rnf GetSamplingRules' {..} = Prelude.rnf nextToken

instance Data.ToHeaders GetSamplingRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetSamplingRules where
  toJSON GetSamplingRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath GetSamplingRules where
  toPath = Prelude.const "/GetSamplingRules"

instance Data.ToQuery GetSamplingRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSamplingRulesResponse' smart constructor.
data GetSamplingRulesResponse = GetSamplingRulesResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Rule definitions and metadata.
    samplingRuleRecords :: Prelude.Maybe [SamplingRuleRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSamplingRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSamplingRulesResponse_nextToken' - Pagination token.
--
-- 'samplingRuleRecords', 'getSamplingRulesResponse_samplingRuleRecords' - Rule definitions and metadata.
--
-- 'httpStatus', 'getSamplingRulesResponse_httpStatus' - The response's http status code.
newGetSamplingRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSamplingRulesResponse
newGetSamplingRulesResponse pHttpStatus_ =
  GetSamplingRulesResponse'
    { nextToken =
        Prelude.Nothing,
      samplingRuleRecords = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getSamplingRulesResponse_nextToken :: Lens.Lens' GetSamplingRulesResponse (Prelude.Maybe Prelude.Text)
getSamplingRulesResponse_nextToken = Lens.lens (\GetSamplingRulesResponse' {nextToken} -> nextToken) (\s@GetSamplingRulesResponse' {} a -> s {nextToken = a} :: GetSamplingRulesResponse)

-- | Rule definitions and metadata.
getSamplingRulesResponse_samplingRuleRecords :: Lens.Lens' GetSamplingRulesResponse (Prelude.Maybe [SamplingRuleRecord])
getSamplingRulesResponse_samplingRuleRecords = Lens.lens (\GetSamplingRulesResponse' {samplingRuleRecords} -> samplingRuleRecords) (\s@GetSamplingRulesResponse' {} a -> s {samplingRuleRecords = a} :: GetSamplingRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSamplingRulesResponse_httpStatus :: Lens.Lens' GetSamplingRulesResponse Prelude.Int
getSamplingRulesResponse_httpStatus = Lens.lens (\GetSamplingRulesResponse' {httpStatus} -> httpStatus) (\s@GetSamplingRulesResponse' {} a -> s {httpStatus = a} :: GetSamplingRulesResponse)

instance Prelude.NFData GetSamplingRulesResponse where
  rnf GetSamplingRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf samplingRuleRecords
      `Prelude.seq` Prelude.rnf httpStatus
