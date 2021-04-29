{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingRules
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetSamplingRules' smart constructor.
data GetSamplingRules = GetSamplingRules'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager GetSamplingRules where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getSamplingRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getSamplingRulesResponse_samplingRuleRecords
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getSamplingRules_nextToken
          Lens..~ rs
          Lens.^? getSamplingRulesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetSamplingRules where
  type Rs GetSamplingRules = GetSamplingRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingRulesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "SamplingRuleRecords"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSamplingRules

instance Prelude.NFData GetSamplingRules

instance Prelude.ToHeaders GetSamplingRules where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetSamplingRules where
  toJSON GetSamplingRules' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("NextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath GetSamplingRules where
  toPath = Prelude.const "/GetSamplingRules"

instance Prelude.ToQuery GetSamplingRules where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getSamplingRulesResponse_samplingRuleRecords = Lens.lens (\GetSamplingRulesResponse' {samplingRuleRecords} -> samplingRuleRecords) (\s@GetSamplingRulesResponse' {} a -> s {samplingRuleRecords = a} :: GetSamplingRulesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getSamplingRulesResponse_httpStatus :: Lens.Lens' GetSamplingRulesResponse Prelude.Int
getSamplingRulesResponse_httpStatus = Lens.lens (\GetSamplingRulesResponse' {httpStatus} -> httpStatus) (\s@GetSamplingRulesResponse' {} a -> s {httpStatus = a} :: GetSamplingRulesResponse)

instance Prelude.NFData GetSamplingRulesResponse
