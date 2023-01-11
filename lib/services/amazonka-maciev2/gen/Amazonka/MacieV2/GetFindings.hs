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
-- Module      : Amazonka.MacieV2.GetFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of one or more findings.
module Amazonka.MacieV2.GetFindings
  ( -- * Creating a Request
    GetFindings (..),
    newGetFindings,

    -- * Request Lenses
    getFindings_sortCriteria,
    getFindings_findingIds,

    -- * Destructuring the Response
    GetFindingsResponse (..),
    newGetFindingsResponse,

    -- * Response Lenses
    getFindingsResponse_findings,
    getFindingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFindings' smart constructor.
data GetFindings = GetFindings'
  { -- | The criteria for sorting the results of the request.
    sortCriteria :: Prelude.Maybe SortCriteria,
    -- | An array of strings that lists the unique identifiers for the findings
    -- to retrieve. You can specify as many as 50 unique identifiers in this
    -- array.
    findingIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortCriteria', 'getFindings_sortCriteria' - The criteria for sorting the results of the request.
--
-- 'findingIds', 'getFindings_findingIds' - An array of strings that lists the unique identifiers for the findings
-- to retrieve. You can specify as many as 50 unique identifiers in this
-- array.
newGetFindings ::
  GetFindings
newGetFindings =
  GetFindings'
    { sortCriteria = Prelude.Nothing,
      findingIds = Prelude.mempty
    }

-- | The criteria for sorting the results of the request.
getFindings_sortCriteria :: Lens.Lens' GetFindings (Prelude.Maybe SortCriteria)
getFindings_sortCriteria = Lens.lens (\GetFindings' {sortCriteria} -> sortCriteria) (\s@GetFindings' {} a -> s {sortCriteria = a} :: GetFindings)

-- | An array of strings that lists the unique identifiers for the findings
-- to retrieve. You can specify as many as 50 unique identifiers in this
-- array.
getFindings_findingIds :: Lens.Lens' GetFindings [Prelude.Text]
getFindings_findingIds = Lens.lens (\GetFindings' {findingIds} -> findingIds) (\s@GetFindings' {} a -> s {findingIds = a} :: GetFindings) Prelude.. Lens.coerced

instance Core.AWSRequest GetFindings where
  type AWSResponse GetFindings = GetFindingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsResponse'
            Prelude.<$> (x Data..?> "findings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFindings where
  hashWithSalt _salt GetFindings' {..} =
    _salt `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` findingIds

instance Prelude.NFData GetFindings where
  rnf GetFindings' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf findingIds

instance Data.ToHeaders GetFindings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFindings where
  toJSON GetFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sortCriteria" Data..=) Prelude.<$> sortCriteria,
            Prelude.Just ("findingIds" Data..= findingIds)
          ]
      )

instance Data.ToPath GetFindings where
  toPath = Prelude.const "/findings/describe"

instance Data.ToQuery GetFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { -- | An array of objects, one for each finding that matches the criteria
    -- specified in the request.
    findings :: Prelude.Maybe [Finding],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findings', 'getFindingsResponse_findings' - An array of objects, one for each finding that matches the criteria
-- specified in the request.
--
-- 'httpStatus', 'getFindingsResponse_httpStatus' - The response's http status code.
newGetFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsResponse
newGetFindingsResponse pHttpStatus_ =
  GetFindingsResponse'
    { findings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects, one for each finding that matches the criteria
-- specified in the request.
getFindingsResponse_findings :: Lens.Lens' GetFindingsResponse (Prelude.Maybe [Finding])
getFindingsResponse_findings = Lens.lens (\GetFindingsResponse' {findings} -> findings) (\s@GetFindingsResponse' {} a -> s {findings = a} :: GetFindingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getFindingsResponse_httpStatus :: Lens.Lens' GetFindingsResponse Prelude.Int
getFindingsResponse_httpStatus = Lens.lens (\GetFindingsResponse' {httpStatus} -> httpStatus) (\s@GetFindingsResponse' {} a -> s {httpStatus = a} :: GetFindingsResponse)

instance Prelude.NFData GetFindingsResponse where
  rnf GetFindingsResponse' {..} =
    Prelude.rnf findings
      `Prelude.seq` Prelude.rnf httpStatus
