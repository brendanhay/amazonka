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
-- Module      : Amazonka.GuardDuty.GetFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon GuardDuty findings specified by finding IDs.
module Amazonka.GuardDuty.GetFindings
  ( -- * Creating a Request
    GetFindings (..),
    newGetFindings,

    -- * Request Lenses
    getFindings_sortCriteria,
    getFindings_detectorId,
    getFindings_findingIds,

    -- * Destructuring the Response
    GetFindingsResponse (..),
    newGetFindingsResponse,

    -- * Response Lenses
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFindings' smart constructor.
data GetFindings = GetFindings'
  { -- | Represents the criteria used for sorting findings.
    sortCriteria :: Prelude.Maybe SortCriteria,
    -- | The ID of the detector that specifies the GuardDuty service whose
    -- findings you want to retrieve.
    detectorId :: Prelude.Text,
    -- | The IDs of the findings that you want to retrieve.
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
-- 'sortCriteria', 'getFindings_sortCriteria' - Represents the criteria used for sorting findings.
--
-- 'detectorId', 'getFindings_detectorId' - The ID of the detector that specifies the GuardDuty service whose
-- findings you want to retrieve.
--
-- 'findingIds', 'getFindings_findingIds' - The IDs of the findings that you want to retrieve.
newGetFindings ::
  -- | 'detectorId'
  Prelude.Text ->
  GetFindings
newGetFindings pDetectorId_ =
  GetFindings'
    { sortCriteria = Prelude.Nothing,
      detectorId = pDetectorId_,
      findingIds = Prelude.mempty
    }

-- | Represents the criteria used for sorting findings.
getFindings_sortCriteria :: Lens.Lens' GetFindings (Prelude.Maybe SortCriteria)
getFindings_sortCriteria = Lens.lens (\GetFindings' {sortCriteria} -> sortCriteria) (\s@GetFindings' {} a -> s {sortCriteria = a} :: GetFindings)

-- | The ID of the detector that specifies the GuardDuty service whose
-- findings you want to retrieve.
getFindings_detectorId :: Lens.Lens' GetFindings Prelude.Text
getFindings_detectorId = Lens.lens (\GetFindings' {detectorId} -> detectorId) (\s@GetFindings' {} a -> s {detectorId = a} :: GetFindings)

-- | The IDs of the findings that you want to retrieve.
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "findings" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetFindings where
  hashWithSalt _salt GetFindings' {..} =
    _salt
      `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` findingIds

instance Prelude.NFData GetFindings where
  rnf GetFindings' {..} =
    Prelude.rnf sortCriteria
      `Prelude.seq` Prelude.rnf detectorId
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
  toPath GetFindings' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/findings/get"]

instance Data.ToQuery GetFindings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of findings.
    findings :: [Finding]
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
-- 'httpStatus', 'getFindingsResponse_httpStatus' - The response's http status code.
--
-- 'findings', 'getFindingsResponse_findings' - A list of findings.
newGetFindingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsResponse
newGetFindingsResponse pHttpStatus_ =
  GetFindingsResponse'
    { httpStatus = pHttpStatus_,
      findings = Prelude.mempty
    }

-- | The response's http status code.
getFindingsResponse_httpStatus :: Lens.Lens' GetFindingsResponse Prelude.Int
getFindingsResponse_httpStatus = Lens.lens (\GetFindingsResponse' {httpStatus} -> httpStatus) (\s@GetFindingsResponse' {} a -> s {httpStatus = a} :: GetFindingsResponse)

-- | A list of findings.
getFindingsResponse_findings :: Lens.Lens' GetFindingsResponse [Finding]
getFindingsResponse_findings = Lens.lens (\GetFindingsResponse' {findings} -> findings) (\s@GetFindingsResponse' {} a -> s {findings = a} :: GetFindingsResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetFindingsResponse where
  rnf GetFindingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf findings
