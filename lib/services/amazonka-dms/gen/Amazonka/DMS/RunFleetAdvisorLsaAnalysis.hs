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
-- Module      : Amazonka.DMS.RunFleetAdvisorLsaAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs large-scale assessment (LSA) analysis on every Fleet Advisor
-- collector in your account.
module Amazonka.DMS.RunFleetAdvisorLsaAnalysis
  ( -- * Creating a Request
    RunFleetAdvisorLsaAnalysis (..),
    newRunFleetAdvisorLsaAnalysis,

    -- * Destructuring the Response
    RunFleetAdvisorLsaAnalysisResponse (..),
    newRunFleetAdvisorLsaAnalysisResponse,

    -- * Response Lenses
    runFleetAdvisorLsaAnalysisResponse_lsaAnalysisId,
    runFleetAdvisorLsaAnalysisResponse_status,
    runFleetAdvisorLsaAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRunFleetAdvisorLsaAnalysis' smart constructor.
data RunFleetAdvisorLsaAnalysis = RunFleetAdvisorLsaAnalysis'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunFleetAdvisorLsaAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRunFleetAdvisorLsaAnalysis ::
  RunFleetAdvisorLsaAnalysis
newRunFleetAdvisorLsaAnalysis =
  RunFleetAdvisorLsaAnalysis'

instance Core.AWSRequest RunFleetAdvisorLsaAnalysis where
  type
    AWSResponse RunFleetAdvisorLsaAnalysis =
      RunFleetAdvisorLsaAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RunFleetAdvisorLsaAnalysisResponse'
            Prelude.<$> (x Core..?> "LsaAnalysisId")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunFleetAdvisorLsaAnalysis where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData RunFleetAdvisorLsaAnalysis where
  rnf _ = ()

instance Core.ToHeaders RunFleetAdvisorLsaAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.RunFleetAdvisorLsaAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RunFleetAdvisorLsaAnalysis where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath RunFleetAdvisorLsaAnalysis where
  toPath = Prelude.const "/"

instance Core.ToQuery RunFleetAdvisorLsaAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRunFleetAdvisorLsaAnalysisResponse' smart constructor.
data RunFleetAdvisorLsaAnalysisResponse = RunFleetAdvisorLsaAnalysisResponse'
  { -- | The ID of the LSA analysis run.
    lsaAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The status of the LSA analysis, for example @COMPLETED@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunFleetAdvisorLsaAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lsaAnalysisId', 'runFleetAdvisorLsaAnalysisResponse_lsaAnalysisId' - The ID of the LSA analysis run.
--
-- 'status', 'runFleetAdvisorLsaAnalysisResponse_status' - The status of the LSA analysis, for example @COMPLETED@.
--
-- 'httpStatus', 'runFleetAdvisorLsaAnalysisResponse_httpStatus' - The response's http status code.
newRunFleetAdvisorLsaAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RunFleetAdvisorLsaAnalysisResponse
newRunFleetAdvisorLsaAnalysisResponse pHttpStatus_ =
  RunFleetAdvisorLsaAnalysisResponse'
    { lsaAnalysisId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the LSA analysis run.
runFleetAdvisorLsaAnalysisResponse_lsaAnalysisId :: Lens.Lens' RunFleetAdvisorLsaAnalysisResponse (Prelude.Maybe Prelude.Text)
runFleetAdvisorLsaAnalysisResponse_lsaAnalysisId = Lens.lens (\RunFleetAdvisorLsaAnalysisResponse' {lsaAnalysisId} -> lsaAnalysisId) (\s@RunFleetAdvisorLsaAnalysisResponse' {} a -> s {lsaAnalysisId = a} :: RunFleetAdvisorLsaAnalysisResponse)

-- | The status of the LSA analysis, for example @COMPLETED@.
runFleetAdvisorLsaAnalysisResponse_status :: Lens.Lens' RunFleetAdvisorLsaAnalysisResponse (Prelude.Maybe Prelude.Text)
runFleetAdvisorLsaAnalysisResponse_status = Lens.lens (\RunFleetAdvisorLsaAnalysisResponse' {status} -> status) (\s@RunFleetAdvisorLsaAnalysisResponse' {} a -> s {status = a} :: RunFleetAdvisorLsaAnalysisResponse)

-- | The response's http status code.
runFleetAdvisorLsaAnalysisResponse_httpStatus :: Lens.Lens' RunFleetAdvisorLsaAnalysisResponse Prelude.Int
runFleetAdvisorLsaAnalysisResponse_httpStatus = Lens.lens (\RunFleetAdvisorLsaAnalysisResponse' {httpStatus} -> httpStatus) (\s@RunFleetAdvisorLsaAnalysisResponse' {} a -> s {httpStatus = a} :: RunFleetAdvisorLsaAnalysisResponse)

instance
  Prelude.NFData
    RunFleetAdvisorLsaAnalysisResponse
  where
  rnf RunFleetAdvisorLsaAnalysisResponse' {..} =
    Prelude.rnf lsaAnalysisId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
