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
-- Module      : Amazonka.IoTEvents.StartDetectorModelAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs an analysis of your detector model. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-analyze-api.html Troubleshooting a detector model>
-- in the /AWS IoT Events Developer Guide/.
module Amazonka.IoTEvents.StartDetectorModelAnalysis
  ( -- * Creating a Request
    StartDetectorModelAnalysis (..),
    newStartDetectorModelAnalysis,

    -- * Request Lenses
    startDetectorModelAnalysis_detectorModelDefinition,

    -- * Destructuring the Response
    StartDetectorModelAnalysisResponse (..),
    newStartDetectorModelAnalysisResponse,

    -- * Response Lenses
    startDetectorModelAnalysisResponse_analysisId,
    startDetectorModelAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDetectorModelAnalysis' smart constructor.
data StartDetectorModelAnalysis = StartDetectorModelAnalysis'
  { detectorModelDefinition :: DetectorModelDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDetectorModelAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorModelDefinition', 'startDetectorModelAnalysis_detectorModelDefinition' - Undocumented member.
newStartDetectorModelAnalysis ::
  -- | 'detectorModelDefinition'
  DetectorModelDefinition ->
  StartDetectorModelAnalysis
newStartDetectorModelAnalysis
  pDetectorModelDefinition_ =
    StartDetectorModelAnalysis'
      { detectorModelDefinition =
          pDetectorModelDefinition_
      }

-- | Undocumented member.
startDetectorModelAnalysis_detectorModelDefinition :: Lens.Lens' StartDetectorModelAnalysis DetectorModelDefinition
startDetectorModelAnalysis_detectorModelDefinition = Lens.lens (\StartDetectorModelAnalysis' {detectorModelDefinition} -> detectorModelDefinition) (\s@StartDetectorModelAnalysis' {} a -> s {detectorModelDefinition = a} :: StartDetectorModelAnalysis)

instance Core.AWSRequest StartDetectorModelAnalysis where
  type
    AWSResponse StartDetectorModelAnalysis =
      StartDetectorModelAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDetectorModelAnalysisResponse'
            Prelude.<$> (x Data..?> "analysisId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDetectorModelAnalysis where
  hashWithSalt _salt StartDetectorModelAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` detectorModelDefinition

instance Prelude.NFData StartDetectorModelAnalysis where
  rnf StartDetectorModelAnalysis' {..} =
    Prelude.rnf detectorModelDefinition

instance Data.ToHeaders StartDetectorModelAnalysis where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartDetectorModelAnalysis where
  toJSON StartDetectorModelAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "detectorModelDefinition"
                  Data..= detectorModelDefinition
              )
          ]
      )

instance Data.ToPath StartDetectorModelAnalysis where
  toPath = Prelude.const "/analysis/detector-models/"

instance Data.ToQuery StartDetectorModelAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDetectorModelAnalysisResponse' smart constructor.
data StartDetectorModelAnalysisResponse = StartDetectorModelAnalysisResponse'
  { -- | The ID that you can use to retrieve the analysis result.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDetectorModelAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisId', 'startDetectorModelAnalysisResponse_analysisId' - The ID that you can use to retrieve the analysis result.
--
-- 'httpStatus', 'startDetectorModelAnalysisResponse_httpStatus' - The response's http status code.
newStartDetectorModelAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDetectorModelAnalysisResponse
newStartDetectorModelAnalysisResponse pHttpStatus_ =
  StartDetectorModelAnalysisResponse'
    { analysisId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID that you can use to retrieve the analysis result.
startDetectorModelAnalysisResponse_analysisId :: Lens.Lens' StartDetectorModelAnalysisResponse (Prelude.Maybe Prelude.Text)
startDetectorModelAnalysisResponse_analysisId = Lens.lens (\StartDetectorModelAnalysisResponse' {analysisId} -> analysisId) (\s@StartDetectorModelAnalysisResponse' {} a -> s {analysisId = a} :: StartDetectorModelAnalysisResponse)

-- | The response's http status code.
startDetectorModelAnalysisResponse_httpStatus :: Lens.Lens' StartDetectorModelAnalysisResponse Prelude.Int
startDetectorModelAnalysisResponse_httpStatus = Lens.lens (\StartDetectorModelAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartDetectorModelAnalysisResponse' {} a -> s {httpStatus = a} :: StartDetectorModelAnalysisResponse)

instance
  Prelude.NFData
    StartDetectorModelAnalysisResponse
  where
  rnf StartDetectorModelAnalysisResponse' {..} =
    Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf httpStatus
