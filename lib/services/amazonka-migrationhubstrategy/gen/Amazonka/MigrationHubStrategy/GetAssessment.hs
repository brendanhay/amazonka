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
-- Module      : Amazonka.MigrationHubStrategy.GetAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of an on-going assessment.
module Amazonka.MigrationHubStrategy.GetAssessment
  ( -- * Creating a Request
    GetAssessment (..),
    newGetAssessment,

    -- * Request Lenses
    getAssessment_id,

    -- * Destructuring the Response
    GetAssessmentResponse (..),
    newGetAssessmentResponse,

    -- * Response Lenses
    getAssessmentResponse_assessmentTargets,
    getAssessmentResponse_dataCollectionDetails,
    getAssessmentResponse_id,
    getAssessmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssessment' smart constructor.
data GetAssessment = GetAssessment'
  { -- | The @assessmentid@ returned by StartAssessment.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getAssessment_id' - The @assessmentid@ returned by StartAssessment.
newGetAssessment ::
  -- | 'id'
  Prelude.Text ->
  GetAssessment
newGetAssessment pId_ = GetAssessment' {id = pId_}

-- | The @assessmentid@ returned by StartAssessment.
getAssessment_id :: Lens.Lens' GetAssessment Prelude.Text
getAssessment_id = Lens.lens (\GetAssessment' {id} -> id) (\s@GetAssessment' {} a -> s {id = a} :: GetAssessment)

instance Core.AWSRequest GetAssessment where
  type
    AWSResponse GetAssessment =
      GetAssessmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssessmentResponse'
            Prelude.<$> ( x
                            Data..?> "assessmentTargets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "dataCollectionDetails")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssessment where
  hashWithSalt _salt GetAssessment' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetAssessment where
  rnf GetAssessment' {..} = Prelude.rnf id

instance Data.ToHeaders GetAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssessment where
  toPath GetAssessment' {..} =
    Prelude.mconcat ["/get-assessment/", Data.toBS id]

instance Data.ToQuery GetAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssessmentResponse' smart constructor.
data GetAssessmentResponse = GetAssessmentResponse'
  { -- | List of criteria for assessment.
    assessmentTargets :: Prelude.Maybe [AssessmentTarget],
    -- | Detailed information about the assessment.
    dataCollectionDetails :: Prelude.Maybe DataCollectionDetails,
    -- | The ID for the specific assessment task.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentTargets', 'getAssessmentResponse_assessmentTargets' - List of criteria for assessment.
--
-- 'dataCollectionDetails', 'getAssessmentResponse_dataCollectionDetails' - Detailed information about the assessment.
--
-- 'id', 'getAssessmentResponse_id' - The ID for the specific assessment task.
--
-- 'httpStatus', 'getAssessmentResponse_httpStatus' - The response's http status code.
newGetAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssessmentResponse
newGetAssessmentResponse pHttpStatus_ =
  GetAssessmentResponse'
    { assessmentTargets =
        Prelude.Nothing,
      dataCollectionDetails = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of criteria for assessment.
getAssessmentResponse_assessmentTargets :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe [AssessmentTarget])
getAssessmentResponse_assessmentTargets = Lens.lens (\GetAssessmentResponse' {assessmentTargets} -> assessmentTargets) (\s@GetAssessmentResponse' {} a -> s {assessmentTargets = a} :: GetAssessmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Detailed information about the assessment.
getAssessmentResponse_dataCollectionDetails :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe DataCollectionDetails)
getAssessmentResponse_dataCollectionDetails = Lens.lens (\GetAssessmentResponse' {dataCollectionDetails} -> dataCollectionDetails) (\s@GetAssessmentResponse' {} a -> s {dataCollectionDetails = a} :: GetAssessmentResponse)

-- | The ID for the specific assessment task.
getAssessmentResponse_id :: Lens.Lens' GetAssessmentResponse (Prelude.Maybe Prelude.Text)
getAssessmentResponse_id = Lens.lens (\GetAssessmentResponse' {id} -> id) (\s@GetAssessmentResponse' {} a -> s {id = a} :: GetAssessmentResponse)

-- | The response's http status code.
getAssessmentResponse_httpStatus :: Lens.Lens' GetAssessmentResponse Prelude.Int
getAssessmentResponse_httpStatus = Lens.lens (\GetAssessmentResponse' {httpStatus} -> httpStatus) (\s@GetAssessmentResponse' {} a -> s {httpStatus = a} :: GetAssessmentResponse)

instance Prelude.NFData GetAssessmentResponse where
  rnf GetAssessmentResponse' {..} =
    Prelude.rnf assessmentTargets `Prelude.seq`
      Prelude.rnf dataCollectionDetails `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf httpStatus
