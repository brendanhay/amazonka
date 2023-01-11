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
-- Module      : Amazonka.MigrationHubStrategy.StopAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment of an on-premises environment.
module Amazonka.MigrationHubStrategy.StopAssessment
  ( -- * Creating a Request
    StopAssessment (..),
    newStopAssessment,

    -- * Request Lenses
    stopAssessment_assessmentId,

    -- * Destructuring the Response
    StopAssessmentResponse (..),
    newStopAssessmentResponse,

    -- * Response Lenses
    stopAssessmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopAssessment' smart constructor.
data StopAssessment = StopAssessment'
  { -- | The @assessmentId@ returned by StartAssessment.
    assessmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentId', 'stopAssessment_assessmentId' - The @assessmentId@ returned by StartAssessment.
newStopAssessment ::
  -- | 'assessmentId'
  Prelude.Text ->
  StopAssessment
newStopAssessment pAssessmentId_ =
  StopAssessment' {assessmentId = pAssessmentId_}

-- | The @assessmentId@ returned by StartAssessment.
stopAssessment_assessmentId :: Lens.Lens' StopAssessment Prelude.Text
stopAssessment_assessmentId = Lens.lens (\StopAssessment' {assessmentId} -> assessmentId) (\s@StopAssessment' {} a -> s {assessmentId = a} :: StopAssessment)

instance Core.AWSRequest StopAssessment where
  type
    AWSResponse StopAssessment =
      StopAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAssessmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopAssessment where
  hashWithSalt _salt StopAssessment' {..} =
    _salt `Prelude.hashWithSalt` assessmentId

instance Prelude.NFData StopAssessment where
  rnf StopAssessment' {..} = Prelude.rnf assessmentId

instance Data.ToHeaders StopAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopAssessment where
  toJSON StopAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("assessmentId" Data..= assessmentId)]
      )

instance Data.ToPath StopAssessment where
  toPath = Prelude.const "/stop-assessment"

instance Data.ToQuery StopAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAssessmentResponse' smart constructor.
data StopAssessmentResponse = StopAssessmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAssessmentResponse_httpStatus' - The response's http status code.
newStopAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopAssessmentResponse
newStopAssessmentResponse pHttpStatus_ =
  StopAssessmentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopAssessmentResponse_httpStatus :: Lens.Lens' StopAssessmentResponse Prelude.Int
stopAssessmentResponse_httpStatus = Lens.lens (\StopAssessmentResponse' {httpStatus} -> httpStatus) (\s@StopAssessmentResponse' {} a -> s {httpStatus = a} :: StopAssessmentResponse)

instance Prelude.NFData StopAssessmentResponse where
  rnf StopAssessmentResponse' {..} =
    Prelude.rnf httpStatus
