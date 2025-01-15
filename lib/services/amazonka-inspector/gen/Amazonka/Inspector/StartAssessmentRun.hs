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
-- Module      : Amazonka.Inspector.StartAssessmentRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the assessment run specified by the ARN of the assessment
-- template. For this API to function properly, you must not exceed the
-- limit of running up to 500 concurrent agents per AWS account.
module Amazonka.Inspector.StartAssessmentRun
  ( -- * Creating a Request
    StartAssessmentRun (..),
    newStartAssessmentRun,

    -- * Request Lenses
    startAssessmentRun_assessmentRunName,
    startAssessmentRun_assessmentTemplateArn,

    -- * Destructuring the Response
    StartAssessmentRunResponse (..),
    newStartAssessmentRunResponse,

    -- * Response Lenses
    startAssessmentRunResponse_httpStatus,
    startAssessmentRunResponse_assessmentRunArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { -- | You can specify the name for the assessment run. The name must be unique
    -- for the assessment template whose ARN is used to start the assessment
    -- run.
    assessmentRunName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the assessment template of the assessment run that you want
    -- to start.
    assessmentTemplateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunName', 'startAssessmentRun_assessmentRunName' - You can specify the name for the assessment run. The name must be unique
-- for the assessment template whose ARN is used to start the assessment
-- run.
--
-- 'assessmentTemplateArn', 'startAssessmentRun_assessmentTemplateArn' - The ARN of the assessment template of the assessment run that you want
-- to start.
newStartAssessmentRun ::
  -- | 'assessmentTemplateArn'
  Prelude.Text ->
  StartAssessmentRun
newStartAssessmentRun pAssessmentTemplateArn_ =
  StartAssessmentRun'
    { assessmentRunName =
        Prelude.Nothing,
      assessmentTemplateArn = pAssessmentTemplateArn_
    }

-- | You can specify the name for the assessment run. The name must be unique
-- for the assessment template whose ARN is used to start the assessment
-- run.
startAssessmentRun_assessmentRunName :: Lens.Lens' StartAssessmentRun (Prelude.Maybe Prelude.Text)
startAssessmentRun_assessmentRunName = Lens.lens (\StartAssessmentRun' {assessmentRunName} -> assessmentRunName) (\s@StartAssessmentRun' {} a -> s {assessmentRunName = a} :: StartAssessmentRun)

-- | The ARN of the assessment template of the assessment run that you want
-- to start.
startAssessmentRun_assessmentTemplateArn :: Lens.Lens' StartAssessmentRun Prelude.Text
startAssessmentRun_assessmentTemplateArn = Lens.lens (\StartAssessmentRun' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@StartAssessmentRun' {} a -> s {assessmentTemplateArn = a} :: StartAssessmentRun)

instance Core.AWSRequest StartAssessmentRun where
  type
    AWSResponse StartAssessmentRun =
      StartAssessmentRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssessmentRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assessmentRunArn")
      )

instance Prelude.Hashable StartAssessmentRun where
  hashWithSalt _salt StartAssessmentRun' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentRunName
      `Prelude.hashWithSalt` assessmentTemplateArn

instance Prelude.NFData StartAssessmentRun where
  rnf StartAssessmentRun' {..} =
    Prelude.rnf assessmentRunName `Prelude.seq`
      Prelude.rnf assessmentTemplateArn

instance Data.ToHeaders StartAssessmentRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.StartAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAssessmentRun where
  toJSON StartAssessmentRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assessmentRunName" Data..=)
              Prelude.<$> assessmentRunName,
            Prelude.Just
              ( "assessmentTemplateArn"
                  Data..= assessmentTemplateArn
              )
          ]
      )

instance Data.ToPath StartAssessmentRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StartAssessmentRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the assessment run that has been started.
    assessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAssessmentRunResponse_httpStatus' - The response's http status code.
--
-- 'assessmentRunArn', 'startAssessmentRunResponse_assessmentRunArn' - The ARN of the assessment run that has been started.
newStartAssessmentRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assessmentRunArn'
  Prelude.Text ->
  StartAssessmentRunResponse
newStartAssessmentRunResponse
  pHttpStatus_
  pAssessmentRunArn_ =
    StartAssessmentRunResponse'
      { httpStatus =
          pHttpStatus_,
        assessmentRunArn = pAssessmentRunArn_
      }

-- | The response's http status code.
startAssessmentRunResponse_httpStatus :: Lens.Lens' StartAssessmentRunResponse Prelude.Int
startAssessmentRunResponse_httpStatus = Lens.lens (\StartAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@StartAssessmentRunResponse' {} a -> s {httpStatus = a} :: StartAssessmentRunResponse)

-- | The ARN of the assessment run that has been started.
startAssessmentRunResponse_assessmentRunArn :: Lens.Lens' StartAssessmentRunResponse Prelude.Text
startAssessmentRunResponse_assessmentRunArn = Lens.lens (\StartAssessmentRunResponse' {assessmentRunArn} -> assessmentRunArn) (\s@StartAssessmentRunResponse' {} a -> s {assessmentRunArn = a} :: StartAssessmentRunResponse)

instance Prelude.NFData StartAssessmentRunResponse where
  rnf StartAssessmentRunResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf assessmentRunArn
