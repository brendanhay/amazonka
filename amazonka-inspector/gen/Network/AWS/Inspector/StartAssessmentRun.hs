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
-- Module      : Network.AWS.Inspector.StartAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the assessment run specified by the ARN of the assessment
-- template. For this API to function properly, you must not exceed the
-- limit of running up to 500 concurrent agents per AWS account.
module Network.AWS.Inspector.StartAssessmentRun
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { -- | You can specify the name for the assessment run. The name must be unique
    -- for the assessment template whose ARN is used to start the assessment
    -- run.
    assessmentRunName :: Core.Maybe Core.Text,
    -- | The ARN of the assessment template of the assessment run that you want
    -- to start.
    assessmentTemplateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StartAssessmentRun
newStartAssessmentRun pAssessmentTemplateArn_ =
  StartAssessmentRun'
    { assessmentRunName =
        Core.Nothing,
      assessmentTemplateArn = pAssessmentTemplateArn_
    }

-- | You can specify the name for the assessment run. The name must be unique
-- for the assessment template whose ARN is used to start the assessment
-- run.
startAssessmentRun_assessmentRunName :: Lens.Lens' StartAssessmentRun (Core.Maybe Core.Text)
startAssessmentRun_assessmentRunName = Lens.lens (\StartAssessmentRun' {assessmentRunName} -> assessmentRunName) (\s@StartAssessmentRun' {} a -> s {assessmentRunName = a} :: StartAssessmentRun)

-- | The ARN of the assessment template of the assessment run that you want
-- to start.
startAssessmentRun_assessmentTemplateArn :: Lens.Lens' StartAssessmentRun Core.Text
startAssessmentRun_assessmentTemplateArn = Lens.lens (\StartAssessmentRun' {assessmentTemplateArn} -> assessmentTemplateArn) (\s@StartAssessmentRun' {} a -> s {assessmentTemplateArn = a} :: StartAssessmentRun)

instance Core.AWSRequest StartAssessmentRun where
  type
    AWSResponse StartAssessmentRun =
      StartAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssessmentRunResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "assessmentRunArn")
      )

instance Core.Hashable StartAssessmentRun

instance Core.NFData StartAssessmentRun

instance Core.ToHeaders StartAssessmentRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.StartAssessmentRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartAssessmentRun where
  toJSON StartAssessmentRun' {..} =
    Core.object
      ( Core.catMaybes
          [ ("assessmentRunName" Core..=)
              Core.<$> assessmentRunName,
            Core.Just
              ( "assessmentTemplateArn"
                  Core..= assessmentTemplateArn
              )
          ]
      )

instance Core.ToPath StartAssessmentRun where
  toPath = Core.const "/"

instance Core.ToQuery StartAssessmentRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ARN of the assessment run that has been started.
    assessmentRunArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'assessmentRunArn'
  Core.Text ->
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
startAssessmentRunResponse_httpStatus :: Lens.Lens' StartAssessmentRunResponse Core.Int
startAssessmentRunResponse_httpStatus = Lens.lens (\StartAssessmentRunResponse' {httpStatus} -> httpStatus) (\s@StartAssessmentRunResponse' {} a -> s {httpStatus = a} :: StartAssessmentRunResponse)

-- | The ARN of the assessment run that has been started.
startAssessmentRunResponse_assessmentRunArn :: Lens.Lens' StartAssessmentRunResponse Core.Text
startAssessmentRunResponse_assessmentRunArn = Lens.lens (\StartAssessmentRunResponse' {assessmentRunArn} -> assessmentRunArn) (\s@StartAssessmentRunResponse' {} a -> s {assessmentRunArn = a} :: StartAssessmentRunResponse)

instance Core.NFData StartAssessmentRunResponse
