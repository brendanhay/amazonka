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
-- Module      : Network.AWS.Inspector.StopAssessmentRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment run that is specified by the ARN of the assessment
-- run.
module Network.AWS.Inspector.StopAssessmentRun
  ( -- * Creating a Request
    StopAssessmentRun (..),
    newStopAssessmentRun,

    -- * Request Lenses
    stopAssessmentRun_stopAction,
    stopAssessmentRun_assessmentRunArn,

    -- * Destructuring the Response
    StopAssessmentRunResponse (..),
    newStopAssessmentRunResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopAssessmentRun' smart constructor.
data StopAssessmentRun = StopAssessmentRun'
  { -- | An input option that can be set to either START_EVALUATION or
    -- SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS
    -- agent from collecting data and begins the results evaluation and the
    -- findings generation process. SKIP_EVALUATION cancels the assessment run
    -- immediately, after which no findings are generated.
    stopAction :: Core.Maybe StopAction,
    -- | The ARN of the assessment run that you want to stop.
    assessmentRunArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopAssessmentRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopAction', 'stopAssessmentRun_stopAction' - An input option that can be set to either START_EVALUATION or
-- SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS
-- agent from collecting data and begins the results evaluation and the
-- findings generation process. SKIP_EVALUATION cancels the assessment run
-- immediately, after which no findings are generated.
--
-- 'assessmentRunArn', 'stopAssessmentRun_assessmentRunArn' - The ARN of the assessment run that you want to stop.
newStopAssessmentRun ::
  -- | 'assessmentRunArn'
  Core.Text ->
  StopAssessmentRun
newStopAssessmentRun pAssessmentRunArn_ =
  StopAssessmentRun'
    { stopAction = Core.Nothing,
      assessmentRunArn = pAssessmentRunArn_
    }

-- | An input option that can be set to either START_EVALUATION or
-- SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS
-- agent from collecting data and begins the results evaluation and the
-- findings generation process. SKIP_EVALUATION cancels the assessment run
-- immediately, after which no findings are generated.
stopAssessmentRun_stopAction :: Lens.Lens' StopAssessmentRun (Core.Maybe StopAction)
stopAssessmentRun_stopAction = Lens.lens (\StopAssessmentRun' {stopAction} -> stopAction) (\s@StopAssessmentRun' {} a -> s {stopAction = a} :: StopAssessmentRun)

-- | The ARN of the assessment run that you want to stop.
stopAssessmentRun_assessmentRunArn :: Lens.Lens' StopAssessmentRun Core.Text
stopAssessmentRun_assessmentRunArn = Lens.lens (\StopAssessmentRun' {assessmentRunArn} -> assessmentRunArn) (\s@StopAssessmentRun' {} a -> s {assessmentRunArn = a} :: StopAssessmentRun)

instance Core.AWSRequest StopAssessmentRun where
  type
    AWSResponse StopAssessmentRun =
      StopAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopAssessmentRunResponse'

instance Core.Hashable StopAssessmentRun

instance Core.NFData StopAssessmentRun

instance Core.ToHeaders StopAssessmentRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.StopAssessmentRun" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopAssessmentRun where
  toJSON StopAssessmentRun' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stopAction" Core..=) Core.<$> stopAction,
            Core.Just
              ("assessmentRunArn" Core..= assessmentRunArn)
          ]
      )

instance Core.ToPath StopAssessmentRun where
  toPath = Core.const "/"

instance Core.ToQuery StopAssessmentRun where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse = StopAssessmentRunResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopAssessmentRunResponse ::
  StopAssessmentRunResponse
newStopAssessmentRunResponse =
  StopAssessmentRunResponse'

instance Core.NFData StopAssessmentRunResponse
