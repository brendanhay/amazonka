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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopAssessmentRun' smart constructor.
data StopAssessmentRun = StopAssessmentRun'
  { -- | An input option that can be set to either START_EVALUATION or
    -- SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS
    -- agent from collecting data and begins the results evaluation and the
    -- findings generation process. SKIP_EVALUATION cancels the assessment run
    -- immediately, after which no findings are generated.
    stopAction :: Prelude.Maybe StopAction,
    -- | The ARN of the assessment run that you want to stop.
    assessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopAssessmentRun
newStopAssessmentRun pAssessmentRunArn_ =
  StopAssessmentRun'
    { stopAction = Prelude.Nothing,
      assessmentRunArn = pAssessmentRunArn_
    }

-- | An input option that can be set to either START_EVALUATION or
-- SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS
-- agent from collecting data and begins the results evaluation and the
-- findings generation process. SKIP_EVALUATION cancels the assessment run
-- immediately, after which no findings are generated.
stopAssessmentRun_stopAction :: Lens.Lens' StopAssessmentRun (Prelude.Maybe StopAction)
stopAssessmentRun_stopAction = Lens.lens (\StopAssessmentRun' {stopAction} -> stopAction) (\s@StopAssessmentRun' {} a -> s {stopAction = a} :: StopAssessmentRun)

-- | The ARN of the assessment run that you want to stop.
stopAssessmentRun_assessmentRunArn :: Lens.Lens' StopAssessmentRun Prelude.Text
stopAssessmentRun_assessmentRunArn = Lens.lens (\StopAssessmentRun' {assessmentRunArn} -> assessmentRunArn) (\s@StopAssessmentRun' {} a -> s {assessmentRunArn = a} :: StopAssessmentRun)

instance Prelude.AWSRequest StopAssessmentRun where
  type Rs StopAssessmentRun = StopAssessmentRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopAssessmentRunResponse'

instance Prelude.Hashable StopAssessmentRun

instance Prelude.NFData StopAssessmentRun

instance Prelude.ToHeaders StopAssessmentRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.StopAssessmentRun" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopAssessmentRun where
  toJSON StopAssessmentRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("stopAction" Prelude..=) Prelude.<$> stopAction,
            Prelude.Just
              ("assessmentRunArn" Prelude..= assessmentRunArn)
          ]
      )

instance Prelude.ToPath StopAssessmentRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopAssessmentRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse = StopAssessmentRunResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopAssessmentRunResponse ::
  StopAssessmentRunResponse
newStopAssessmentRunResponse =
  StopAssessmentRunResponse'

instance Prelude.NFData StopAssessmentRunResponse
