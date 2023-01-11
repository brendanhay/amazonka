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
-- Module      : Amazonka.Inspector.StopAssessmentRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment run that is specified by the ARN of the assessment
-- run.
module Amazonka.Inspector.StopAssessmentRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StopAssessmentRun where
  type
    AWSResponse StopAssessmentRun =
      StopAssessmentRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopAssessmentRunResponse'

instance Prelude.Hashable StopAssessmentRun where
  hashWithSalt _salt StopAssessmentRun' {..} =
    _salt `Prelude.hashWithSalt` stopAction
      `Prelude.hashWithSalt` assessmentRunArn

instance Prelude.NFData StopAssessmentRun where
  rnf StopAssessmentRun' {..} =
    Prelude.rnf stopAction
      `Prelude.seq` Prelude.rnf assessmentRunArn

instance Data.ToHeaders StopAssessmentRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.StopAssessmentRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopAssessmentRun where
  toJSON StopAssessmentRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("stopAction" Data..=) Prelude.<$> stopAction,
            Prelude.Just
              ("assessmentRunArn" Data..= assessmentRunArn)
          ]
      )

instance Data.ToPath StopAssessmentRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StopAssessmentRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse = StopAssessmentRunResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAssessmentRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopAssessmentRunResponse ::
  StopAssessmentRunResponse
newStopAssessmentRunResponse =
  StopAssessmentRunResponse'

instance Prelude.NFData StopAssessmentRunResponse where
  rnf _ = ()
