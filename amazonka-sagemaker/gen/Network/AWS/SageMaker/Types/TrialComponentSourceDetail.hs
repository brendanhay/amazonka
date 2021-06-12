{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSourceDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.TransformJob

-- | Detailed information about the source of a trial component. Either
-- @ProcessingJob@ or @TrainingJob@ is returned.
--
-- /See:/ 'newTrialComponentSourceDetail' smart constructor.
data TrialComponentSourceDetail = TrialComponentSourceDetail'
  { -- | Information about a processing job that\'s the source of a trial
    -- component.
    processingJob :: Core.Maybe ProcessingJob,
    -- | Information about a training job that\'s the source of a trial
    -- component.
    trainingJob :: Core.Maybe TrainingJob,
    -- | Information about a transform job that\'s the source of a trial
    -- component.
    transformJob :: Core.Maybe TransformJob,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponentSourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingJob', 'trialComponentSourceDetail_processingJob' - Information about a processing job that\'s the source of a trial
-- component.
--
-- 'trainingJob', 'trialComponentSourceDetail_trainingJob' - Information about a training job that\'s the source of a trial
-- component.
--
-- 'transformJob', 'trialComponentSourceDetail_transformJob' - Information about a transform job that\'s the source of a trial
-- component.
--
-- 'sourceArn', 'trialComponentSourceDetail_sourceArn' - The Amazon Resource Name (ARN) of the source.
newTrialComponentSourceDetail ::
  TrialComponentSourceDetail
newTrialComponentSourceDetail =
  TrialComponentSourceDetail'
    { processingJob =
        Core.Nothing,
      trainingJob = Core.Nothing,
      transformJob = Core.Nothing,
      sourceArn = Core.Nothing
    }

-- | Information about a processing job that\'s the source of a trial
-- component.
trialComponentSourceDetail_processingJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe ProcessingJob)
trialComponentSourceDetail_processingJob = Lens.lens (\TrialComponentSourceDetail' {processingJob} -> processingJob) (\s@TrialComponentSourceDetail' {} a -> s {processingJob = a} :: TrialComponentSourceDetail)

-- | Information about a training job that\'s the source of a trial
-- component.
trialComponentSourceDetail_trainingJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe TrainingJob)
trialComponentSourceDetail_trainingJob = Lens.lens (\TrialComponentSourceDetail' {trainingJob} -> trainingJob) (\s@TrialComponentSourceDetail' {} a -> s {trainingJob = a} :: TrialComponentSourceDetail)

-- | Information about a transform job that\'s the source of a trial
-- component.
trialComponentSourceDetail_transformJob :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe TransformJob)
trialComponentSourceDetail_transformJob = Lens.lens (\TrialComponentSourceDetail' {transformJob} -> transformJob) (\s@TrialComponentSourceDetail' {} a -> s {transformJob = a} :: TrialComponentSourceDetail)

-- | The Amazon Resource Name (ARN) of the source.
trialComponentSourceDetail_sourceArn :: Lens.Lens' TrialComponentSourceDetail (Core.Maybe Core.Text)
trialComponentSourceDetail_sourceArn = Lens.lens (\TrialComponentSourceDetail' {sourceArn} -> sourceArn) (\s@TrialComponentSourceDetail' {} a -> s {sourceArn = a} :: TrialComponentSourceDetail)

instance Core.FromJSON TrialComponentSourceDetail where
  parseJSON =
    Core.withObject
      "TrialComponentSourceDetail"
      ( \x ->
          TrialComponentSourceDetail'
            Core.<$> (x Core..:? "ProcessingJob")
            Core.<*> (x Core..:? "TrainingJob")
            Core.<*> (x Core..:? "TransformJob")
            Core.<*> (x Core..:? "SourceArn")
      )

instance Core.Hashable TrialComponentSourceDetail

instance Core.NFData TrialComponentSourceDetail
