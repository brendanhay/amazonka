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
-- Module      : Network.AWS.SageMaker.Types.ModelQualityJobInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelQualityJobInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.EndpointInput
import Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input

-- | The input for the model quality monitoring job. Currently endponts are
-- supported for input for model quality monitoring jobs.
--
-- /See:/ 'newModelQualityJobInput' smart constructor.
data ModelQualityJobInput = ModelQualityJobInput'
  { endpointInput :: EndpointInput,
    -- | The ground truth label provided for the model.
    groundTruthS3Input :: MonitoringGroundTruthS3Input
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelQualityJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointInput', 'modelQualityJobInput_endpointInput' - Undocumented member.
--
-- 'groundTruthS3Input', 'modelQualityJobInput_groundTruthS3Input' - The ground truth label provided for the model.
newModelQualityJobInput ::
  -- | 'endpointInput'
  EndpointInput ->
  -- | 'groundTruthS3Input'
  MonitoringGroundTruthS3Input ->
  ModelQualityJobInput
newModelQualityJobInput
  pEndpointInput_
  pGroundTruthS3Input_ =
    ModelQualityJobInput'
      { endpointInput =
          pEndpointInput_,
        groundTruthS3Input = pGroundTruthS3Input_
      }

-- | Undocumented member.
modelQualityJobInput_endpointInput :: Lens.Lens' ModelQualityJobInput EndpointInput
modelQualityJobInput_endpointInput = Lens.lens (\ModelQualityJobInput' {endpointInput} -> endpointInput) (\s@ModelQualityJobInput' {} a -> s {endpointInput = a} :: ModelQualityJobInput)

-- | The ground truth label provided for the model.
modelQualityJobInput_groundTruthS3Input :: Lens.Lens' ModelQualityJobInput MonitoringGroundTruthS3Input
modelQualityJobInput_groundTruthS3Input = Lens.lens (\ModelQualityJobInput' {groundTruthS3Input} -> groundTruthS3Input) (\s@ModelQualityJobInput' {} a -> s {groundTruthS3Input = a} :: ModelQualityJobInput)

instance Core.FromJSON ModelQualityJobInput where
  parseJSON =
    Core.withObject
      "ModelQualityJobInput"
      ( \x ->
          ModelQualityJobInput'
            Core.<$> (x Core..: "EndpointInput")
            Core.<*> (x Core..: "GroundTruthS3Input")
      )

instance Core.Hashable ModelQualityJobInput

instance Core.NFData ModelQualityJobInput

instance Core.ToJSON ModelQualityJobInput where
  toJSON ModelQualityJobInput' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointInput" Core..= endpointInput),
            Core.Just
              ("GroundTruthS3Input" Core..= groundTruthS3Input)
          ]
      )
