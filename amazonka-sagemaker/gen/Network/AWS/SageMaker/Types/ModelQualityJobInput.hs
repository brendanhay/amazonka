{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ModelQualityJobInput where
  parseJSON =
    Prelude.withObject
      "ModelQualityJobInput"
      ( \x ->
          ModelQualityJobInput'
            Prelude.<$> (x Prelude..: "EndpointInput")
            Prelude.<*> (x Prelude..: "GroundTruthS3Input")
      )

instance Prelude.Hashable ModelQualityJobInput

instance Prelude.NFData ModelQualityJobInput

instance Prelude.ToJSON ModelQualityJobInput where
  toJSON ModelQualityJobInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointInput" Prelude..= endpointInput),
            Prelude.Just
              ( "GroundTruthS3Input"
                  Prelude..= groundTruthS3Input
              )
          ]
      )
