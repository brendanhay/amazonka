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
-- Module      : Network.AWS.SageMaker.Types.ModelBiasJobInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelBiasJobInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.EndpointInput
import Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input

-- | Inputs for the model bias job.
--
-- /See:/ 'newModelBiasJobInput' smart constructor.
data ModelBiasJobInput = ModelBiasJobInput'
  { endpointInput :: EndpointInput,
    -- | Location of ground truth labels to use in model bias job.
    groundTruthS3Input :: MonitoringGroundTruthS3Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelBiasJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointInput', 'modelBiasJobInput_endpointInput' - Undocumented member.
--
-- 'groundTruthS3Input', 'modelBiasJobInput_groundTruthS3Input' - Location of ground truth labels to use in model bias job.
newModelBiasJobInput ::
  -- | 'endpointInput'
  EndpointInput ->
  -- | 'groundTruthS3Input'
  MonitoringGroundTruthS3Input ->
  ModelBiasJobInput
newModelBiasJobInput
  pEndpointInput_
  pGroundTruthS3Input_ =
    ModelBiasJobInput'
      { endpointInput = pEndpointInput_,
        groundTruthS3Input = pGroundTruthS3Input_
      }

-- | Undocumented member.
modelBiasJobInput_endpointInput :: Lens.Lens' ModelBiasJobInput EndpointInput
modelBiasJobInput_endpointInput = Lens.lens (\ModelBiasJobInput' {endpointInput} -> endpointInput) (\s@ModelBiasJobInput' {} a -> s {endpointInput = a} :: ModelBiasJobInput)

-- | Location of ground truth labels to use in model bias job.
modelBiasJobInput_groundTruthS3Input :: Lens.Lens' ModelBiasJobInput MonitoringGroundTruthS3Input
modelBiasJobInput_groundTruthS3Input = Lens.lens (\ModelBiasJobInput' {groundTruthS3Input} -> groundTruthS3Input) (\s@ModelBiasJobInput' {} a -> s {groundTruthS3Input = a} :: ModelBiasJobInput)

instance Prelude.FromJSON ModelBiasJobInput where
  parseJSON =
    Prelude.withObject
      "ModelBiasJobInput"
      ( \x ->
          ModelBiasJobInput'
            Prelude.<$> (x Prelude..: "EndpointInput")
            Prelude.<*> (x Prelude..: "GroundTruthS3Input")
      )

instance Prelude.Hashable ModelBiasJobInput

instance Prelude.NFData ModelBiasJobInput

instance Prelude.ToJSON ModelBiasJobInput where
  toJSON ModelBiasJobInput' {..} =
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
