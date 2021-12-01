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
-- Module      : Amazonka.FraudDetector.Types.ExternalModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ExternalModel where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.ModelEndpointStatus
import Amazonka.FraudDetector.Types.ModelInputConfiguration
import Amazonka.FraudDetector.Types.ModelOutputConfiguration
import Amazonka.FraudDetector.Types.ModelSource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon SageMaker model.
--
-- /See:/ 'newExternalModel' smart constructor.
data ExternalModel = ExternalModel'
  { -- | The Amazon SageMaker model endpoints.
    modelEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The source of the model.
    modelSource :: Prelude.Maybe ModelSource,
    -- | Timestamp of when the model was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The model ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the model was last created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Fraud Detector status for the external model endpoint
    modelEndpointStatus :: Prelude.Maybe ModelEndpointStatus,
    -- | The output configuration.
    outputConfiguration :: Prelude.Maybe ModelOutputConfiguration,
    -- | The role used to invoke the model.
    invokeModelEndpointRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The input configuration.
    inputConfiguration :: Prelude.Maybe ModelInputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelEndpoint', 'externalModel_modelEndpoint' - The Amazon SageMaker model endpoints.
--
-- 'modelSource', 'externalModel_modelSource' - The source of the model.
--
-- 'lastUpdatedTime', 'externalModel_lastUpdatedTime' - Timestamp of when the model was last updated.
--
-- 'arn', 'externalModel_arn' - The model ARN.
--
-- 'createdTime', 'externalModel_createdTime' - Timestamp of when the model was last created.
--
-- 'modelEndpointStatus', 'externalModel_modelEndpointStatus' - The Amazon Fraud Detector status for the external model endpoint
--
-- 'outputConfiguration', 'externalModel_outputConfiguration' - The output configuration.
--
-- 'invokeModelEndpointRoleArn', 'externalModel_invokeModelEndpointRoleArn' - The role used to invoke the model.
--
-- 'inputConfiguration', 'externalModel_inputConfiguration' - The input configuration.
newExternalModel ::
  ExternalModel
newExternalModel =
  ExternalModel'
    { modelEndpoint = Prelude.Nothing,
      modelSource = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      modelEndpointStatus = Prelude.Nothing,
      outputConfiguration = Prelude.Nothing,
      invokeModelEndpointRoleArn = Prelude.Nothing,
      inputConfiguration = Prelude.Nothing
    }

-- | The Amazon SageMaker model endpoints.
externalModel_modelEndpoint :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_modelEndpoint = Lens.lens (\ExternalModel' {modelEndpoint} -> modelEndpoint) (\s@ExternalModel' {} a -> s {modelEndpoint = a} :: ExternalModel)

-- | The source of the model.
externalModel_modelSource :: Lens.Lens' ExternalModel (Prelude.Maybe ModelSource)
externalModel_modelSource = Lens.lens (\ExternalModel' {modelSource} -> modelSource) (\s@ExternalModel' {} a -> s {modelSource = a} :: ExternalModel)

-- | Timestamp of when the model was last updated.
externalModel_lastUpdatedTime :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_lastUpdatedTime = Lens.lens (\ExternalModel' {lastUpdatedTime} -> lastUpdatedTime) (\s@ExternalModel' {} a -> s {lastUpdatedTime = a} :: ExternalModel)

-- | The model ARN.
externalModel_arn :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_arn = Lens.lens (\ExternalModel' {arn} -> arn) (\s@ExternalModel' {} a -> s {arn = a} :: ExternalModel)

-- | Timestamp of when the model was last created.
externalModel_createdTime :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_createdTime = Lens.lens (\ExternalModel' {createdTime} -> createdTime) (\s@ExternalModel' {} a -> s {createdTime = a} :: ExternalModel)

-- | The Amazon Fraud Detector status for the external model endpoint
externalModel_modelEndpointStatus :: Lens.Lens' ExternalModel (Prelude.Maybe ModelEndpointStatus)
externalModel_modelEndpointStatus = Lens.lens (\ExternalModel' {modelEndpointStatus} -> modelEndpointStatus) (\s@ExternalModel' {} a -> s {modelEndpointStatus = a} :: ExternalModel)

-- | The output configuration.
externalModel_outputConfiguration :: Lens.Lens' ExternalModel (Prelude.Maybe ModelOutputConfiguration)
externalModel_outputConfiguration = Lens.lens (\ExternalModel' {outputConfiguration} -> outputConfiguration) (\s@ExternalModel' {} a -> s {outputConfiguration = a} :: ExternalModel)

-- | The role used to invoke the model.
externalModel_invokeModelEndpointRoleArn :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_invokeModelEndpointRoleArn = Lens.lens (\ExternalModel' {invokeModelEndpointRoleArn} -> invokeModelEndpointRoleArn) (\s@ExternalModel' {} a -> s {invokeModelEndpointRoleArn = a} :: ExternalModel)

-- | The input configuration.
externalModel_inputConfiguration :: Lens.Lens' ExternalModel (Prelude.Maybe ModelInputConfiguration)
externalModel_inputConfiguration = Lens.lens (\ExternalModel' {inputConfiguration} -> inputConfiguration) (\s@ExternalModel' {} a -> s {inputConfiguration = a} :: ExternalModel)

instance Core.FromJSON ExternalModel where
  parseJSON =
    Core.withObject
      "ExternalModel"
      ( \x ->
          ExternalModel'
            Prelude.<$> (x Core..:? "modelEndpoint")
            Prelude.<*> (x Core..:? "modelSource")
            Prelude.<*> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "modelEndpointStatus")
            Prelude.<*> (x Core..:? "outputConfiguration")
            Prelude.<*> (x Core..:? "invokeModelEndpointRoleArn")
            Prelude.<*> (x Core..:? "inputConfiguration")
      )

instance Prelude.Hashable ExternalModel where
  hashWithSalt salt' ExternalModel' {..} =
    salt' `Prelude.hashWithSalt` inputConfiguration
      `Prelude.hashWithSalt` invokeModelEndpointRoleArn
      `Prelude.hashWithSalt` outputConfiguration
      `Prelude.hashWithSalt` modelEndpointStatus
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` modelSource
      `Prelude.hashWithSalt` modelEndpoint

instance Prelude.NFData ExternalModel where
  rnf ExternalModel' {..} =
    Prelude.rnf modelEndpoint
      `Prelude.seq` Prelude.rnf inputConfiguration
      `Prelude.seq` Prelude.rnf invokeModelEndpointRoleArn
      `Prelude.seq` Prelude.rnf outputConfiguration
      `Prelude.seq` Prelude.rnf modelEndpointStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf modelSource
