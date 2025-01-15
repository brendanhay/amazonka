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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ExternalModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelEndpointStatus
import Amazonka.FraudDetector.Types.ModelInputConfiguration
import Amazonka.FraudDetector.Types.ModelOutputConfiguration
import Amazonka.FraudDetector.Types.ModelSource
import qualified Amazonka.Prelude as Prelude

-- | The Amazon SageMaker model.
--
-- /See:/ 'newExternalModel' smart constructor.
data ExternalModel = ExternalModel'
  { -- | The model ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the model was last created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The input configuration.
    inputConfiguration :: Prelude.Maybe ModelInputConfiguration,
    -- | The role used to invoke the model.
    invokeModelEndpointRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the model was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon SageMaker model endpoints.
    modelEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Fraud Detector status for the external model endpoint
    modelEndpointStatus :: Prelude.Maybe ModelEndpointStatus,
    -- | The source of the model.
    modelSource :: Prelude.Maybe ModelSource,
    -- | The output configuration.
    outputConfiguration :: Prelude.Maybe ModelOutputConfiguration
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
-- 'arn', 'externalModel_arn' - The model ARN.
--
-- 'createdTime', 'externalModel_createdTime' - Timestamp of when the model was last created.
--
-- 'inputConfiguration', 'externalModel_inputConfiguration' - The input configuration.
--
-- 'invokeModelEndpointRoleArn', 'externalModel_invokeModelEndpointRoleArn' - The role used to invoke the model.
--
-- 'lastUpdatedTime', 'externalModel_lastUpdatedTime' - Timestamp of when the model was last updated.
--
-- 'modelEndpoint', 'externalModel_modelEndpoint' - The Amazon SageMaker model endpoints.
--
-- 'modelEndpointStatus', 'externalModel_modelEndpointStatus' - The Amazon Fraud Detector status for the external model endpoint
--
-- 'modelSource', 'externalModel_modelSource' - The source of the model.
--
-- 'outputConfiguration', 'externalModel_outputConfiguration' - The output configuration.
newExternalModel ::
  ExternalModel
newExternalModel =
  ExternalModel'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      inputConfiguration = Prelude.Nothing,
      invokeModelEndpointRoleArn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      modelEndpoint = Prelude.Nothing,
      modelEndpointStatus = Prelude.Nothing,
      modelSource = Prelude.Nothing,
      outputConfiguration = Prelude.Nothing
    }

-- | The model ARN.
externalModel_arn :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_arn = Lens.lens (\ExternalModel' {arn} -> arn) (\s@ExternalModel' {} a -> s {arn = a} :: ExternalModel)

-- | Timestamp of when the model was last created.
externalModel_createdTime :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_createdTime = Lens.lens (\ExternalModel' {createdTime} -> createdTime) (\s@ExternalModel' {} a -> s {createdTime = a} :: ExternalModel)

-- | The input configuration.
externalModel_inputConfiguration :: Lens.Lens' ExternalModel (Prelude.Maybe ModelInputConfiguration)
externalModel_inputConfiguration = Lens.lens (\ExternalModel' {inputConfiguration} -> inputConfiguration) (\s@ExternalModel' {} a -> s {inputConfiguration = a} :: ExternalModel)

-- | The role used to invoke the model.
externalModel_invokeModelEndpointRoleArn :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_invokeModelEndpointRoleArn = Lens.lens (\ExternalModel' {invokeModelEndpointRoleArn} -> invokeModelEndpointRoleArn) (\s@ExternalModel' {} a -> s {invokeModelEndpointRoleArn = a} :: ExternalModel)

-- | Timestamp of when the model was last updated.
externalModel_lastUpdatedTime :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_lastUpdatedTime = Lens.lens (\ExternalModel' {lastUpdatedTime} -> lastUpdatedTime) (\s@ExternalModel' {} a -> s {lastUpdatedTime = a} :: ExternalModel)

-- | The Amazon SageMaker model endpoints.
externalModel_modelEndpoint :: Lens.Lens' ExternalModel (Prelude.Maybe Prelude.Text)
externalModel_modelEndpoint = Lens.lens (\ExternalModel' {modelEndpoint} -> modelEndpoint) (\s@ExternalModel' {} a -> s {modelEndpoint = a} :: ExternalModel)

-- | The Amazon Fraud Detector status for the external model endpoint
externalModel_modelEndpointStatus :: Lens.Lens' ExternalModel (Prelude.Maybe ModelEndpointStatus)
externalModel_modelEndpointStatus = Lens.lens (\ExternalModel' {modelEndpointStatus} -> modelEndpointStatus) (\s@ExternalModel' {} a -> s {modelEndpointStatus = a} :: ExternalModel)

-- | The source of the model.
externalModel_modelSource :: Lens.Lens' ExternalModel (Prelude.Maybe ModelSource)
externalModel_modelSource = Lens.lens (\ExternalModel' {modelSource} -> modelSource) (\s@ExternalModel' {} a -> s {modelSource = a} :: ExternalModel)

-- | The output configuration.
externalModel_outputConfiguration :: Lens.Lens' ExternalModel (Prelude.Maybe ModelOutputConfiguration)
externalModel_outputConfiguration = Lens.lens (\ExternalModel' {outputConfiguration} -> outputConfiguration) (\s@ExternalModel' {} a -> s {outputConfiguration = a} :: ExternalModel)

instance Data.FromJSON ExternalModel where
  parseJSON =
    Data.withObject
      "ExternalModel"
      ( \x ->
          ExternalModel'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "inputConfiguration")
            Prelude.<*> (x Data..:? "invokeModelEndpointRoleArn")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "modelEndpoint")
            Prelude.<*> (x Data..:? "modelEndpointStatus")
            Prelude.<*> (x Data..:? "modelSource")
            Prelude.<*> (x Data..:? "outputConfiguration")
      )

instance Prelude.Hashable ExternalModel where
  hashWithSalt _salt ExternalModel' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` inputConfiguration
      `Prelude.hashWithSalt` invokeModelEndpointRoleArn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` modelEndpoint
      `Prelude.hashWithSalt` modelEndpointStatus
      `Prelude.hashWithSalt` modelSource
      `Prelude.hashWithSalt` outputConfiguration

instance Prelude.NFData ExternalModel where
  rnf ExternalModel' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf inputConfiguration `Prelude.seq`
          Prelude.rnf invokeModelEndpointRoleArn `Prelude.seq`
            Prelude.rnf lastUpdatedTime `Prelude.seq`
              Prelude.rnf modelEndpoint `Prelude.seq`
                Prelude.rnf modelEndpointStatus `Prelude.seq`
                  Prelude.rnf modelSource `Prelude.seq`
                    Prelude.rnf outputConfiguration
