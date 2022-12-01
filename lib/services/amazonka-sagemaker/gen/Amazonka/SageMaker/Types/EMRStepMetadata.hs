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
-- Module      : Amazonka.SageMaker.Types.EMRStepMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EMRStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configurations and outcomes of an Amazon EMR step execution.
--
-- /See:/ 'newEMRStepMetadata' smart constructor.
data EMRStepMetadata = EMRStepMetadata'
  { -- | The path to the log file where the cluster step\'s failure root cause is
    -- recorded.
    logFilePath :: Prelude.Maybe Prelude.Text,
    -- | The name of the EMR cluster step.
    stepName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the EMR cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the EMR cluster step.
    stepId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EMRStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logFilePath', 'eMRStepMetadata_logFilePath' - The path to the log file where the cluster step\'s failure root cause is
-- recorded.
--
-- 'stepName', 'eMRStepMetadata_stepName' - The name of the EMR cluster step.
--
-- 'clusterId', 'eMRStepMetadata_clusterId' - The identifier of the EMR cluster.
--
-- 'stepId', 'eMRStepMetadata_stepId' - The identifier of the EMR cluster step.
newEMRStepMetadata ::
  EMRStepMetadata
newEMRStepMetadata =
  EMRStepMetadata'
    { logFilePath = Prelude.Nothing,
      stepName = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      stepId = Prelude.Nothing
    }

-- | The path to the log file where the cluster step\'s failure root cause is
-- recorded.
eMRStepMetadata_logFilePath :: Lens.Lens' EMRStepMetadata (Prelude.Maybe Prelude.Text)
eMRStepMetadata_logFilePath = Lens.lens (\EMRStepMetadata' {logFilePath} -> logFilePath) (\s@EMRStepMetadata' {} a -> s {logFilePath = a} :: EMRStepMetadata)

-- | The name of the EMR cluster step.
eMRStepMetadata_stepName :: Lens.Lens' EMRStepMetadata (Prelude.Maybe Prelude.Text)
eMRStepMetadata_stepName = Lens.lens (\EMRStepMetadata' {stepName} -> stepName) (\s@EMRStepMetadata' {} a -> s {stepName = a} :: EMRStepMetadata)

-- | The identifier of the EMR cluster.
eMRStepMetadata_clusterId :: Lens.Lens' EMRStepMetadata (Prelude.Maybe Prelude.Text)
eMRStepMetadata_clusterId = Lens.lens (\EMRStepMetadata' {clusterId} -> clusterId) (\s@EMRStepMetadata' {} a -> s {clusterId = a} :: EMRStepMetadata)

-- | The identifier of the EMR cluster step.
eMRStepMetadata_stepId :: Lens.Lens' EMRStepMetadata (Prelude.Maybe Prelude.Text)
eMRStepMetadata_stepId = Lens.lens (\EMRStepMetadata' {stepId} -> stepId) (\s@EMRStepMetadata' {} a -> s {stepId = a} :: EMRStepMetadata)

instance Core.FromJSON EMRStepMetadata where
  parseJSON =
    Core.withObject
      "EMRStepMetadata"
      ( \x ->
          EMRStepMetadata'
            Prelude.<$> (x Core..:? "LogFilePath")
            Prelude.<*> (x Core..:? "StepName")
            Prelude.<*> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "StepId")
      )

instance Prelude.Hashable EMRStepMetadata where
  hashWithSalt _salt EMRStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` logFilePath
      `Prelude.hashWithSalt` stepName
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` stepId

instance Prelude.NFData EMRStepMetadata where
  rnf EMRStepMetadata' {..} =
    Prelude.rnf logFilePath
      `Prelude.seq` Prelude.rnf stepName
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf stepId
