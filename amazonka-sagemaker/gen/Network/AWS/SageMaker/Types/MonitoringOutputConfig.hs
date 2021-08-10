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
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringOutputConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MonitoringOutput

-- | The output configuration for monitoring jobs.
--
-- /See:/ 'newMonitoringOutputConfig' smart constructor.
data MonitoringOutputConfig = MonitoringOutputConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt the model artifacts at rest using Amazon S3 server-side
    -- encryption.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Monitoring outputs for monitoring jobs. This is where the output of the
    -- periodic monitoring jobs is uploaded.
    monitoringOutputs :: Prelude.NonEmpty MonitoringOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'monitoringOutputConfig_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption.
--
-- 'monitoringOutputs', 'monitoringOutputConfig_monitoringOutputs' - Monitoring outputs for monitoring jobs. This is where the output of the
-- periodic monitoring jobs is uploaded.
newMonitoringOutputConfig ::
  -- | 'monitoringOutputs'
  Prelude.NonEmpty MonitoringOutput ->
  MonitoringOutputConfig
newMonitoringOutputConfig pMonitoringOutputs_ =
  MonitoringOutputConfig'
    { kmsKeyId = Prelude.Nothing,
      monitoringOutputs =
        Lens._Coerce Lens.# pMonitoringOutputs_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption.
monitoringOutputConfig_kmsKeyId :: Lens.Lens' MonitoringOutputConfig (Prelude.Maybe Prelude.Text)
monitoringOutputConfig_kmsKeyId = Lens.lens (\MonitoringOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@MonitoringOutputConfig' {} a -> s {kmsKeyId = a} :: MonitoringOutputConfig)

-- | Monitoring outputs for monitoring jobs. This is where the output of the
-- periodic monitoring jobs is uploaded.
monitoringOutputConfig_monitoringOutputs :: Lens.Lens' MonitoringOutputConfig (Prelude.NonEmpty MonitoringOutput)
monitoringOutputConfig_monitoringOutputs = Lens.lens (\MonitoringOutputConfig' {monitoringOutputs} -> monitoringOutputs) (\s@MonitoringOutputConfig' {} a -> s {monitoringOutputs = a} :: MonitoringOutputConfig) Prelude.. Lens._Coerce

instance Core.FromJSON MonitoringOutputConfig where
  parseJSON =
    Core.withObject
      "MonitoringOutputConfig"
      ( \x ->
          MonitoringOutputConfig'
            Prelude.<$> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..: "MonitoringOutputs")
      )

instance Prelude.Hashable MonitoringOutputConfig

instance Prelude.NFData MonitoringOutputConfig

instance Core.ToJSON MonitoringOutputConfig where
  toJSON MonitoringOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("MonitoringOutputs" Core..= monitoringOutputs)
          ]
      )
