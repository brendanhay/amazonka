-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringOutputConfig
  ( MonitoringOutputConfig (..),

    -- * Smart constructor
    mkMonitoringOutputConfig,

    -- * Lenses
    mocKMSKeyId,
    mocMonitoringOutputs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MonitoringOutput

-- | The output configuration for monitoring jobs.
--
-- /See:/ 'mkMonitoringOutputConfig' smart constructor.
data MonitoringOutputConfig = MonitoringOutputConfig'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    monitoringOutputs ::
      Lude.NonEmpty MonitoringOutput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringOutputConfig' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
-- * 'monitoringOutputs' - Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
mkMonitoringOutputConfig ::
  -- | 'monitoringOutputs'
  Lude.NonEmpty MonitoringOutput ->
  MonitoringOutputConfig
mkMonitoringOutputConfig pMonitoringOutputs_ =
  MonitoringOutputConfig'
    { kmsKeyId = Lude.Nothing,
      monitoringOutputs = pMonitoringOutputs_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mocKMSKeyId :: Lens.Lens' MonitoringOutputConfig (Lude.Maybe Lude.Text)
mocKMSKeyId = Lens.lens (kmsKeyId :: MonitoringOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: MonitoringOutputConfig)
{-# DEPRECATED mocKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
--
-- /Note:/ Consider using 'monitoringOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mocMonitoringOutputs :: Lens.Lens' MonitoringOutputConfig (Lude.NonEmpty MonitoringOutput)
mocMonitoringOutputs = Lens.lens (monitoringOutputs :: MonitoringOutputConfig -> Lude.NonEmpty MonitoringOutput) (\s a -> s {monitoringOutputs = a} :: MonitoringOutputConfig)
{-# DEPRECATED mocMonitoringOutputs "Use generic-lens or generic-optics with 'monitoringOutputs' instead." #-}

instance Lude.FromJSON MonitoringOutputConfig where
  parseJSON =
    Lude.withObject
      "MonitoringOutputConfig"
      ( \x ->
          MonitoringOutputConfig'
            Lude.<$> (x Lude..:? "KmsKeyId") Lude.<*> (x Lude..: "MonitoringOutputs")
      )

instance Lude.ToJSON MonitoringOutputConfig where
  toJSON MonitoringOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            Lude.Just ("MonitoringOutputs" Lude..= monitoringOutputs)
          ]
      )
