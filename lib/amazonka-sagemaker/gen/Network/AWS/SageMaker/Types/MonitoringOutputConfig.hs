{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringOutputConfig
  ( MonitoringOutputConfig (..)
  -- * Smart constructor
  , mkMonitoringOutputConfig
  -- * Lenses
  , mocMonitoringOutputs
  , mocKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.MonitoringOutput as Types

-- | The output configuration for monitoring jobs.
--
-- /See:/ 'mkMonitoringOutputConfig' smart constructor.
data MonitoringOutputConfig = MonitoringOutputConfig'
  { monitoringOutputs :: Core.NonEmpty Types.MonitoringOutput
    -- ^ Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringOutputConfig' value with any optional fields omitted.
mkMonitoringOutputConfig
    :: Core.NonEmpty Types.MonitoringOutput -- ^ 'monitoringOutputs'
    -> MonitoringOutputConfig
mkMonitoringOutputConfig monitoringOutputs
  = MonitoringOutputConfig'{monitoringOutputs,
                            kmsKeyId = Core.Nothing}

-- | Monitoring outputs for monitoring jobs. This is where the output of the periodic monitoring jobs is uploaded.
--
-- /Note:/ Consider using 'monitoringOutputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mocMonitoringOutputs :: Lens.Lens' MonitoringOutputConfig (Core.NonEmpty Types.MonitoringOutput)
mocMonitoringOutputs = Lens.field @"monitoringOutputs"
{-# INLINEABLE mocMonitoringOutputs #-}
{-# DEPRECATED monitoringOutputs "Use generic-lens or generic-optics with 'monitoringOutputs' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mocKmsKeyId :: Lens.Lens' MonitoringOutputConfig (Core.Maybe Types.KmsKeyId)
mocKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE mocKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.FromJSON MonitoringOutputConfig where
        toJSON MonitoringOutputConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MonitoringOutputs" Core..= monitoringOutputs),
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId])

instance Core.FromJSON MonitoringOutputConfig where
        parseJSON
          = Core.withObject "MonitoringOutputConfig" Core.$
              \ x ->
                MonitoringOutputConfig' Core.<$>
                  (x Core..: "MonitoringOutputs") Core.<*> x Core..:? "KmsKeyId"
