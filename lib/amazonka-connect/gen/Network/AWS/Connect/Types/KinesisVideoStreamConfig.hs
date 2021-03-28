{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisVideoStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.KinesisVideoStreamConfig
  ( KinesisVideoStreamConfig (..)
  -- * Smart constructor
  , mkKinesisVideoStreamConfig
  -- * Lenses
  , kvscPrefix
  , kvscRetentionPeriodHours
  , kvscEncryptionConfig
  ) where

import qualified Network.AWS.Connect.Types.EncryptionConfig as Types
import qualified Network.AWS.Connect.Types.Prefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information of a Kinesis video stream.
--
-- /See:/ 'mkKinesisVideoStreamConfig' smart constructor.
data KinesisVideoStreamConfig = KinesisVideoStreamConfig'
  { prefix :: Types.Prefix
    -- ^ The prefix of the video stream.
  , retentionPeriodHours :: Core.Natural
    -- ^ The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
  , encryptionConfig :: Types.EncryptionConfig
    -- ^ The encryption configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisVideoStreamConfig' value with any optional fields omitted.
mkKinesisVideoStreamConfig
    :: Types.Prefix -- ^ 'prefix'
    -> Core.Natural -- ^ 'retentionPeriodHours'
    -> Types.EncryptionConfig -- ^ 'encryptionConfig'
    -> KinesisVideoStreamConfig
mkKinesisVideoStreamConfig prefix retentionPeriodHours
  encryptionConfig
  = KinesisVideoStreamConfig'{prefix, retentionPeriodHours,
                              encryptionConfig}

-- | The prefix of the video stream.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscPrefix :: Lens.Lens' KinesisVideoStreamConfig Types.Prefix
kvscPrefix = Lens.field @"prefix"
{-# INLINEABLE kvscPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscRetentionPeriodHours :: Lens.Lens' KinesisVideoStreamConfig Core.Natural
kvscRetentionPeriodHours = Lens.field @"retentionPeriodHours"
{-# INLINEABLE kvscRetentionPeriodHours #-}
{-# DEPRECATED retentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead"  #-}

-- | The encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscEncryptionConfig :: Lens.Lens' KinesisVideoStreamConfig Types.EncryptionConfig
kvscEncryptionConfig = Lens.field @"encryptionConfig"
{-# INLINEABLE kvscEncryptionConfig #-}
{-# DEPRECATED encryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead"  #-}

instance Core.FromJSON KinesisVideoStreamConfig where
        toJSON KinesisVideoStreamConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Prefix" Core..= prefix),
                  Core.Just ("RetentionPeriodHours" Core..= retentionPeriodHours),
                  Core.Just ("EncryptionConfig" Core..= encryptionConfig)])

instance Core.FromJSON KinesisVideoStreamConfig where
        parseJSON
          = Core.withObject "KinesisVideoStreamConfig" Core.$
              \ x ->
                KinesisVideoStreamConfig' Core.<$>
                  (x Core..: "Prefix") Core.<*> x Core..: "RetentionPeriodHours"
                    Core.<*> x Core..: "EncryptionConfig"
