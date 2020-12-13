{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisVideoStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisVideoStreamConfig
  ( KinesisVideoStreamConfig (..),

    -- * Smart constructor
    mkKinesisVideoStreamConfig,

    -- * Lenses
    kvscPrefix,
    kvscRetentionPeriodHours,
    kvscEncryptionConfig,
  )
where

import Network.AWS.Connect.Types.EncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information of a Kinesis video stream.
--
-- /See:/ 'mkKinesisVideoStreamConfig' smart constructor.
data KinesisVideoStreamConfig = KinesisVideoStreamConfig'
  { -- | The prefix of the video stream.
    prefix :: Lude.Text,
    -- | The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
    --
    -- The default value is 0, indicating that the stream does not persist data.
    retentionPeriodHours :: Lude.Natural,
    -- | The encryption configuration.
    encryptionConfig :: EncryptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisVideoStreamConfig' with the minimum fields required to make a request.
--
-- * 'prefix' - The prefix of the video stream.
-- * 'retentionPeriodHours' - The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
-- * 'encryptionConfig' - The encryption configuration.
mkKinesisVideoStreamConfig ::
  -- | 'prefix'
  Lude.Text ->
  -- | 'retentionPeriodHours'
  Lude.Natural ->
  -- | 'encryptionConfig'
  EncryptionConfig ->
  KinesisVideoStreamConfig
mkKinesisVideoStreamConfig
  pPrefix_
  pRetentionPeriodHours_
  pEncryptionConfig_ =
    KinesisVideoStreamConfig'
      { prefix = pPrefix_,
        retentionPeriodHours = pRetentionPeriodHours_,
        encryptionConfig = pEncryptionConfig_
      }

-- | The prefix of the video stream.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscPrefix :: Lens.Lens' KinesisVideoStreamConfig Lude.Text
kvscPrefix = Lens.lens (prefix :: KinesisVideoStreamConfig -> Lude.Text) (\s a -> s {prefix = a} :: KinesisVideoStreamConfig)
{-# DEPRECATED kvscPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist data.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscRetentionPeriodHours :: Lens.Lens' KinesisVideoStreamConfig Lude.Natural
kvscRetentionPeriodHours = Lens.lens (retentionPeriodHours :: KinesisVideoStreamConfig -> Lude.Natural) (\s a -> s {retentionPeriodHours = a} :: KinesisVideoStreamConfig)
{-# DEPRECATED kvscRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

-- | The encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvscEncryptionConfig :: Lens.Lens' KinesisVideoStreamConfig EncryptionConfig
kvscEncryptionConfig = Lens.lens (encryptionConfig :: KinesisVideoStreamConfig -> EncryptionConfig) (\s a -> s {encryptionConfig = a} :: KinesisVideoStreamConfig)
{-# DEPRECATED kvscEncryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead." #-}

instance Lude.FromJSON KinesisVideoStreamConfig where
  parseJSON =
    Lude.withObject
      "KinesisVideoStreamConfig"
      ( \x ->
          KinesisVideoStreamConfig'
            Lude.<$> (x Lude..: "Prefix")
            Lude.<*> (x Lude..: "RetentionPeriodHours")
            Lude.<*> (x Lude..: "EncryptionConfig")
      )

instance Lude.ToJSON KinesisVideoStreamConfig where
  toJSON KinesisVideoStreamConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Prefix" Lude..= prefix),
            Lude.Just ("RetentionPeriodHours" Lude..= retentionPeriodHours),
            Lude.Just ("EncryptionConfig" Lude..= encryptionConfig)
          ]
      )
