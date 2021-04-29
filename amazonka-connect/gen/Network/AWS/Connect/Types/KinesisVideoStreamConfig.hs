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
-- Module      : Network.AWS.Connect.Types.KinesisVideoStreamConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisVideoStreamConfig where

import Network.AWS.Connect.Types.EncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information of a Kinesis video stream.
--
-- /See:/ 'newKinesisVideoStreamConfig' smart constructor.
data KinesisVideoStreamConfig = KinesisVideoStreamConfig'
  { -- | The prefix of the video stream.
    prefix :: Prelude.Text,
    -- | The number of hours data is retained in the stream. Kinesis Video
    -- Streams retains the data in a data store that is associated with the
    -- stream.
    --
    -- The default value is 0, indicating that the stream does not persist
    -- data.
    retentionPeriodHours :: Prelude.Natural,
    -- | The encryption configuration.
    encryptionConfig :: EncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisVideoStreamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'kinesisVideoStreamConfig_prefix' - The prefix of the video stream.
--
-- 'retentionPeriodHours', 'kinesisVideoStreamConfig_retentionPeriodHours' - The number of hours data is retained in the stream. Kinesis Video
-- Streams retains the data in a data store that is associated with the
-- stream.
--
-- The default value is 0, indicating that the stream does not persist
-- data.
--
-- 'encryptionConfig', 'kinesisVideoStreamConfig_encryptionConfig' - The encryption configuration.
newKinesisVideoStreamConfig ::
  -- | 'prefix'
  Prelude.Text ->
  -- | 'retentionPeriodHours'
  Prelude.Natural ->
  -- | 'encryptionConfig'
  EncryptionConfig ->
  KinesisVideoStreamConfig
newKinesisVideoStreamConfig
  pPrefix_
  pRetentionPeriodHours_
  pEncryptionConfig_ =
    KinesisVideoStreamConfig'
      { prefix = pPrefix_,
        retentionPeriodHours = pRetentionPeriodHours_,
        encryptionConfig = pEncryptionConfig_
      }

-- | The prefix of the video stream.
kinesisVideoStreamConfig_prefix :: Lens.Lens' KinesisVideoStreamConfig Prelude.Text
kinesisVideoStreamConfig_prefix = Lens.lens (\KinesisVideoStreamConfig' {prefix} -> prefix) (\s@KinesisVideoStreamConfig' {} a -> s {prefix = a} :: KinesisVideoStreamConfig)

-- | The number of hours data is retained in the stream. Kinesis Video
-- Streams retains the data in a data store that is associated with the
-- stream.
--
-- The default value is 0, indicating that the stream does not persist
-- data.
kinesisVideoStreamConfig_retentionPeriodHours :: Lens.Lens' KinesisVideoStreamConfig Prelude.Natural
kinesisVideoStreamConfig_retentionPeriodHours = Lens.lens (\KinesisVideoStreamConfig' {retentionPeriodHours} -> retentionPeriodHours) (\s@KinesisVideoStreamConfig' {} a -> s {retentionPeriodHours = a} :: KinesisVideoStreamConfig)

-- | The encryption configuration.
kinesisVideoStreamConfig_encryptionConfig :: Lens.Lens' KinesisVideoStreamConfig EncryptionConfig
kinesisVideoStreamConfig_encryptionConfig = Lens.lens (\KinesisVideoStreamConfig' {encryptionConfig} -> encryptionConfig) (\s@KinesisVideoStreamConfig' {} a -> s {encryptionConfig = a} :: KinesisVideoStreamConfig)

instance Prelude.FromJSON KinesisVideoStreamConfig where
  parseJSON =
    Prelude.withObject
      "KinesisVideoStreamConfig"
      ( \x ->
          KinesisVideoStreamConfig'
            Prelude.<$> (x Prelude..: "Prefix")
            Prelude.<*> (x Prelude..: "RetentionPeriodHours")
            Prelude.<*> (x Prelude..: "EncryptionConfig")
      )

instance Prelude.Hashable KinesisVideoStreamConfig

instance Prelude.NFData KinesisVideoStreamConfig

instance Prelude.ToJSON KinesisVideoStreamConfig where
  toJSON KinesisVideoStreamConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Prefix" Prelude..= prefix),
            Prelude.Just
              ( "RetentionPeriodHours"
                  Prelude..= retentionPeriodHours
              ),
            Prelude.Just
              ("EncryptionConfig" Prelude..= encryptionConfig)
          ]
      )
