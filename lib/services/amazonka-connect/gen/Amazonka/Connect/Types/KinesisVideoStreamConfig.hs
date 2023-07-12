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
-- Module      : Amazonka.Connect.Types.KinesisVideoStreamConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.KinesisVideoStreamConfig where

import Amazonka.Connect.Types.EncryptionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON KinesisVideoStreamConfig where
  parseJSON =
    Data.withObject
      "KinesisVideoStreamConfig"
      ( \x ->
          KinesisVideoStreamConfig'
            Prelude.<$> (x Data..: "Prefix")
            Prelude.<*> (x Data..: "RetentionPeriodHours")
            Prelude.<*> (x Data..: "EncryptionConfig")
      )

instance Prelude.Hashable KinesisVideoStreamConfig where
  hashWithSalt _salt KinesisVideoStreamConfig' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` retentionPeriodHours
      `Prelude.hashWithSalt` encryptionConfig

instance Prelude.NFData KinesisVideoStreamConfig where
  rnf KinesisVideoStreamConfig' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf retentionPeriodHours
      `Prelude.seq` Prelude.rnf encryptionConfig

instance Data.ToJSON KinesisVideoStreamConfig where
  toJSON KinesisVideoStreamConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Prefix" Data..= prefix),
            Prelude.Just
              ( "RetentionPeriodHours"
                  Data..= retentionPeriodHours
              ),
            Prelude.Just
              ("EncryptionConfig" Data..= encryptionConfig)
          ]
      )
