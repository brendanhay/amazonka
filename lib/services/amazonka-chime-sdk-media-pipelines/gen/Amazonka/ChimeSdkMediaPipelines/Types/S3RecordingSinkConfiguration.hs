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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.RecordingFileFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure that holds the settings for transmitting media to the
-- Amazon S3 bucket. These values are used as defaults if
-- @S3RecordingSinkRuntimeConfiguration@ is not specified.
--
-- /See:/ 'newS3RecordingSinkConfiguration' smart constructor.
data S3RecordingSinkConfiguration = S3RecordingSinkConfiguration'
  { -- | The default URI of the Amazon S3 bucket used as the recording sink.
    destination :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The default file format for the media files sent to the Amazon S3
    -- bucket.
    recordingFileFormat :: Prelude.Maybe RecordingFileFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3RecordingSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 's3RecordingSinkConfiguration_destination' - The default URI of the Amazon S3 bucket used as the recording sink.
--
-- 'recordingFileFormat', 's3RecordingSinkConfiguration_recordingFileFormat' - The default file format for the media files sent to the Amazon S3
-- bucket.
newS3RecordingSinkConfiguration ::
  S3RecordingSinkConfiguration
newS3RecordingSinkConfiguration =
  S3RecordingSinkConfiguration'
    { destination =
        Prelude.Nothing,
      recordingFileFormat = Prelude.Nothing
    }

-- | The default URI of the Amazon S3 bucket used as the recording sink.
s3RecordingSinkConfiguration_destination :: Lens.Lens' S3RecordingSinkConfiguration (Prelude.Maybe Prelude.Text)
s3RecordingSinkConfiguration_destination = Lens.lens (\S3RecordingSinkConfiguration' {destination} -> destination) (\s@S3RecordingSinkConfiguration' {} a -> s {destination = a} :: S3RecordingSinkConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The default file format for the media files sent to the Amazon S3
-- bucket.
s3RecordingSinkConfiguration_recordingFileFormat :: Lens.Lens' S3RecordingSinkConfiguration (Prelude.Maybe RecordingFileFormat)
s3RecordingSinkConfiguration_recordingFileFormat = Lens.lens (\S3RecordingSinkConfiguration' {recordingFileFormat} -> recordingFileFormat) (\s@S3RecordingSinkConfiguration' {} a -> s {recordingFileFormat = a} :: S3RecordingSinkConfiguration)

instance Data.FromJSON S3RecordingSinkConfiguration where
  parseJSON =
    Data.withObject
      "S3RecordingSinkConfiguration"
      ( \x ->
          S3RecordingSinkConfiguration'
            Prelude.<$> (x Data..:? "Destination")
            Prelude.<*> (x Data..:? "RecordingFileFormat")
      )

instance
  Prelude.Hashable
    S3RecordingSinkConfiguration
  where
  hashWithSalt _salt S3RecordingSinkConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` recordingFileFormat

instance Prelude.NFData S3RecordingSinkConfiguration where
  rnf S3RecordingSinkConfiguration' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf recordingFileFormat

instance Data.ToJSON S3RecordingSinkConfiguration where
  toJSON S3RecordingSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destination" Data..=) Prelude.<$> destination,
            ("RecordingFileFormat" Data..=)
              Prelude.<$> recordingFileFormat
          ]
      )
