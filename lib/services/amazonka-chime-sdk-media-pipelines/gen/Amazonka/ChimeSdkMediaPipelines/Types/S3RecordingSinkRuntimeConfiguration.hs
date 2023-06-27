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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkRuntimeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.S3RecordingSinkRuntimeConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.RecordingFileFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that holds the settings for transmitting media files to the
-- Amazon S3 bucket. If specified, the settings in this structure override
-- any settings in @S3RecordingSinkConfiguration@.
--
-- /See:/ 'newS3RecordingSinkRuntimeConfiguration' smart constructor.
data S3RecordingSinkRuntimeConfiguration = S3RecordingSinkRuntimeConfiguration'
  { -- | The URI of the S3 bucket used as the sink.
    destination :: Data.Sensitive Prelude.Text,
    -- | The file format for the media files sent to the Amazon S3 bucket.
    recordingFileFormat :: RecordingFileFormat
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3RecordingSinkRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 's3RecordingSinkRuntimeConfiguration_destination' - The URI of the S3 bucket used as the sink.
--
-- 'recordingFileFormat', 's3RecordingSinkRuntimeConfiguration_recordingFileFormat' - The file format for the media files sent to the Amazon S3 bucket.
newS3RecordingSinkRuntimeConfiguration ::
  -- | 'destination'
  Prelude.Text ->
  -- | 'recordingFileFormat'
  RecordingFileFormat ->
  S3RecordingSinkRuntimeConfiguration
newS3RecordingSinkRuntimeConfiguration
  pDestination_
  pRecordingFileFormat_ =
    S3RecordingSinkRuntimeConfiguration'
      { destination =
          Data._Sensitive Lens.# pDestination_,
        recordingFileFormat =
          pRecordingFileFormat_
      }

-- | The URI of the S3 bucket used as the sink.
s3RecordingSinkRuntimeConfiguration_destination :: Lens.Lens' S3RecordingSinkRuntimeConfiguration Prelude.Text
s3RecordingSinkRuntimeConfiguration_destination = Lens.lens (\S3RecordingSinkRuntimeConfiguration' {destination} -> destination) (\s@S3RecordingSinkRuntimeConfiguration' {} a -> s {destination = a} :: S3RecordingSinkRuntimeConfiguration) Prelude.. Data._Sensitive

-- | The file format for the media files sent to the Amazon S3 bucket.
s3RecordingSinkRuntimeConfiguration_recordingFileFormat :: Lens.Lens' S3RecordingSinkRuntimeConfiguration RecordingFileFormat
s3RecordingSinkRuntimeConfiguration_recordingFileFormat = Lens.lens (\S3RecordingSinkRuntimeConfiguration' {recordingFileFormat} -> recordingFileFormat) (\s@S3RecordingSinkRuntimeConfiguration' {} a -> s {recordingFileFormat = a} :: S3RecordingSinkRuntimeConfiguration)

instance
  Data.FromJSON
    S3RecordingSinkRuntimeConfiguration
  where
  parseJSON =
    Data.withObject
      "S3RecordingSinkRuntimeConfiguration"
      ( \x ->
          S3RecordingSinkRuntimeConfiguration'
            Prelude.<$> (x Data..: "Destination")
            Prelude.<*> (x Data..: "RecordingFileFormat")
      )

instance
  Prelude.Hashable
    S3RecordingSinkRuntimeConfiguration
  where
  hashWithSalt
    _salt
    S3RecordingSinkRuntimeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` recordingFileFormat

instance
  Prelude.NFData
    S3RecordingSinkRuntimeConfiguration
  where
  rnf S3RecordingSinkRuntimeConfiguration' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf recordingFileFormat

instance
  Data.ToJSON
    S3RecordingSinkRuntimeConfiguration
  where
  toJSON S3RecordingSinkRuntimeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Destination" Data..= destination),
            Prelude.Just
              ("RecordingFileFormat" Data..= recordingFileFormat)
          ]
      )
