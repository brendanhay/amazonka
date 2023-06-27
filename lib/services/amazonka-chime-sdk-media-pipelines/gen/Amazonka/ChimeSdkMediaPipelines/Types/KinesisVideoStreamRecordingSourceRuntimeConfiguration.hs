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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamRecordingSourceRuntimeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.KinesisVideoStreamRecordingSourceRuntimeConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.FragmentSelector
import Amazonka.ChimeSdkMediaPipelines.Types.RecordingStreamConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the runtime settings for recording a Kinesis
-- video stream.
--
-- /See:/ 'newKinesisVideoStreamRecordingSourceRuntimeConfiguration' smart constructor.
data KinesisVideoStreamRecordingSourceRuntimeConfiguration = KinesisVideoStreamRecordingSourceRuntimeConfiguration'
  { -- | The stream or streams to be recorded.
    streams :: Prelude.NonEmpty RecordingStreamConfiguration,
    -- | Describes the timestamp range and timestamp origin of a range of
    -- fragments in the Kinesis video stream.
    fragmentSelector :: FragmentSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisVideoStreamRecordingSourceRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streams', 'kinesisVideoStreamRecordingSourceRuntimeConfiguration_streams' - The stream or streams to be recorded.
--
-- 'fragmentSelector', 'kinesisVideoStreamRecordingSourceRuntimeConfiguration_fragmentSelector' - Describes the timestamp range and timestamp origin of a range of
-- fragments in the Kinesis video stream.
newKinesisVideoStreamRecordingSourceRuntimeConfiguration ::
  -- | 'streams'
  Prelude.NonEmpty RecordingStreamConfiguration ->
  -- | 'fragmentSelector'
  FragmentSelector ->
  KinesisVideoStreamRecordingSourceRuntimeConfiguration
newKinesisVideoStreamRecordingSourceRuntimeConfiguration
  pStreams_
  pFragmentSelector_ =
    KinesisVideoStreamRecordingSourceRuntimeConfiguration'
      { streams =
          Lens.coerced
            Lens.# pStreams_,
        fragmentSelector =
          pFragmentSelector_
      }

-- | The stream or streams to be recorded.
kinesisVideoStreamRecordingSourceRuntimeConfiguration_streams :: Lens.Lens' KinesisVideoStreamRecordingSourceRuntimeConfiguration (Prelude.NonEmpty RecordingStreamConfiguration)
kinesisVideoStreamRecordingSourceRuntimeConfiguration_streams = Lens.lens (\KinesisVideoStreamRecordingSourceRuntimeConfiguration' {streams} -> streams) (\s@KinesisVideoStreamRecordingSourceRuntimeConfiguration' {} a -> s {streams = a} :: KinesisVideoStreamRecordingSourceRuntimeConfiguration) Prelude.. Lens.coerced

-- | Describes the timestamp range and timestamp origin of a range of
-- fragments in the Kinesis video stream.
kinesisVideoStreamRecordingSourceRuntimeConfiguration_fragmentSelector :: Lens.Lens' KinesisVideoStreamRecordingSourceRuntimeConfiguration FragmentSelector
kinesisVideoStreamRecordingSourceRuntimeConfiguration_fragmentSelector = Lens.lens (\KinesisVideoStreamRecordingSourceRuntimeConfiguration' {fragmentSelector} -> fragmentSelector) (\s@KinesisVideoStreamRecordingSourceRuntimeConfiguration' {} a -> s {fragmentSelector = a} :: KinesisVideoStreamRecordingSourceRuntimeConfiguration)

instance
  Data.FromJSON
    KinesisVideoStreamRecordingSourceRuntimeConfiguration
  where
  parseJSON =
    Data.withObject
      "KinesisVideoStreamRecordingSourceRuntimeConfiguration"
      ( \x ->
          KinesisVideoStreamRecordingSourceRuntimeConfiguration'
            Prelude.<$> (x Data..: "Streams")
            Prelude.<*> (x Data..: "FragmentSelector")
      )

instance
  Prelude.Hashable
    KinesisVideoStreamRecordingSourceRuntimeConfiguration
  where
  hashWithSalt
    _salt
    KinesisVideoStreamRecordingSourceRuntimeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` streams
        `Prelude.hashWithSalt` fragmentSelector

instance
  Prelude.NFData
    KinesisVideoStreamRecordingSourceRuntimeConfiguration
  where
  rnf
    KinesisVideoStreamRecordingSourceRuntimeConfiguration' {..} =
      Prelude.rnf streams
        `Prelude.seq` Prelude.rnf fragmentSelector

instance
  Data.ToJSON
    KinesisVideoStreamRecordingSourceRuntimeConfiguration
  where
  toJSON
    KinesisVideoStreamRecordingSourceRuntimeConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("Streams" Data..= streams),
              Prelude.Just
                ("FragmentSelector" Data..= fragmentSelector)
            ]
        )
