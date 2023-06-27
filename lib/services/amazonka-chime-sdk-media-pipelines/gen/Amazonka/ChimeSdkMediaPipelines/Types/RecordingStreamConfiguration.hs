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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.RecordingStreamConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.RecordingStreamConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that holds the settings for recording media.
--
-- /See:/ 'newRecordingStreamConfiguration' smart constructor.
data RecordingStreamConfiguration = RecordingStreamConfiguration'
  { -- | The ARN of the recording stream.
    streamArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordingStreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamArn', 'recordingStreamConfiguration_streamArn' - The ARN of the recording stream.
newRecordingStreamConfiguration ::
  RecordingStreamConfiguration
newRecordingStreamConfiguration =
  RecordingStreamConfiguration'
    { streamArn =
        Prelude.Nothing
    }

-- | The ARN of the recording stream.
recordingStreamConfiguration_streamArn :: Lens.Lens' RecordingStreamConfiguration (Prelude.Maybe Prelude.Text)
recordingStreamConfiguration_streamArn = Lens.lens (\RecordingStreamConfiguration' {streamArn} -> streamArn) (\s@RecordingStreamConfiguration' {} a -> s {streamArn = a} :: RecordingStreamConfiguration)

instance Data.FromJSON RecordingStreamConfiguration where
  parseJSON =
    Data.withObject
      "RecordingStreamConfiguration"
      ( \x ->
          RecordingStreamConfiguration'
            Prelude.<$> (x Data..:? "StreamArn")
      )

instance
  Prelude.Hashable
    RecordingStreamConfiguration
  where
  hashWithSalt _salt RecordingStreamConfiguration' {..} =
    _salt `Prelude.hashWithSalt` streamArn

instance Prelude.NFData RecordingStreamConfiguration where
  rnf RecordingStreamConfiguration' {..} =
    Prelude.rnf streamArn

instance Data.ToJSON RecordingStreamConfiguration where
  toJSON RecordingStreamConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StreamArn" Data..=) Prelude.<$> streamArn]
      )
