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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.TranscriptionMessagesConcatenationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.TranscriptionMessagesConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for concatenating transcription messages.
--
-- /See:/ 'newTranscriptionMessagesConcatenationConfiguration' smart constructor.
data TranscriptionMessagesConcatenationConfiguration = TranscriptionMessagesConcatenationConfiguration'
  { -- | Enables or disables the configuration object.
    state :: ArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranscriptionMessagesConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'transcriptionMessagesConcatenationConfiguration_state' - Enables or disables the configuration object.
newTranscriptionMessagesConcatenationConfiguration ::
  -- | 'state'
  ArtifactsConcatenationState ->
  TranscriptionMessagesConcatenationConfiguration
newTranscriptionMessagesConcatenationConfiguration
  pState_ =
    TranscriptionMessagesConcatenationConfiguration'
      { state =
          pState_
      }

-- | Enables or disables the configuration object.
transcriptionMessagesConcatenationConfiguration_state :: Lens.Lens' TranscriptionMessagesConcatenationConfiguration ArtifactsConcatenationState
transcriptionMessagesConcatenationConfiguration_state = Lens.lens (\TranscriptionMessagesConcatenationConfiguration' {state} -> state) (\s@TranscriptionMessagesConcatenationConfiguration' {} a -> s {state = a} :: TranscriptionMessagesConcatenationConfiguration)

instance
  Data.FromJSON
    TranscriptionMessagesConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "TranscriptionMessagesConcatenationConfiguration"
      ( \x ->
          TranscriptionMessagesConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    TranscriptionMessagesConcatenationConfiguration
  where
  hashWithSalt
    _salt
    TranscriptionMessagesConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    TranscriptionMessagesConcatenationConfiguration
  where
  rnf
    TranscriptionMessagesConcatenationConfiguration' {..} =
      Prelude.rnf state

instance
  Data.ToJSON
    TranscriptionMessagesConcatenationConfiguration
  where
  toJSON
    TranscriptionMessagesConcatenationConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("State" Data..= state)]
        )
