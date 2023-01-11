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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.AudioConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.AudioConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The audio artifact concatenation configuration object.
--
-- /See:/ 'newAudioConcatenationConfiguration' smart constructor.
data AudioConcatenationConfiguration = AudioConcatenationConfiguration'
  { -- | Enables the /name/ object, where /name/ is the name of the configuration
    -- object, such as @AudioConcatenation@.
    state :: AudioArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'audioConcatenationConfiguration_state' - Enables the /name/ object, where /name/ is the name of the configuration
-- object, such as @AudioConcatenation@.
newAudioConcatenationConfiguration ::
  -- | 'state'
  AudioArtifactsConcatenationState ->
  AudioConcatenationConfiguration
newAudioConcatenationConfiguration pState_ =
  AudioConcatenationConfiguration' {state = pState_}

-- | Enables the /name/ object, where /name/ is the name of the configuration
-- object, such as @AudioConcatenation@.
audioConcatenationConfiguration_state :: Lens.Lens' AudioConcatenationConfiguration AudioArtifactsConcatenationState
audioConcatenationConfiguration_state = Lens.lens (\AudioConcatenationConfiguration' {state} -> state) (\s@AudioConcatenationConfiguration' {} a -> s {state = a} :: AudioConcatenationConfiguration)

instance
  Data.FromJSON
    AudioConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "AudioConcatenationConfiguration"
      ( \x ->
          AudioConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    AudioConcatenationConfiguration
  where
  hashWithSalt
    _salt
    AudioConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    AudioConcatenationConfiguration
  where
  rnf AudioConcatenationConfiguration' {..} =
    Prelude.rnf state

instance Data.ToJSON AudioConcatenationConfiguration where
  toJSON AudioConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("State" Data..= state)]
      )
