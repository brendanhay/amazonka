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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.VideoConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.VideoConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object of a video contacatentation pipeline.
--
-- /See:/ 'newVideoConcatenationConfiguration' smart constructor.
data VideoConcatenationConfiguration = VideoConcatenationConfiguration'
  { -- | Enables or disables the configuration object.
    state :: ArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'videoConcatenationConfiguration_state' - Enables or disables the configuration object.
newVideoConcatenationConfiguration ::
  -- | 'state'
  ArtifactsConcatenationState ->
  VideoConcatenationConfiguration
newVideoConcatenationConfiguration pState_ =
  VideoConcatenationConfiguration' {state = pState_}

-- | Enables or disables the configuration object.
videoConcatenationConfiguration_state :: Lens.Lens' VideoConcatenationConfiguration ArtifactsConcatenationState
videoConcatenationConfiguration_state = Lens.lens (\VideoConcatenationConfiguration' {state} -> state) (\s@VideoConcatenationConfiguration' {} a -> s {state = a} :: VideoConcatenationConfiguration)

instance
  Data.FromJSON
    VideoConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "VideoConcatenationConfiguration"
      ( \x ->
          VideoConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    VideoConcatenationConfiguration
  where
  hashWithSalt
    _salt
    VideoConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    VideoConcatenationConfiguration
  where
  rnf VideoConcatenationConfiguration' {..} =
    Prelude.rnf state

instance Data.ToJSON VideoConcatenationConfiguration where
  toJSON VideoConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("State" Data..= state)]
      )
