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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ContentArtifactsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ContentArtifactsConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsState
import Amazonka.ChimeSdkMediaPipelines.Types.ContentMuxType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The content artifact object.
--
-- /See:/ 'newContentArtifactsConfiguration' smart constructor.
data ContentArtifactsConfiguration = ContentArtifactsConfiguration'
  { -- | The MUX type of the artifact configuration.
    muxType :: Prelude.Maybe ContentMuxType,
    -- | Indicates whether the content artifact is enabled or disabled.
    state :: ArtifactsState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContentArtifactsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'muxType', 'contentArtifactsConfiguration_muxType' - The MUX type of the artifact configuration.
--
-- 'state', 'contentArtifactsConfiguration_state' - Indicates whether the content artifact is enabled or disabled.
newContentArtifactsConfiguration ::
  -- | 'state'
  ArtifactsState ->
  ContentArtifactsConfiguration
newContentArtifactsConfiguration pState_ =
  ContentArtifactsConfiguration'
    { muxType =
        Prelude.Nothing,
      state = pState_
    }

-- | The MUX type of the artifact configuration.
contentArtifactsConfiguration_muxType :: Lens.Lens' ContentArtifactsConfiguration (Prelude.Maybe ContentMuxType)
contentArtifactsConfiguration_muxType = Lens.lens (\ContentArtifactsConfiguration' {muxType} -> muxType) (\s@ContentArtifactsConfiguration' {} a -> s {muxType = a} :: ContentArtifactsConfiguration)

-- | Indicates whether the content artifact is enabled or disabled.
contentArtifactsConfiguration_state :: Lens.Lens' ContentArtifactsConfiguration ArtifactsState
contentArtifactsConfiguration_state = Lens.lens (\ContentArtifactsConfiguration' {state} -> state) (\s@ContentArtifactsConfiguration' {} a -> s {state = a} :: ContentArtifactsConfiguration)

instance Core.FromJSON ContentArtifactsConfiguration where
  parseJSON =
    Core.withObject
      "ContentArtifactsConfiguration"
      ( \x ->
          ContentArtifactsConfiguration'
            Prelude.<$> (x Core..:? "MuxType")
            Prelude.<*> (x Core..: "State")
      )

instance
  Prelude.Hashable
    ContentArtifactsConfiguration
  where
  hashWithSalt _salt ContentArtifactsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` muxType
      `Prelude.hashWithSalt` state

instance Prelude.NFData ContentArtifactsConfiguration where
  rnf ContentArtifactsConfiguration' {..} =
    Prelude.rnf muxType `Prelude.seq` Prelude.rnf state

instance Core.ToJSON ContentArtifactsConfiguration where
  toJSON ContentArtifactsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MuxType" Core..=) Prelude.<$> muxType,
            Prelude.Just ("State" Core..= state)
          ]
      )
