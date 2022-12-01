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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoArtifactsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.CompositedVideoArtifactsConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.GridViewConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LayoutOption
import Amazonka.ChimeSdkMediaPipelines.Types.ResolutionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for the composited video artifacts.
--
-- /See:/ 'newCompositedVideoArtifactsConfiguration' smart constructor.
data CompositedVideoArtifactsConfiguration = CompositedVideoArtifactsConfiguration'
  { -- | The video resolution setting in the configuration object. Default: HD at
    -- 1280 x 720. FHD resolution: 1920 x 1080.
    resolution :: Prelude.Maybe ResolutionOption,
    -- | The layout setting, such as @GridView@ in the configuration object.
    layout :: Prelude.Maybe LayoutOption,
    -- | The @GridView@ configuration setting.
    gridViewConfiguration :: GridViewConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompositedVideoArtifactsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolution', 'compositedVideoArtifactsConfiguration_resolution' - The video resolution setting in the configuration object. Default: HD at
-- 1280 x 720. FHD resolution: 1920 x 1080.
--
-- 'layout', 'compositedVideoArtifactsConfiguration_layout' - The layout setting, such as @GridView@ in the configuration object.
--
-- 'gridViewConfiguration', 'compositedVideoArtifactsConfiguration_gridViewConfiguration' - The @GridView@ configuration setting.
newCompositedVideoArtifactsConfiguration ::
  -- | 'gridViewConfiguration'
  GridViewConfiguration ->
  CompositedVideoArtifactsConfiguration
newCompositedVideoArtifactsConfiguration
  pGridViewConfiguration_ =
    CompositedVideoArtifactsConfiguration'
      { resolution =
          Prelude.Nothing,
        layout = Prelude.Nothing,
        gridViewConfiguration =
          pGridViewConfiguration_
      }

-- | The video resolution setting in the configuration object. Default: HD at
-- 1280 x 720. FHD resolution: 1920 x 1080.
compositedVideoArtifactsConfiguration_resolution :: Lens.Lens' CompositedVideoArtifactsConfiguration (Prelude.Maybe ResolutionOption)
compositedVideoArtifactsConfiguration_resolution = Lens.lens (\CompositedVideoArtifactsConfiguration' {resolution} -> resolution) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {resolution = a} :: CompositedVideoArtifactsConfiguration)

-- | The layout setting, such as @GridView@ in the configuration object.
compositedVideoArtifactsConfiguration_layout :: Lens.Lens' CompositedVideoArtifactsConfiguration (Prelude.Maybe LayoutOption)
compositedVideoArtifactsConfiguration_layout = Lens.lens (\CompositedVideoArtifactsConfiguration' {layout} -> layout) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {layout = a} :: CompositedVideoArtifactsConfiguration)

-- | The @GridView@ configuration setting.
compositedVideoArtifactsConfiguration_gridViewConfiguration :: Lens.Lens' CompositedVideoArtifactsConfiguration GridViewConfiguration
compositedVideoArtifactsConfiguration_gridViewConfiguration = Lens.lens (\CompositedVideoArtifactsConfiguration' {gridViewConfiguration} -> gridViewConfiguration) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {gridViewConfiguration = a} :: CompositedVideoArtifactsConfiguration)

instance
  Core.FromJSON
    CompositedVideoArtifactsConfiguration
  where
  parseJSON =
    Core.withObject
      "CompositedVideoArtifactsConfiguration"
      ( \x ->
          CompositedVideoArtifactsConfiguration'
            Prelude.<$> (x Core..:? "Resolution")
            Prelude.<*> (x Core..:? "Layout")
            Prelude.<*> (x Core..: "GridViewConfiguration")
      )

instance
  Prelude.Hashable
    CompositedVideoArtifactsConfiguration
  where
  hashWithSalt
    _salt
    CompositedVideoArtifactsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` resolution
        `Prelude.hashWithSalt` layout
        `Prelude.hashWithSalt` gridViewConfiguration

instance
  Prelude.NFData
    CompositedVideoArtifactsConfiguration
  where
  rnf CompositedVideoArtifactsConfiguration' {..} =
    Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf layout
      `Prelude.seq` Prelude.rnf gridViewConfiguration

instance
  Core.ToJSON
    CompositedVideoArtifactsConfiguration
  where
  toJSON CompositedVideoArtifactsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Resolution" Core..=) Prelude.<$> resolution,
            ("Layout" Core..=) Prelude.<$> layout,
            Prelude.Just
              ( "GridViewConfiguration"
                  Core..= gridViewConfiguration
              )
          ]
      )
