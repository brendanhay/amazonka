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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for the composited video artifacts.
--
-- /See:/ 'newCompositedVideoArtifactsConfiguration' smart constructor.
data CompositedVideoArtifactsConfiguration = CompositedVideoArtifactsConfiguration'
  { -- | The layout setting, such as @GridView@ in the configuration object.
    layout :: Prelude.Maybe LayoutOption,
    -- | The video resolution setting in the configuration object. Default: HD at
    -- 1280 x 720. FHD resolution: 1920 x 1080.
    resolution :: Prelude.Maybe ResolutionOption,
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
-- 'layout', 'compositedVideoArtifactsConfiguration_layout' - The layout setting, such as @GridView@ in the configuration object.
--
-- 'resolution', 'compositedVideoArtifactsConfiguration_resolution' - The video resolution setting in the configuration object. Default: HD at
-- 1280 x 720. FHD resolution: 1920 x 1080.
--
-- 'gridViewConfiguration', 'compositedVideoArtifactsConfiguration_gridViewConfiguration' - The @GridView@ configuration setting.
newCompositedVideoArtifactsConfiguration ::
  -- | 'gridViewConfiguration'
  GridViewConfiguration ->
  CompositedVideoArtifactsConfiguration
newCompositedVideoArtifactsConfiguration
  pGridViewConfiguration_ =
    CompositedVideoArtifactsConfiguration'
      { layout =
          Prelude.Nothing,
        resolution = Prelude.Nothing,
        gridViewConfiguration =
          pGridViewConfiguration_
      }

-- | The layout setting, such as @GridView@ in the configuration object.
compositedVideoArtifactsConfiguration_layout :: Lens.Lens' CompositedVideoArtifactsConfiguration (Prelude.Maybe LayoutOption)
compositedVideoArtifactsConfiguration_layout = Lens.lens (\CompositedVideoArtifactsConfiguration' {layout} -> layout) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {layout = a} :: CompositedVideoArtifactsConfiguration)

-- | The video resolution setting in the configuration object. Default: HD at
-- 1280 x 720. FHD resolution: 1920 x 1080.
compositedVideoArtifactsConfiguration_resolution :: Lens.Lens' CompositedVideoArtifactsConfiguration (Prelude.Maybe ResolutionOption)
compositedVideoArtifactsConfiguration_resolution = Lens.lens (\CompositedVideoArtifactsConfiguration' {resolution} -> resolution) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {resolution = a} :: CompositedVideoArtifactsConfiguration)

-- | The @GridView@ configuration setting.
compositedVideoArtifactsConfiguration_gridViewConfiguration :: Lens.Lens' CompositedVideoArtifactsConfiguration GridViewConfiguration
compositedVideoArtifactsConfiguration_gridViewConfiguration = Lens.lens (\CompositedVideoArtifactsConfiguration' {gridViewConfiguration} -> gridViewConfiguration) (\s@CompositedVideoArtifactsConfiguration' {} a -> s {gridViewConfiguration = a} :: CompositedVideoArtifactsConfiguration)

instance
  Data.FromJSON
    CompositedVideoArtifactsConfiguration
  where
  parseJSON =
    Data.withObject
      "CompositedVideoArtifactsConfiguration"
      ( \x ->
          CompositedVideoArtifactsConfiguration'
            Prelude.<$> (x Data..:? "Layout")
            Prelude.<*> (x Data..:? "Resolution")
            Prelude.<*> (x Data..: "GridViewConfiguration")
      )

instance
  Prelude.Hashable
    CompositedVideoArtifactsConfiguration
  where
  hashWithSalt
    _salt
    CompositedVideoArtifactsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` layout
        `Prelude.hashWithSalt` resolution
        `Prelude.hashWithSalt` gridViewConfiguration

instance
  Prelude.NFData
    CompositedVideoArtifactsConfiguration
  where
  rnf CompositedVideoArtifactsConfiguration' {..} =
    Prelude.rnf layout
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf gridViewConfiguration

instance
  Data.ToJSON
    CompositedVideoArtifactsConfiguration
  where
  toJSON CompositedVideoArtifactsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Layout" Data..=) Prelude.<$> layout,
            ("Resolution" Data..=) Prelude.<$> resolution,
            Prelude.Just
              ( "GridViewConfiguration"
                  Data..= gridViewConfiguration
              )
          ]
      )
