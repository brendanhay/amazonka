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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.GridViewConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.GridViewConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ContentShareLayoutOption
import Amazonka.ChimeSdkMediaPipelines.Types.PresenterOnlyConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type of grid layout.
--
-- /See:/ 'newGridViewConfiguration' smart constructor.
data GridViewConfiguration = GridViewConfiguration'
  { -- | Defines the configuration options for a presenter only video tile.
    presenterOnlyConfiguration :: Prelude.Maybe PresenterOnlyConfiguration,
    -- | Defines the layout of the video tiles when content sharing is enabled.
    contentShareLayout :: ContentShareLayoutOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GridViewConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'presenterOnlyConfiguration', 'gridViewConfiguration_presenterOnlyConfiguration' - Defines the configuration options for a presenter only video tile.
--
-- 'contentShareLayout', 'gridViewConfiguration_contentShareLayout' - Defines the layout of the video tiles when content sharing is enabled.
newGridViewConfiguration ::
  -- | 'contentShareLayout'
  ContentShareLayoutOption ->
  GridViewConfiguration
newGridViewConfiguration pContentShareLayout_ =
  GridViewConfiguration'
    { presenterOnlyConfiguration =
        Prelude.Nothing,
      contentShareLayout = pContentShareLayout_
    }

-- | Defines the configuration options for a presenter only video tile.
gridViewConfiguration_presenterOnlyConfiguration :: Lens.Lens' GridViewConfiguration (Prelude.Maybe PresenterOnlyConfiguration)
gridViewConfiguration_presenterOnlyConfiguration = Lens.lens (\GridViewConfiguration' {presenterOnlyConfiguration} -> presenterOnlyConfiguration) (\s@GridViewConfiguration' {} a -> s {presenterOnlyConfiguration = a} :: GridViewConfiguration)

-- | Defines the layout of the video tiles when content sharing is enabled.
gridViewConfiguration_contentShareLayout :: Lens.Lens' GridViewConfiguration ContentShareLayoutOption
gridViewConfiguration_contentShareLayout = Lens.lens (\GridViewConfiguration' {contentShareLayout} -> contentShareLayout) (\s@GridViewConfiguration' {} a -> s {contentShareLayout = a} :: GridViewConfiguration)

instance Data.FromJSON GridViewConfiguration where
  parseJSON =
    Data.withObject
      "GridViewConfiguration"
      ( \x ->
          GridViewConfiguration'
            Prelude.<$> (x Data..:? "PresenterOnlyConfiguration")
            Prelude.<*> (x Data..: "ContentShareLayout")
      )

instance Prelude.Hashable GridViewConfiguration where
  hashWithSalt _salt GridViewConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` presenterOnlyConfiguration
      `Prelude.hashWithSalt` contentShareLayout

instance Prelude.NFData GridViewConfiguration where
  rnf GridViewConfiguration' {..} =
    Prelude.rnf presenterOnlyConfiguration
      `Prelude.seq` Prelude.rnf contentShareLayout

instance Data.ToJSON GridViewConfiguration where
  toJSON GridViewConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PresenterOnlyConfiguration" Data..=)
              Prelude.<$> presenterOnlyConfiguration,
            Prelude.Just
              ("ContentShareLayout" Data..= contentShareLayout)
          ]
      )
