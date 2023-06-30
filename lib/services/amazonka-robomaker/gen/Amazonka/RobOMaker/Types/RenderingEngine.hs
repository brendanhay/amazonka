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
-- Module      : Amazonka.RobOMaker.Types.RenderingEngine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RenderingEngine where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.RenderingEngineType

-- | Information about a rendering engine.
--
-- /See:/ 'newRenderingEngine' smart constructor.
data RenderingEngine = RenderingEngine'
  { -- | The name of the rendering engine.
    name :: Prelude.Maybe RenderingEngineType,
    -- | The version of the rendering engine.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenderingEngine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'renderingEngine_name' - The name of the rendering engine.
--
-- 'version', 'renderingEngine_version' - The version of the rendering engine.
newRenderingEngine ::
  RenderingEngine
newRenderingEngine =
  RenderingEngine'
    { name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the rendering engine.
renderingEngine_name :: Lens.Lens' RenderingEngine (Prelude.Maybe RenderingEngineType)
renderingEngine_name = Lens.lens (\RenderingEngine' {name} -> name) (\s@RenderingEngine' {} a -> s {name = a} :: RenderingEngine)

-- | The version of the rendering engine.
renderingEngine_version :: Lens.Lens' RenderingEngine (Prelude.Maybe Prelude.Text)
renderingEngine_version = Lens.lens (\RenderingEngine' {version} -> version) (\s@RenderingEngine' {} a -> s {version = a} :: RenderingEngine)

instance Data.FromJSON RenderingEngine where
  parseJSON =
    Data.withObject
      "RenderingEngine"
      ( \x ->
          RenderingEngine'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable RenderingEngine where
  hashWithSalt _salt RenderingEngine' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData RenderingEngine where
  rnf RenderingEngine' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToJSON RenderingEngine where
  toJSON RenderingEngine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("version" Data..=) Prelude.<$> version
          ]
      )
