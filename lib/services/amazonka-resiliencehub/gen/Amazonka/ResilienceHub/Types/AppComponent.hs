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
-- Module      : Amazonka.ResilienceHub.Types.AppComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines an application component.
--
-- /See:/ 'newAppComponent' smart constructor.
data AppComponent = AppComponent'
  { -- | The name of the application component.
    name :: Prelude.Text,
    -- | The type of application component.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'appComponent_name' - The name of the application component.
--
-- 'type'', 'appComponent_type' - The type of application component.
newAppComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  AppComponent
newAppComponent pName_ pType_ =
  AppComponent' {name = pName_, type' = pType_}

-- | The name of the application component.
appComponent_name :: Lens.Lens' AppComponent Prelude.Text
appComponent_name = Lens.lens (\AppComponent' {name} -> name) (\s@AppComponent' {} a -> s {name = a} :: AppComponent)

-- | The type of application component.
appComponent_type :: Lens.Lens' AppComponent Prelude.Text
appComponent_type = Lens.lens (\AppComponent' {type'} -> type') (\s@AppComponent' {} a -> s {type' = a} :: AppComponent)

instance Core.FromJSON AppComponent where
  parseJSON =
    Core.withObject
      "AppComponent"
      ( \x ->
          AppComponent'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable AppComponent where
  hashWithSalt _salt AppComponent' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AppComponent where
  rnf AppComponent' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'
