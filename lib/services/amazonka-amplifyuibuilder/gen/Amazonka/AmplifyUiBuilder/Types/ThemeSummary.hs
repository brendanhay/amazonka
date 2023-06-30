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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ThemeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ThemeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the basic information about a theme.
--
-- /See:/ 'newThemeSummary' smart constructor.
data ThemeSummary = ThemeSummary'
  { -- | The unique ID for the app associated with the theme summary.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The ID of the theme.
    id :: Prelude.Text,
    -- | The name of the theme.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'themeSummary_appId' - The unique ID for the app associated with the theme summary.
--
-- 'environmentName', 'themeSummary_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'id', 'themeSummary_id' - The ID of the theme.
--
-- 'name', 'themeSummary_name' - The name of the theme.
newThemeSummary ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ThemeSummary
newThemeSummary pAppId_ pEnvironmentName_ pId_ pName_ =
  ThemeSummary'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_,
      name = pName_
    }

-- | The unique ID for the app associated with the theme summary.
themeSummary_appId :: Lens.Lens' ThemeSummary Prelude.Text
themeSummary_appId = Lens.lens (\ThemeSummary' {appId} -> appId) (\s@ThemeSummary' {} a -> s {appId = a} :: ThemeSummary)

-- | The name of the backend environment that is part of the Amplify app.
themeSummary_environmentName :: Lens.Lens' ThemeSummary Prelude.Text
themeSummary_environmentName = Lens.lens (\ThemeSummary' {environmentName} -> environmentName) (\s@ThemeSummary' {} a -> s {environmentName = a} :: ThemeSummary)

-- | The ID of the theme.
themeSummary_id :: Lens.Lens' ThemeSummary Prelude.Text
themeSummary_id = Lens.lens (\ThemeSummary' {id} -> id) (\s@ThemeSummary' {} a -> s {id = a} :: ThemeSummary)

-- | The name of the theme.
themeSummary_name :: Lens.Lens' ThemeSummary Prelude.Text
themeSummary_name = Lens.lens (\ThemeSummary' {name} -> name) (\s@ThemeSummary' {} a -> s {name = a} :: ThemeSummary)

instance Data.FromJSON ThemeSummary where
  parseJSON =
    Data.withObject
      "ThemeSummary"
      ( \x ->
          ThemeSummary'
            Prelude.<$> (x Data..: "appId")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ThemeSummary where
  hashWithSalt _salt ThemeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ThemeSummary where
  rnf ThemeSummary' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
