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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a component. This is a read-only data type that is
-- returned by @ListComponents@.
--
-- /See:/ 'newComponentSummary' smart constructor.
data ComponentSummary = ComponentSummary'
  { -- | The unique ID of the Amplify app associated with the component.
    appId :: Prelude.Text,
    -- | The component type.
    componentType :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the component.
    id :: Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'componentSummary_appId' - The unique ID of the Amplify app associated with the component.
--
-- 'componentType', 'componentSummary_componentType' - The component type.
--
-- 'environmentName', 'componentSummary_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'componentSummary_id' - The unique ID of the component.
--
-- 'name', 'componentSummary_name' - The name of the component.
newComponentSummary ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'componentType'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ComponentSummary
newComponentSummary
  pAppId_
  pComponentType_
  pEnvironmentName_
  pId_
  pName_ =
    ComponentSummary'
      { appId = pAppId_,
        componentType = pComponentType_,
        environmentName = pEnvironmentName_,
        id = pId_,
        name = pName_
      }

-- | The unique ID of the Amplify app associated with the component.
componentSummary_appId :: Lens.Lens' ComponentSummary Prelude.Text
componentSummary_appId = Lens.lens (\ComponentSummary' {appId} -> appId) (\s@ComponentSummary' {} a -> s {appId = a} :: ComponentSummary)

-- | The component type.
componentSummary_componentType :: Lens.Lens' ComponentSummary Prelude.Text
componentSummary_componentType = Lens.lens (\ComponentSummary' {componentType} -> componentType) (\s@ComponentSummary' {} a -> s {componentType = a} :: ComponentSummary)

-- | The name of the backend environment that is a part of the Amplify app.
componentSummary_environmentName :: Lens.Lens' ComponentSummary Prelude.Text
componentSummary_environmentName = Lens.lens (\ComponentSummary' {environmentName} -> environmentName) (\s@ComponentSummary' {} a -> s {environmentName = a} :: ComponentSummary)

-- | The unique ID of the component.
componentSummary_id :: Lens.Lens' ComponentSummary Prelude.Text
componentSummary_id = Lens.lens (\ComponentSummary' {id} -> id) (\s@ComponentSummary' {} a -> s {id = a} :: ComponentSummary)

-- | The name of the component.
componentSummary_name :: Lens.Lens' ComponentSummary Prelude.Text
componentSummary_name = Lens.lens (\ComponentSummary' {name} -> name) (\s@ComponentSummary' {} a -> s {name = a} :: ComponentSummary)

instance Data.FromJSON ComponentSummary where
  parseJSON =
    Data.withObject
      "ComponentSummary"
      ( \x ->
          ComponentSummary'
            Prelude.<$> (x Data..: "appId")
            Prelude.<*> (x Data..: "componentType")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ComponentSummary where
  hashWithSalt _salt ComponentSummary' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData ComponentSummary where
  rnf ComponentSummary' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
