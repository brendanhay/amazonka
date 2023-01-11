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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CreateComponentData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CreateComponentData where

import Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValue
import Amazonka.AmplifyUiBuilder.Types.ComponentChild
import Amazonka.AmplifyUiBuilder.Types.ComponentDataConfiguration
import Amazonka.AmplifyUiBuilder.Types.ComponentEvent
import Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import Amazonka.AmplifyUiBuilder.Types.ComponentVariant
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents all of the information that is required to create a
-- component.
--
-- /See:/ 'newCreateComponentData' smart constructor.
data CreateComponentData = CreateComponentData'
  { -- | A list of child components that are instances of the main component.
    children :: Prelude.Maybe [ComponentChild],
    -- | The data binding configuration for customizing a component\'s
    -- properties. Use this for a collection component.
    collectionProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration),
    -- | The event configuration for the component. Use for the workflow feature
    -- in Amplify Studio that allows you to bind events and actions to
    -- components.
    events :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent),
    -- | The schema version of the component when it was imported.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the component in its original source system, such as
    -- Figma.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | One or more key-value pairs to use when tagging the component data.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The data binding information for the component\'s properties.
    bindingProperties :: Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue,
    -- | The component type. This can be an Amplify custom UI component or
    -- another custom component.
    componentType :: Prelude.Text,
    -- | The name of the component
    name :: Prelude.Text,
    -- | Describes the component properties that can be overriden to customize an
    -- instance of the component.
    overrides :: Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Describes the component\'s properties.
    properties :: Prelude.HashMap Prelude.Text ComponentProperty,
    -- | A list of the unique variants of this component.
    variants :: [ComponentVariant]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'createComponentData_children' - A list of child components that are instances of the main component.
--
-- 'collectionProperties', 'createComponentData_collectionProperties' - The data binding configuration for customizing a component\'s
-- properties. Use this for a collection component.
--
-- 'events', 'createComponentData_events' - The event configuration for the component. Use for the workflow feature
-- in Amplify Studio that allows you to bind events and actions to
-- components.
--
-- 'schemaVersion', 'createComponentData_schemaVersion' - The schema version of the component when it was imported.
--
-- 'sourceId', 'createComponentData_sourceId' - The unique ID of the component in its original source system, such as
-- Figma.
--
-- 'tags', 'createComponentData_tags' - One or more key-value pairs to use when tagging the component data.
--
-- 'bindingProperties', 'createComponentData_bindingProperties' - The data binding information for the component\'s properties.
--
-- 'componentType', 'createComponentData_componentType' - The component type. This can be an Amplify custom UI component or
-- another custom component.
--
-- 'name', 'createComponentData_name' - The name of the component
--
-- 'overrides', 'createComponentData_overrides' - Describes the component properties that can be overriden to customize an
-- instance of the component.
--
-- 'properties', 'createComponentData_properties' - Describes the component\'s properties.
--
-- 'variants', 'createComponentData_variants' - A list of the unique variants of this component.
newCreateComponentData ::
  -- | 'componentType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateComponentData
newCreateComponentData pComponentType_ pName_ =
  CreateComponentData'
    { children = Prelude.Nothing,
      collectionProperties = Prelude.Nothing,
      events = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      bindingProperties = Prelude.mempty,
      componentType = pComponentType_,
      name = pName_,
      overrides = Prelude.mempty,
      properties = Prelude.mempty,
      variants = Prelude.mempty
    }

-- | A list of child components that are instances of the main component.
createComponentData_children :: Lens.Lens' CreateComponentData (Prelude.Maybe [ComponentChild])
createComponentData_children = Lens.lens (\CreateComponentData' {children} -> children) (\s@CreateComponentData' {} a -> s {children = a} :: CreateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The data binding configuration for customizing a component\'s
-- properties. Use this for a collection component.
createComponentData_collectionProperties :: Lens.Lens' CreateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration))
createComponentData_collectionProperties = Lens.lens (\CreateComponentData' {collectionProperties} -> collectionProperties) (\s@CreateComponentData' {} a -> s {collectionProperties = a} :: CreateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The event configuration for the component. Use for the workflow feature
-- in Amplify Studio that allows you to bind events and actions to
-- components.
createComponentData_events :: Lens.Lens' CreateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent))
createComponentData_events = Lens.lens (\CreateComponentData' {events} -> events) (\s@CreateComponentData' {} a -> s {events = a} :: CreateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The schema version of the component when it was imported.
createComponentData_schemaVersion :: Lens.Lens' CreateComponentData (Prelude.Maybe Prelude.Text)
createComponentData_schemaVersion = Lens.lens (\CreateComponentData' {schemaVersion} -> schemaVersion) (\s@CreateComponentData' {} a -> s {schemaVersion = a} :: CreateComponentData)

-- | The unique ID of the component in its original source system, such as
-- Figma.
createComponentData_sourceId :: Lens.Lens' CreateComponentData (Prelude.Maybe Prelude.Text)
createComponentData_sourceId = Lens.lens (\CreateComponentData' {sourceId} -> sourceId) (\s@CreateComponentData' {} a -> s {sourceId = a} :: CreateComponentData)

-- | One or more key-value pairs to use when tagging the component data.
createComponentData_tags :: Lens.Lens' CreateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createComponentData_tags = Lens.lens (\CreateComponentData' {tags} -> tags) (\s@CreateComponentData' {} a -> s {tags = a} :: CreateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The data binding information for the component\'s properties.
createComponentData_bindingProperties :: Lens.Lens' CreateComponentData (Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue)
createComponentData_bindingProperties = Lens.lens (\CreateComponentData' {bindingProperties} -> bindingProperties) (\s@CreateComponentData' {} a -> s {bindingProperties = a} :: CreateComponentData) Prelude.. Lens.coerced

-- | The component type. This can be an Amplify custom UI component or
-- another custom component.
createComponentData_componentType :: Lens.Lens' CreateComponentData Prelude.Text
createComponentData_componentType = Lens.lens (\CreateComponentData' {componentType} -> componentType) (\s@CreateComponentData' {} a -> s {componentType = a} :: CreateComponentData)

-- | The name of the component
createComponentData_name :: Lens.Lens' CreateComponentData Prelude.Text
createComponentData_name = Lens.lens (\CreateComponentData' {name} -> name) (\s@CreateComponentData' {} a -> s {name = a} :: CreateComponentData)

-- | Describes the component properties that can be overriden to customize an
-- instance of the component.
createComponentData_overrides :: Lens.Lens' CreateComponentData (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text))
createComponentData_overrides = Lens.lens (\CreateComponentData' {overrides} -> overrides) (\s@CreateComponentData' {} a -> s {overrides = a} :: CreateComponentData) Prelude.. Lens.coerced

-- | Describes the component\'s properties.
createComponentData_properties :: Lens.Lens' CreateComponentData (Prelude.HashMap Prelude.Text ComponentProperty)
createComponentData_properties = Lens.lens (\CreateComponentData' {properties} -> properties) (\s@CreateComponentData' {} a -> s {properties = a} :: CreateComponentData) Prelude.. Lens.coerced

-- | A list of the unique variants of this component.
createComponentData_variants :: Lens.Lens' CreateComponentData [ComponentVariant]
createComponentData_variants = Lens.lens (\CreateComponentData' {variants} -> variants) (\s@CreateComponentData' {} a -> s {variants = a} :: CreateComponentData) Prelude.. Lens.coerced

instance Prelude.Hashable CreateComponentData where
  hashWithSalt _salt CreateComponentData' {..} =
    _salt `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` collectionProperties
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` variants

instance Prelude.NFData CreateComponentData where
  rnf CreateComponentData' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf collectionProperties
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf variants

instance Data.ToJSON CreateComponentData where
  toJSON CreateComponentData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("children" Data..=) Prelude.<$> children,
            ("collectionProperties" Data..=)
              Prelude.<$> collectionProperties,
            ("events" Data..=) Prelude.<$> events,
            ("schemaVersion" Data..=) Prelude.<$> schemaVersion,
            ("sourceId" Data..=) Prelude.<$> sourceId,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("bindingProperties" Data..= bindingProperties),
            Prelude.Just ("componentType" Data..= componentType),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("overrides" Data..= overrides),
            Prelude.Just ("properties" Data..= properties),
            Prelude.Just ("variants" Data..= variants)
          ]
      )
