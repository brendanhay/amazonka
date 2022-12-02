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
-- Module      : Amazonka.AmplifyUiBuilder.Types.UpdateComponentData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.UpdateComponentData where

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

-- | Updates and saves all of the information about a component, based on
-- component ID.
--
-- /See:/ 'newUpdateComponentData' smart constructor.
data UpdateComponentData = UpdateComponentData'
  { -- | The name of the component to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the unique variants of the main component being updated.
    variants :: Prelude.Maybe [ComponentVariant],
    -- | The unique ID of the component in its original source system, such as
    -- Figma.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | Describes the component\'s properties.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentProperty),
    -- | The data binding information for the component\'s properties.
    bindingProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue),
    -- | The unique ID of the component to update.
    id :: Prelude.Maybe Prelude.Text,
    -- | The components that are instances of the main component.
    children :: Prelude.Maybe [ComponentChild],
    -- | The event configuration for the component. Use for the workflow feature
    -- in Amplify Studio that allows you to bind events and actions to
    -- components.
    events :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent),
    -- | The schema version of the component when it was imported.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The configuration for binding a component\'s properties to a data model.
    -- Use this for a collection component.
    collectionProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration),
    -- | The type of the component. This can be an Amplify custom UI component or
    -- another custom component.
    componentType :: Prelude.Maybe Prelude.Text,
    -- | Describes the properties that can be overriden to customize the
    -- component.
    overrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateComponentData_name' - The name of the component to update.
--
-- 'variants', 'updateComponentData_variants' - A list of the unique variants of the main component being updated.
--
-- 'sourceId', 'updateComponentData_sourceId' - The unique ID of the component in its original source system, such as
-- Figma.
--
-- 'properties', 'updateComponentData_properties' - Describes the component\'s properties.
--
-- 'bindingProperties', 'updateComponentData_bindingProperties' - The data binding information for the component\'s properties.
--
-- 'id', 'updateComponentData_id' - The unique ID of the component to update.
--
-- 'children', 'updateComponentData_children' - The components that are instances of the main component.
--
-- 'events', 'updateComponentData_events' - The event configuration for the component. Use for the workflow feature
-- in Amplify Studio that allows you to bind events and actions to
-- components.
--
-- 'schemaVersion', 'updateComponentData_schemaVersion' - The schema version of the component when it was imported.
--
-- 'collectionProperties', 'updateComponentData_collectionProperties' - The configuration for binding a component\'s properties to a data model.
-- Use this for a collection component.
--
-- 'componentType', 'updateComponentData_componentType' - The type of the component. This can be an Amplify custom UI component or
-- another custom component.
--
-- 'overrides', 'updateComponentData_overrides' - Describes the properties that can be overriden to customize the
-- component.
newUpdateComponentData ::
  UpdateComponentData
newUpdateComponentData =
  UpdateComponentData'
    { name = Prelude.Nothing,
      variants = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      properties = Prelude.Nothing,
      bindingProperties = Prelude.Nothing,
      id = Prelude.Nothing,
      children = Prelude.Nothing,
      events = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      collectionProperties = Prelude.Nothing,
      componentType = Prelude.Nothing,
      overrides = Prelude.Nothing
    }

-- | The name of the component to update.
updateComponentData_name :: Lens.Lens' UpdateComponentData (Prelude.Maybe Prelude.Text)
updateComponentData_name = Lens.lens (\UpdateComponentData' {name} -> name) (\s@UpdateComponentData' {} a -> s {name = a} :: UpdateComponentData)

-- | A list of the unique variants of the main component being updated.
updateComponentData_variants :: Lens.Lens' UpdateComponentData (Prelude.Maybe [ComponentVariant])
updateComponentData_variants = Lens.lens (\UpdateComponentData' {variants} -> variants) (\s@UpdateComponentData' {} a -> s {variants = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the component in its original source system, such as
-- Figma.
updateComponentData_sourceId :: Lens.Lens' UpdateComponentData (Prelude.Maybe Prelude.Text)
updateComponentData_sourceId = Lens.lens (\UpdateComponentData' {sourceId} -> sourceId) (\s@UpdateComponentData' {} a -> s {sourceId = a} :: UpdateComponentData)

-- | Describes the component\'s properties.
updateComponentData_properties :: Lens.Lens' UpdateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentProperty))
updateComponentData_properties = Lens.lens (\UpdateComponentData' {properties} -> properties) (\s@UpdateComponentData' {} a -> s {properties = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The data binding information for the component\'s properties.
updateComponentData_bindingProperties :: Lens.Lens' UpdateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue))
updateComponentData_bindingProperties = Lens.lens (\UpdateComponentData' {bindingProperties} -> bindingProperties) (\s@UpdateComponentData' {} a -> s {bindingProperties = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the component to update.
updateComponentData_id :: Lens.Lens' UpdateComponentData (Prelude.Maybe Prelude.Text)
updateComponentData_id = Lens.lens (\UpdateComponentData' {id} -> id) (\s@UpdateComponentData' {} a -> s {id = a} :: UpdateComponentData)

-- | The components that are instances of the main component.
updateComponentData_children :: Lens.Lens' UpdateComponentData (Prelude.Maybe [ComponentChild])
updateComponentData_children = Lens.lens (\UpdateComponentData' {children} -> children) (\s@UpdateComponentData' {} a -> s {children = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The event configuration for the component. Use for the workflow feature
-- in Amplify Studio that allows you to bind events and actions to
-- components.
updateComponentData_events :: Lens.Lens' UpdateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent))
updateComponentData_events = Lens.lens (\UpdateComponentData' {events} -> events) (\s@UpdateComponentData' {} a -> s {events = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The schema version of the component when it was imported.
updateComponentData_schemaVersion :: Lens.Lens' UpdateComponentData (Prelude.Maybe Prelude.Text)
updateComponentData_schemaVersion = Lens.lens (\UpdateComponentData' {schemaVersion} -> schemaVersion) (\s@UpdateComponentData' {} a -> s {schemaVersion = a} :: UpdateComponentData)

-- | The configuration for binding a component\'s properties to a data model.
-- Use this for a collection component.
updateComponentData_collectionProperties :: Lens.Lens' UpdateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration))
updateComponentData_collectionProperties = Lens.lens (\UpdateComponentData' {collectionProperties} -> collectionProperties) (\s@UpdateComponentData' {} a -> s {collectionProperties = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

-- | The type of the component. This can be an Amplify custom UI component or
-- another custom component.
updateComponentData_componentType :: Lens.Lens' UpdateComponentData (Prelude.Maybe Prelude.Text)
updateComponentData_componentType = Lens.lens (\UpdateComponentData' {componentType} -> componentType) (\s@UpdateComponentData' {} a -> s {componentType = a} :: UpdateComponentData)

-- | Describes the properties that can be overriden to customize the
-- component.
updateComponentData_overrides :: Lens.Lens' UpdateComponentData (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
updateComponentData_overrides = Lens.lens (\UpdateComponentData' {overrides} -> overrides) (\s@UpdateComponentData' {} a -> s {overrides = a} :: UpdateComponentData) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UpdateComponentData where
  hashWithSalt _salt UpdateComponentData' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` variants
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` collectionProperties
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` overrides

instance Prelude.NFData UpdateComponentData where
  rnf UpdateComponentData' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf variants
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf children
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf collectionProperties
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf overrides

instance Data.ToJSON UpdateComponentData where
  toJSON UpdateComponentData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("variants" Data..=) Prelude.<$> variants,
            ("sourceId" Data..=) Prelude.<$> sourceId,
            ("properties" Data..=) Prelude.<$> properties,
            ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties,
            ("id" Data..=) Prelude.<$> id,
            ("children" Data..=) Prelude.<$> children,
            ("events" Data..=) Prelude.<$> events,
            ("schemaVersion" Data..=) Prelude.<$> schemaVersion,
            ("collectionProperties" Data..=)
              Prelude.<$> collectionProperties,
            ("componentType" Data..=) Prelude.<$> componentType,
            ("overrides" Data..=) Prelude.<$> overrides
          ]
      )
