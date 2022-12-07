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
-- Module      : Amazonka.AmplifyUiBuilder.Types.Component
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.Component where

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

-- | Contains the configuration settings for a user interface (UI) element
-- for an Amplify app. A component is configured as a primary, stand-alone
-- UI element. Use @ComponentChild@ to configure an instance of a
-- @Component@. A @ComponentChild@ instance inherits the configuration of
-- the main @Component@.
--
-- /See:/ 'newComponent' smart constructor.
data Component = Component'
  { -- | One or more key-value pairs to use when tagging the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time that the component was modified.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The unique ID of the component in its original source system, such as
    -- Figma.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | A list of the component\'s @ComponentChild@ instances.
    children :: Prelude.Maybe [ComponentChild],
    -- | Describes the events that can be raised on the component. Use for the
    -- workflow feature in Amplify Studio that allows you to bind events and
    -- actions to components.
    events :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent),
    -- | The schema version of the component when it was imported.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The data binding configuration for the component\'s properties. Use this
    -- for a collection component. You can\'t specify @tags@ as a valid
    -- property for @collectionProperties@.
    collectionProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration),
    -- | The unique ID of the Amplify app associated with the component.
    appId :: Prelude.Text,
    -- | The information to connect a component\'s properties to data at runtime.
    -- You can\'t specify @tags@ as a valid property for @bindingProperties@.
    bindingProperties :: Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue,
    -- | The type of the component. This can be an Amplify custom UI component or
    -- another custom component.
    componentType :: Prelude.Text,
    -- | The time that the component was created.
    createdAt :: Data.POSIX,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the component.
    id :: Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Text,
    -- | Describes the component\'s properties that can be overriden in a
    -- customized instance of the component. You can\'t specify @tags@ as a
    -- valid property for @overrides@.
    overrides :: Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Describes the component\'s properties. You can\'t specify @tags@ as a
    -- valid property for @properties@.
    properties :: Prelude.HashMap Prelude.Text ComponentProperty,
    -- | A list of the component\'s variants. A variant is a unique style
    -- configuration of a main component.
    variants :: [ComponentVariant]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Component' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'component_tags' - One or more key-value pairs to use when tagging the component.
--
-- 'modifiedAt', 'component_modifiedAt' - The time that the component was modified.
--
-- 'sourceId', 'component_sourceId' - The unique ID of the component in its original source system, such as
-- Figma.
--
-- 'children', 'component_children' - A list of the component\'s @ComponentChild@ instances.
--
-- 'events', 'component_events' - Describes the events that can be raised on the component. Use for the
-- workflow feature in Amplify Studio that allows you to bind events and
-- actions to components.
--
-- 'schemaVersion', 'component_schemaVersion' - The schema version of the component when it was imported.
--
-- 'collectionProperties', 'component_collectionProperties' - The data binding configuration for the component\'s properties. Use this
-- for a collection component. You can\'t specify @tags@ as a valid
-- property for @collectionProperties@.
--
-- 'appId', 'component_appId' - The unique ID of the Amplify app associated with the component.
--
-- 'bindingProperties', 'component_bindingProperties' - The information to connect a component\'s properties to data at runtime.
-- You can\'t specify @tags@ as a valid property for @bindingProperties@.
--
-- 'componentType', 'component_componentType' - The type of the component. This can be an Amplify custom UI component or
-- another custom component.
--
-- 'createdAt', 'component_createdAt' - The time that the component was created.
--
-- 'environmentName', 'component_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'component_id' - The unique ID of the component.
--
-- 'name', 'component_name' - The name of the component.
--
-- 'overrides', 'component_overrides' - Describes the component\'s properties that can be overriden in a
-- customized instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
--
-- 'properties', 'component_properties' - Describes the component\'s properties. You can\'t specify @tags@ as a
-- valid property for @properties@.
--
-- 'variants', 'component_variants' - A list of the component\'s variants. A variant is a unique style
-- configuration of a main component.
newComponent ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'componentType'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Component
newComponent
  pAppId_
  pComponentType_
  pCreatedAt_
  pEnvironmentName_
  pId_
  pName_ =
    Component'
      { tags = Prelude.Nothing,
        modifiedAt = Prelude.Nothing,
        sourceId = Prelude.Nothing,
        children = Prelude.Nothing,
        events = Prelude.Nothing,
        schemaVersion = Prelude.Nothing,
        collectionProperties = Prelude.Nothing,
        appId = pAppId_,
        bindingProperties = Prelude.mempty,
        componentType = pComponentType_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        environmentName = pEnvironmentName_,
        id = pId_,
        name = pName_,
        overrides = Prelude.mempty,
        properties = Prelude.mempty,
        variants = Prelude.mempty
      }

-- | One or more key-value pairs to use when tagging the component.
component_tags :: Lens.Lens' Component (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
component_tags = Lens.lens (\Component' {tags} -> tags) (\s@Component' {} a -> s {tags = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | The time that the component was modified.
component_modifiedAt :: Lens.Lens' Component (Prelude.Maybe Prelude.UTCTime)
component_modifiedAt = Lens.lens (\Component' {modifiedAt} -> modifiedAt) (\s@Component' {} a -> s {modifiedAt = a} :: Component) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the component in its original source system, such as
-- Figma.
component_sourceId :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_sourceId = Lens.lens (\Component' {sourceId} -> sourceId) (\s@Component' {} a -> s {sourceId = a} :: Component)

-- | A list of the component\'s @ComponentChild@ instances.
component_children :: Lens.Lens' Component (Prelude.Maybe [ComponentChild])
component_children = Lens.lens (\Component' {children} -> children) (\s@Component' {} a -> s {children = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | Describes the events that can be raised on the component. Use for the
-- workflow feature in Amplify Studio that allows you to bind events and
-- actions to components.
component_events :: Lens.Lens' Component (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent))
component_events = Lens.lens (\Component' {events} -> events) (\s@Component' {} a -> s {events = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | The schema version of the component when it was imported.
component_schemaVersion :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_schemaVersion = Lens.lens (\Component' {schemaVersion} -> schemaVersion) (\s@Component' {} a -> s {schemaVersion = a} :: Component)

-- | The data binding configuration for the component\'s properties. Use this
-- for a collection component. You can\'t specify @tags@ as a valid
-- property for @collectionProperties@.
component_collectionProperties :: Lens.Lens' Component (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentDataConfiguration))
component_collectionProperties = Lens.lens (\Component' {collectionProperties} -> collectionProperties) (\s@Component' {} a -> s {collectionProperties = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the Amplify app associated with the component.
component_appId :: Lens.Lens' Component Prelude.Text
component_appId = Lens.lens (\Component' {appId} -> appId) (\s@Component' {} a -> s {appId = a} :: Component)

-- | The information to connect a component\'s properties to data at runtime.
-- You can\'t specify @tags@ as a valid property for @bindingProperties@.
component_bindingProperties :: Lens.Lens' Component (Prelude.HashMap Prelude.Text ComponentBindingPropertiesValue)
component_bindingProperties = Lens.lens (\Component' {bindingProperties} -> bindingProperties) (\s@Component' {} a -> s {bindingProperties = a} :: Component) Prelude.. Lens.coerced

-- | The type of the component. This can be an Amplify custom UI component or
-- another custom component.
component_componentType :: Lens.Lens' Component Prelude.Text
component_componentType = Lens.lens (\Component' {componentType} -> componentType) (\s@Component' {} a -> s {componentType = a} :: Component)

-- | The time that the component was created.
component_createdAt :: Lens.Lens' Component Prelude.UTCTime
component_createdAt = Lens.lens (\Component' {createdAt} -> createdAt) (\s@Component' {} a -> s {createdAt = a} :: Component) Prelude.. Data._Time

-- | The name of the backend environment that is a part of the Amplify app.
component_environmentName :: Lens.Lens' Component Prelude.Text
component_environmentName = Lens.lens (\Component' {environmentName} -> environmentName) (\s@Component' {} a -> s {environmentName = a} :: Component)

-- | The unique ID of the component.
component_id :: Lens.Lens' Component Prelude.Text
component_id = Lens.lens (\Component' {id} -> id) (\s@Component' {} a -> s {id = a} :: Component)

-- | The name of the component.
component_name :: Lens.Lens' Component Prelude.Text
component_name = Lens.lens (\Component' {name} -> name) (\s@Component' {} a -> s {name = a} :: Component)

-- | Describes the component\'s properties that can be overriden in a
-- customized instance of the component. You can\'t specify @tags@ as a
-- valid property for @overrides@.
component_overrides :: Lens.Lens' Component (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text))
component_overrides = Lens.lens (\Component' {overrides} -> overrides) (\s@Component' {} a -> s {overrides = a} :: Component) Prelude.. Lens.coerced

-- | Describes the component\'s properties. You can\'t specify @tags@ as a
-- valid property for @properties@.
component_properties :: Lens.Lens' Component (Prelude.HashMap Prelude.Text ComponentProperty)
component_properties = Lens.lens (\Component' {properties} -> properties) (\s@Component' {} a -> s {properties = a} :: Component) Prelude.. Lens.coerced

-- | A list of the component\'s variants. A variant is a unique style
-- configuration of a main component.
component_variants :: Lens.Lens' Component [ComponentVariant]
component_variants = Lens.lens (\Component' {variants} -> variants) (\s@Component' {} a -> s {variants = a} :: Component) Prelude.. Lens.coerced

instance Data.FromJSON Component where
  parseJSON =
    Data.withObject
      "Component"
      ( \x ->
          Component'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..:? "sourceId")
            Prelude.<*> (x Data..:? "children" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "events" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "schemaVersion")
            Prelude.<*> ( x Data..:? "collectionProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "appId")
            Prelude.<*> ( x Data..:? "bindingProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "componentType")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..:? "overrides" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "variants" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Component where
  hashWithSalt _salt Component' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` collectionProperties
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` variants

instance Prelude.NFData Component where
  rnf Component' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf children
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf collectionProperties
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf variants
