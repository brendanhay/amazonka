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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentProperty where

import {-# SOURCE #-} Amazonka.AmplifyUiBuilder.Types.ComponentConditionProperty
import Amazonka.AmplifyUiBuilder.Types.ComponentPropertyBindingProperties
import Amazonka.AmplifyUiBuilder.Types.FormBindingElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for all of a component\'s properties. Use
-- @ComponentProperty@ to specify the values to render or bind by default.
--
-- /See:/ 'newComponentProperty' smart constructor.
data ComponentProperty = ComponentProperty'
  { -- | The information to bind the component property to data at runtime.
    bindingProperties :: Prelude.Maybe ComponentPropertyBindingProperties,
    -- | The information to bind the component property to form data.
    bindings :: Prelude.Maybe (Prelude.HashMap Prelude.Text FormBindingElement),
    -- | The information to bind the component property to data at runtime. Use
    -- this for collection components.
    collectionBindingProperties :: Prelude.Maybe ComponentPropertyBindingProperties,
    -- | The name of the component that is affected by an event.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | A list of component properties to concatenate to create the value to
    -- assign to this component property.
    concat :: Prelude.Maybe [ComponentProperty],
    -- | The conditional expression to use to assign a value to the component
    -- property.
    condition :: Prelude.Maybe ComponentConditionProperty,
    -- | Specifies whether the user configured the property in Amplify Studio
    -- after importing it.
    configured :: Prelude.Maybe Prelude.Bool,
    -- | The default value to assign to the component property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | An event that occurs in your app. Use this for workflow data binding.
    event :: Prelude.Maybe Prelude.Text,
    -- | The default value assigned to the property when the component is
    -- imported into an app.
    importedValue :: Prelude.Maybe Prelude.Text,
    -- | The data model to use to assign a value to the component property.
    model :: Prelude.Maybe Prelude.Text,
    -- | The name of the component\'s property that is affected by an event.
    property :: Prelude.Maybe Prelude.Text,
    -- | The component type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | An authenticated user attribute to use to assign a value to the
    -- component property.
    userAttribute :: Prelude.Maybe Prelude.Text,
    -- | The value to assign to the component property.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bindingProperties', 'componentProperty_bindingProperties' - The information to bind the component property to data at runtime.
--
-- 'bindings', 'componentProperty_bindings' - The information to bind the component property to form data.
--
-- 'collectionBindingProperties', 'componentProperty_collectionBindingProperties' - The information to bind the component property to data at runtime. Use
-- this for collection components.
--
-- 'componentName', 'componentProperty_componentName' - The name of the component that is affected by an event.
--
-- 'concat', 'componentProperty_concat' - A list of component properties to concatenate to create the value to
-- assign to this component property.
--
-- 'condition', 'componentProperty_condition' - The conditional expression to use to assign a value to the component
-- property.
--
-- 'configured', 'componentProperty_configured' - Specifies whether the user configured the property in Amplify Studio
-- after importing it.
--
-- 'defaultValue', 'componentProperty_defaultValue' - The default value to assign to the component property.
--
-- 'event', 'componentProperty_event' - An event that occurs in your app. Use this for workflow data binding.
--
-- 'importedValue', 'componentProperty_importedValue' - The default value assigned to the property when the component is
-- imported into an app.
--
-- 'model', 'componentProperty_model' - The data model to use to assign a value to the component property.
--
-- 'property', 'componentProperty_property' - The name of the component\'s property that is affected by an event.
--
-- 'type'', 'componentProperty_type' - The component type.
--
-- 'userAttribute', 'componentProperty_userAttribute' - An authenticated user attribute to use to assign a value to the
-- component property.
--
-- 'value', 'componentProperty_value' - The value to assign to the component property.
newComponentProperty ::
  ComponentProperty
newComponentProperty =
  ComponentProperty'
    { bindingProperties =
        Prelude.Nothing,
      bindings = Prelude.Nothing,
      collectionBindingProperties = Prelude.Nothing,
      componentName = Prelude.Nothing,
      concat = Prelude.Nothing,
      condition = Prelude.Nothing,
      configured = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      event = Prelude.Nothing,
      importedValue = Prelude.Nothing,
      model = Prelude.Nothing,
      property = Prelude.Nothing,
      type' = Prelude.Nothing,
      userAttribute = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The information to bind the component property to data at runtime.
componentProperty_bindingProperties :: Lens.Lens' ComponentProperty (Prelude.Maybe ComponentPropertyBindingProperties)
componentProperty_bindingProperties = Lens.lens (\ComponentProperty' {bindingProperties} -> bindingProperties) (\s@ComponentProperty' {} a -> s {bindingProperties = a} :: ComponentProperty)

-- | The information to bind the component property to form data.
componentProperty_bindings :: Lens.Lens' ComponentProperty (Prelude.Maybe (Prelude.HashMap Prelude.Text FormBindingElement))
componentProperty_bindings = Lens.lens (\ComponentProperty' {bindings} -> bindings) (\s@ComponentProperty' {} a -> s {bindings = a} :: ComponentProperty) Prelude.. Lens.mapping Lens.coerced

-- | The information to bind the component property to data at runtime. Use
-- this for collection components.
componentProperty_collectionBindingProperties :: Lens.Lens' ComponentProperty (Prelude.Maybe ComponentPropertyBindingProperties)
componentProperty_collectionBindingProperties = Lens.lens (\ComponentProperty' {collectionBindingProperties} -> collectionBindingProperties) (\s@ComponentProperty' {} a -> s {collectionBindingProperties = a} :: ComponentProperty)

-- | The name of the component that is affected by an event.
componentProperty_componentName :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_componentName = Lens.lens (\ComponentProperty' {componentName} -> componentName) (\s@ComponentProperty' {} a -> s {componentName = a} :: ComponentProperty)

-- | A list of component properties to concatenate to create the value to
-- assign to this component property.
componentProperty_concat :: Lens.Lens' ComponentProperty (Prelude.Maybe [ComponentProperty])
componentProperty_concat = Lens.lens (\ComponentProperty' {concat} -> concat) (\s@ComponentProperty' {} a -> s {concat = a} :: ComponentProperty) Prelude.. Lens.mapping Lens.coerced

-- | The conditional expression to use to assign a value to the component
-- property.
componentProperty_condition :: Lens.Lens' ComponentProperty (Prelude.Maybe ComponentConditionProperty)
componentProperty_condition = Lens.lens (\ComponentProperty' {condition} -> condition) (\s@ComponentProperty' {} a -> s {condition = a} :: ComponentProperty)

-- | Specifies whether the user configured the property in Amplify Studio
-- after importing it.
componentProperty_configured :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Bool)
componentProperty_configured = Lens.lens (\ComponentProperty' {configured} -> configured) (\s@ComponentProperty' {} a -> s {configured = a} :: ComponentProperty)

-- | The default value to assign to the component property.
componentProperty_defaultValue :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_defaultValue = Lens.lens (\ComponentProperty' {defaultValue} -> defaultValue) (\s@ComponentProperty' {} a -> s {defaultValue = a} :: ComponentProperty)

-- | An event that occurs in your app. Use this for workflow data binding.
componentProperty_event :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_event = Lens.lens (\ComponentProperty' {event} -> event) (\s@ComponentProperty' {} a -> s {event = a} :: ComponentProperty)

-- | The default value assigned to the property when the component is
-- imported into an app.
componentProperty_importedValue :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_importedValue = Lens.lens (\ComponentProperty' {importedValue} -> importedValue) (\s@ComponentProperty' {} a -> s {importedValue = a} :: ComponentProperty)

-- | The data model to use to assign a value to the component property.
componentProperty_model :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_model = Lens.lens (\ComponentProperty' {model} -> model) (\s@ComponentProperty' {} a -> s {model = a} :: ComponentProperty)

-- | The name of the component\'s property that is affected by an event.
componentProperty_property :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_property = Lens.lens (\ComponentProperty' {property} -> property) (\s@ComponentProperty' {} a -> s {property = a} :: ComponentProperty)

-- | The component type.
componentProperty_type :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_type = Lens.lens (\ComponentProperty' {type'} -> type') (\s@ComponentProperty' {} a -> s {type' = a} :: ComponentProperty)

-- | An authenticated user attribute to use to assign a value to the
-- component property.
componentProperty_userAttribute :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_userAttribute = Lens.lens (\ComponentProperty' {userAttribute} -> userAttribute) (\s@ComponentProperty' {} a -> s {userAttribute = a} :: ComponentProperty)

-- | The value to assign to the component property.
componentProperty_value :: Lens.Lens' ComponentProperty (Prelude.Maybe Prelude.Text)
componentProperty_value = Lens.lens (\ComponentProperty' {value} -> value) (\s@ComponentProperty' {} a -> s {value = a} :: ComponentProperty)

instance Data.FromJSON ComponentProperty where
  parseJSON =
    Data.withObject
      "ComponentProperty"
      ( \x ->
          ComponentProperty'
            Prelude.<$> (x Data..:? "bindingProperties")
            Prelude.<*> (x Data..:? "bindings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "collectionBindingProperties")
            Prelude.<*> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "concat" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "condition")
            Prelude.<*> (x Data..:? "configured")
            Prelude.<*> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "event")
            Prelude.<*> (x Data..:? "importedValue")
            Prelude.<*> (x Data..:? "model")
            Prelude.<*> (x Data..:? "property")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "userAttribute")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ComponentProperty where
  hashWithSalt _salt ComponentProperty' {..} =
    _salt
      `Prelude.hashWithSalt` bindingProperties
      `Prelude.hashWithSalt` bindings
      `Prelude.hashWithSalt` collectionBindingProperties
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` concat
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` configured
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` importedValue
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` property
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` userAttribute
      `Prelude.hashWithSalt` value

instance Prelude.NFData ComponentProperty where
  rnf ComponentProperty' {..} =
    Prelude.rnf bindingProperties
      `Prelude.seq` Prelude.rnf bindings
      `Prelude.seq` Prelude.rnf collectionBindingProperties
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf concat
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf configured
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf importedValue
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf property
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf userAttribute
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ComponentProperty where
  toJSON ComponentProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bindingProperties" Data..=)
              Prelude.<$> bindingProperties,
            ("bindings" Data..=) Prelude.<$> bindings,
            ("collectionBindingProperties" Data..=)
              Prelude.<$> collectionBindingProperties,
            ("componentName" Data..=) Prelude.<$> componentName,
            ("concat" Data..=) Prelude.<$> concat,
            ("condition" Data..=) Prelude.<$> condition,
            ("configured" Data..=) Prelude.<$> configured,
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("event" Data..=) Prelude.<$> event,
            ("importedValue" Data..=) Prelude.<$> importedValue,
            ("model" Data..=) Prelude.<$> model,
            ("property" Data..=) Prelude.<$> property,
            ("type" Data..=) Prelude.<$> type',
            ("userAttribute" Data..=) Prelude.<$> userAttribute,
            ("value" Data..=) Prelude.<$> value
          ]
      )
