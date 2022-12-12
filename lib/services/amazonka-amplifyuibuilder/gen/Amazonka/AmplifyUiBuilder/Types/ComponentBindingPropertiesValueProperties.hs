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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValueProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValueProperties where

import Amazonka.AmplifyUiBuilder.Types.Predicate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a specific property using
-- data stored in Amazon Web Services. For Amazon Web Services connected
-- properties, you can bind a property to data stored in an Amazon S3
-- bucket, an Amplify DataStore model or an authenticated user attribute.
--
-- /See:/ 'newComponentBindingPropertiesValueProperties' smart constructor.
data ComponentBindingPropertiesValueProperties = ComponentBindingPropertiesValueProperties'
  { -- | An Amazon S3 bucket.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | The default value to assign to the property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The field to bind the data to.
    field :: Prelude.Maybe Prelude.Text,
    -- | The storage key for an Amazon S3 bucket.
    key :: Prelude.Maybe Prelude.Text,
    -- | An Amplify DataStore model.
    model :: Prelude.Maybe Prelude.Text,
    -- | A list of predicates for binding a component\'s properties to data.
    predicates :: Prelude.Maybe [Predicate],
    -- | The name of a component slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | An authenticated user attribute.
    userAttribute :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentBindingPropertiesValueProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'componentBindingPropertiesValueProperties_bucket' - An Amazon S3 bucket.
--
-- 'defaultValue', 'componentBindingPropertiesValueProperties_defaultValue' - The default value to assign to the property.
--
-- 'field', 'componentBindingPropertiesValueProperties_field' - The field to bind the data to.
--
-- 'key', 'componentBindingPropertiesValueProperties_key' - The storage key for an Amazon S3 bucket.
--
-- 'model', 'componentBindingPropertiesValueProperties_model' - An Amplify DataStore model.
--
-- 'predicates', 'componentBindingPropertiesValueProperties_predicates' - A list of predicates for binding a component\'s properties to data.
--
-- 'slotName', 'componentBindingPropertiesValueProperties_slotName' - The name of a component slot.
--
-- 'userAttribute', 'componentBindingPropertiesValueProperties_userAttribute' - An authenticated user attribute.
newComponentBindingPropertiesValueProperties ::
  ComponentBindingPropertiesValueProperties
newComponentBindingPropertiesValueProperties =
  ComponentBindingPropertiesValueProperties'
    { bucket =
        Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      field = Prelude.Nothing,
      key = Prelude.Nothing,
      model = Prelude.Nothing,
      predicates = Prelude.Nothing,
      slotName = Prelude.Nothing,
      userAttribute = Prelude.Nothing
    }

-- | An Amazon S3 bucket.
componentBindingPropertiesValueProperties_bucket :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_bucket = Lens.lens (\ComponentBindingPropertiesValueProperties' {bucket} -> bucket) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {bucket = a} :: ComponentBindingPropertiesValueProperties)

-- | The default value to assign to the property.
componentBindingPropertiesValueProperties_defaultValue :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_defaultValue = Lens.lens (\ComponentBindingPropertiesValueProperties' {defaultValue} -> defaultValue) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {defaultValue = a} :: ComponentBindingPropertiesValueProperties)

-- | The field to bind the data to.
componentBindingPropertiesValueProperties_field :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_field = Lens.lens (\ComponentBindingPropertiesValueProperties' {field} -> field) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {field = a} :: ComponentBindingPropertiesValueProperties)

-- | The storage key for an Amazon S3 bucket.
componentBindingPropertiesValueProperties_key :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_key = Lens.lens (\ComponentBindingPropertiesValueProperties' {key} -> key) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {key = a} :: ComponentBindingPropertiesValueProperties)

-- | An Amplify DataStore model.
componentBindingPropertiesValueProperties_model :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_model = Lens.lens (\ComponentBindingPropertiesValueProperties' {model} -> model) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {model = a} :: ComponentBindingPropertiesValueProperties)

-- | A list of predicates for binding a component\'s properties to data.
componentBindingPropertiesValueProperties_predicates :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe [Predicate])
componentBindingPropertiesValueProperties_predicates = Lens.lens (\ComponentBindingPropertiesValueProperties' {predicates} -> predicates) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {predicates = a} :: ComponentBindingPropertiesValueProperties) Prelude.. Lens.mapping Lens.coerced

-- | The name of a component slot.
componentBindingPropertiesValueProperties_slotName :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_slotName = Lens.lens (\ComponentBindingPropertiesValueProperties' {slotName} -> slotName) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {slotName = a} :: ComponentBindingPropertiesValueProperties)

-- | An authenticated user attribute.
componentBindingPropertiesValueProperties_userAttribute :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_userAttribute = Lens.lens (\ComponentBindingPropertiesValueProperties' {userAttribute} -> userAttribute) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {userAttribute = a} :: ComponentBindingPropertiesValueProperties)

instance
  Data.FromJSON
    ComponentBindingPropertiesValueProperties
  where
  parseJSON =
    Data.withObject
      "ComponentBindingPropertiesValueProperties"
      ( \x ->
          ComponentBindingPropertiesValueProperties'
            Prelude.<$> (x Data..:? "bucket")
              Prelude.<*> (x Data..:? "defaultValue")
              Prelude.<*> (x Data..:? "field")
              Prelude.<*> (x Data..:? "key")
              Prelude.<*> (x Data..:? "model")
              Prelude.<*> (x Data..:? "predicates" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "slotName")
              Prelude.<*> (x Data..:? "userAttribute")
      )

instance
  Prelude.Hashable
    ComponentBindingPropertiesValueProperties
  where
  hashWithSalt
    _salt
    ComponentBindingPropertiesValueProperties' {..} =
      _salt `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` field
        `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` model
        `Prelude.hashWithSalt` predicates
        `Prelude.hashWithSalt` slotName
        `Prelude.hashWithSalt` userAttribute

instance
  Prelude.NFData
    ComponentBindingPropertiesValueProperties
  where
  rnf ComponentBindingPropertiesValueProperties' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf field
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf predicates
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf userAttribute

instance
  Data.ToJSON
    ComponentBindingPropertiesValueProperties
  where
  toJSON ComponentBindingPropertiesValueProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucket" Data..=) Prelude.<$> bucket,
            ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("field" Data..=) Prelude.<$> field,
            ("key" Data..=) Prelude.<$> key,
            ("model" Data..=) Prelude.<$> model,
            ("predicates" Data..=) Prelude.<$> predicates,
            ("slotName" Data..=) Prelude.<$> slotName,
            ("userAttribute" Data..=) Prelude.<$> userAttribute
          ]
      )
