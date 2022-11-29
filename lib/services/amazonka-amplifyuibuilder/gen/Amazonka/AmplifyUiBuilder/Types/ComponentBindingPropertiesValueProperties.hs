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
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a specific property using
-- data stored in Amazon Web Services. For Amazon Web Services connected
-- properties, you can bind a property to data stored in an Amazon S3
-- bucket, an Amplify DataStore model or an authenticated user attribute.
--
-- /See:/ 'newComponentBindingPropertiesValueProperties' smart constructor.
data ComponentBindingPropertiesValueProperties = ComponentBindingPropertiesValueProperties'
  { -- | The storage key for an Amazon S3 bucket.
    key :: Prelude.Maybe Prelude.Text,
    -- | The name of a component slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | An Amplify DataStore model.
    model :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 bucket.
    bucket :: Prelude.Maybe Prelude.Text,
    -- | A list of predicates for binding a component\'s properties to data.
    predicates :: Prelude.Maybe [Predicate],
    -- | An authenticated user attribute.
    userAttribute :: Prelude.Maybe Prelude.Text,
    -- | The default value to assign to the property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The field to bind the data to.
    field :: Prelude.Maybe Prelude.Text
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
-- 'key', 'componentBindingPropertiesValueProperties_key' - The storage key for an Amazon S3 bucket.
--
-- 'slotName', 'componentBindingPropertiesValueProperties_slotName' - The name of a component slot.
--
-- 'model', 'componentBindingPropertiesValueProperties_model' - An Amplify DataStore model.
--
-- 'bucket', 'componentBindingPropertiesValueProperties_bucket' - An Amazon S3 bucket.
--
-- 'predicates', 'componentBindingPropertiesValueProperties_predicates' - A list of predicates for binding a component\'s properties to data.
--
-- 'userAttribute', 'componentBindingPropertiesValueProperties_userAttribute' - An authenticated user attribute.
--
-- 'defaultValue', 'componentBindingPropertiesValueProperties_defaultValue' - The default value to assign to the property.
--
-- 'field', 'componentBindingPropertiesValueProperties_field' - The field to bind the data to.
newComponentBindingPropertiesValueProperties ::
  ComponentBindingPropertiesValueProperties
newComponentBindingPropertiesValueProperties =
  ComponentBindingPropertiesValueProperties'
    { key =
        Prelude.Nothing,
      slotName = Prelude.Nothing,
      model = Prelude.Nothing,
      bucket = Prelude.Nothing,
      predicates = Prelude.Nothing,
      userAttribute = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      field = Prelude.Nothing
    }

-- | The storage key for an Amazon S3 bucket.
componentBindingPropertiesValueProperties_key :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_key = Lens.lens (\ComponentBindingPropertiesValueProperties' {key} -> key) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {key = a} :: ComponentBindingPropertiesValueProperties)

-- | The name of a component slot.
componentBindingPropertiesValueProperties_slotName :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_slotName = Lens.lens (\ComponentBindingPropertiesValueProperties' {slotName} -> slotName) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {slotName = a} :: ComponentBindingPropertiesValueProperties)

-- | An Amplify DataStore model.
componentBindingPropertiesValueProperties_model :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_model = Lens.lens (\ComponentBindingPropertiesValueProperties' {model} -> model) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {model = a} :: ComponentBindingPropertiesValueProperties)

-- | An Amazon S3 bucket.
componentBindingPropertiesValueProperties_bucket :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_bucket = Lens.lens (\ComponentBindingPropertiesValueProperties' {bucket} -> bucket) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {bucket = a} :: ComponentBindingPropertiesValueProperties)

-- | A list of predicates for binding a component\'s properties to data.
componentBindingPropertiesValueProperties_predicates :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe [Predicate])
componentBindingPropertiesValueProperties_predicates = Lens.lens (\ComponentBindingPropertiesValueProperties' {predicates} -> predicates) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {predicates = a} :: ComponentBindingPropertiesValueProperties) Prelude.. Lens.mapping Lens.coerced

-- | An authenticated user attribute.
componentBindingPropertiesValueProperties_userAttribute :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_userAttribute = Lens.lens (\ComponentBindingPropertiesValueProperties' {userAttribute} -> userAttribute) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {userAttribute = a} :: ComponentBindingPropertiesValueProperties)

-- | The default value to assign to the property.
componentBindingPropertiesValueProperties_defaultValue :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_defaultValue = Lens.lens (\ComponentBindingPropertiesValueProperties' {defaultValue} -> defaultValue) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {defaultValue = a} :: ComponentBindingPropertiesValueProperties)

-- | The field to bind the data to.
componentBindingPropertiesValueProperties_field :: Lens.Lens' ComponentBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
componentBindingPropertiesValueProperties_field = Lens.lens (\ComponentBindingPropertiesValueProperties' {field} -> field) (\s@ComponentBindingPropertiesValueProperties' {} a -> s {field = a} :: ComponentBindingPropertiesValueProperties)

instance
  Core.FromJSON
    ComponentBindingPropertiesValueProperties
  where
  parseJSON =
    Core.withObject
      "ComponentBindingPropertiesValueProperties"
      ( \x ->
          ComponentBindingPropertiesValueProperties'
            Prelude.<$> (x Core..:? "key")
              Prelude.<*> (x Core..:? "slotName")
              Prelude.<*> (x Core..:? "model")
              Prelude.<*> (x Core..:? "bucket")
              Prelude.<*> (x Core..:? "predicates" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "userAttribute")
              Prelude.<*> (x Core..:? "defaultValue")
              Prelude.<*> (x Core..:? "field")
      )

instance
  Prelude.Hashable
    ComponentBindingPropertiesValueProperties
  where
  hashWithSalt
    _salt
    ComponentBindingPropertiesValueProperties' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` slotName
        `Prelude.hashWithSalt` model
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` predicates
        `Prelude.hashWithSalt` userAttribute
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` field

instance
  Prelude.NFData
    ComponentBindingPropertiesValueProperties
  where
  rnf ComponentBindingPropertiesValueProperties' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf predicates
      `Prelude.seq` Prelude.rnf userAttribute
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf field

instance
  Core.ToJSON
    ComponentBindingPropertiesValueProperties
  where
  toJSON ComponentBindingPropertiesValueProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("key" Core..=) Prelude.<$> key,
            ("slotName" Core..=) Prelude.<$> slotName,
            ("model" Core..=) Prelude.<$> model,
            ("bucket" Core..=) Prelude.<$> bucket,
            ("predicates" Core..=) Prelude.<$> predicates,
            ("userAttribute" Core..=) Prelude.<$> userAttribute,
            ("defaultValue" Core..=) Prelude.<$> defaultValue,
            ("field" Core..=) Prelude.<$> field
          ]
      )
