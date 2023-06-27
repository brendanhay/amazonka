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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValueProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormInputBindingPropertiesValueProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a specific property using
-- data stored in Amazon Web Services. For Amazon Web Services connected
-- properties, you can bind a property to data stored in an Amplify
-- DataStore model.
--
-- /See:/ 'newFormInputBindingPropertiesValueProperties' smart constructor.
data FormInputBindingPropertiesValueProperties = FormInputBindingPropertiesValueProperties'
  { -- | An Amplify DataStore model.
    model :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormInputBindingPropertiesValueProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'model', 'formInputBindingPropertiesValueProperties_model' - An Amplify DataStore model.
newFormInputBindingPropertiesValueProperties ::
  FormInputBindingPropertiesValueProperties
newFormInputBindingPropertiesValueProperties =
  FormInputBindingPropertiesValueProperties'
    { model =
        Prelude.Nothing
    }

-- | An Amplify DataStore model.
formInputBindingPropertiesValueProperties_model :: Lens.Lens' FormInputBindingPropertiesValueProperties (Prelude.Maybe Prelude.Text)
formInputBindingPropertiesValueProperties_model = Lens.lens (\FormInputBindingPropertiesValueProperties' {model} -> model) (\s@FormInputBindingPropertiesValueProperties' {} a -> s {model = a} :: FormInputBindingPropertiesValueProperties)

instance
  Data.FromJSON
    FormInputBindingPropertiesValueProperties
  where
  parseJSON =
    Data.withObject
      "FormInputBindingPropertiesValueProperties"
      ( \x ->
          FormInputBindingPropertiesValueProperties'
            Prelude.<$> (x Data..:? "model")
      )

instance
  Prelude.Hashable
    FormInputBindingPropertiesValueProperties
  where
  hashWithSalt
    _salt
    FormInputBindingPropertiesValueProperties' {..} =
      _salt `Prelude.hashWithSalt` model

instance
  Prelude.NFData
    FormInputBindingPropertiesValueProperties
  where
  rnf FormInputBindingPropertiesValueProperties' {..} =
    Prelude.rnf model

instance
  Data.ToJSON
    FormInputBindingPropertiesValueProperties
  where
  toJSON FormInputBindingPropertiesValueProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("model" Data..=) Prelude.<$> model]
      )
