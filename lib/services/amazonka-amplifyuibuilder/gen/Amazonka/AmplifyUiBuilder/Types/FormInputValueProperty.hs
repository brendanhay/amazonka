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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for an input field on a form. Use
-- @FormInputValueProperty@ to specify the values to render or bind by
-- default.
--
-- /See:/ 'newFormInputValueProperty' smart constructor.
data FormInputValueProperty = FormInputValueProperty'
  { -- | The value to assign to the input field.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormInputValueProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'formInputValueProperty_value' - The value to assign to the input field.
newFormInputValueProperty ::
  FormInputValueProperty
newFormInputValueProperty =
  FormInputValueProperty' {value = Prelude.Nothing}

-- | The value to assign to the input field.
formInputValueProperty_value :: Lens.Lens' FormInputValueProperty (Prelude.Maybe Prelude.Text)
formInputValueProperty_value = Lens.lens (\FormInputValueProperty' {value} -> value) (\s@FormInputValueProperty' {} a -> s {value = a} :: FormInputValueProperty)

instance Core.FromJSON FormInputValueProperty where
  parseJSON =
    Core.withObject
      "FormInputValueProperty"
      ( \x ->
          FormInputValueProperty'
            Prelude.<$> (x Core..:? "value")
      )

instance Prelude.Hashable FormInputValueProperty where
  hashWithSalt _salt FormInputValueProperty' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData FormInputValueProperty where
  rnf FormInputValueProperty' {..} = Prelude.rnf value

instance Core.ToJSON FormInputValueProperty where
  toJSON FormInputValueProperty' {..} =
    Core.object
      ( Prelude.catMaybes
          [("value" Core..=) Prelude.<$> value]
      )
