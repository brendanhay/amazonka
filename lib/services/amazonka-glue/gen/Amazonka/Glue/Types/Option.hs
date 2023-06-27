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
-- Module      : Amazonka.Glue.Types.Option
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Option where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an option value.
--
-- /See:/ 'newOption' smart constructor.
data Option = Option'
  { -- | Specifies the description of the option.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the label of the option.
    label :: Prelude.Maybe Prelude.Text,
    -- | Specifies the value of the option.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Option' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'option_description' - Specifies the description of the option.
--
-- 'label', 'option_label' - Specifies the label of the option.
--
-- 'value', 'option_value' - Specifies the value of the option.
newOption ::
  Option
newOption =
  Option'
    { description = Prelude.Nothing,
      label = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specifies the description of the option.
option_description :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_description = Lens.lens (\Option' {description} -> description) (\s@Option' {} a -> s {description = a} :: Option)

-- | Specifies the label of the option.
option_label :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_label = Lens.lens (\Option' {label} -> label) (\s@Option' {} a -> s {label = a} :: Option)

-- | Specifies the value of the option.
option_value :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_value = Lens.lens (\Option' {value} -> value) (\s@Option' {} a -> s {value = a} :: Option)

instance Data.FromJSON Option where
  parseJSON =
    Data.withObject
      "Option"
      ( \x ->
          Option'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Option where
  hashWithSalt _salt Option' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` value

instance Prelude.NFData Option where
  rnf Option' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Option where
  toJSON Option' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Label" Data..=) Prelude.<$> label,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
