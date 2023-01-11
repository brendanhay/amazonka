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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FieldPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FieldPosition where

import Amazonka.AmplifyUiBuilder.Types.FixedPosition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the field position.
--
-- /See:/ 'newFieldPosition' smart constructor.
data FieldPosition = FieldPosition'
  { -- | The field position is below the field specified by the string.
    below :: Prelude.Maybe Prelude.Text,
    -- | The field position is fixed and doesn\'t change in relation to other
    -- fields.
    fixed :: Prelude.Maybe FixedPosition,
    -- | The field position is to the right of the field specified by the string.
    rightOf :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldPosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'below', 'fieldPosition_below' - The field position is below the field specified by the string.
--
-- 'fixed', 'fieldPosition_fixed' - The field position is fixed and doesn\'t change in relation to other
-- fields.
--
-- 'rightOf', 'fieldPosition_rightOf' - The field position is to the right of the field specified by the string.
newFieldPosition ::
  FieldPosition
newFieldPosition =
  FieldPosition'
    { below = Prelude.Nothing,
      fixed = Prelude.Nothing,
      rightOf = Prelude.Nothing
    }

-- | The field position is below the field specified by the string.
fieldPosition_below :: Lens.Lens' FieldPosition (Prelude.Maybe Prelude.Text)
fieldPosition_below = Lens.lens (\FieldPosition' {below} -> below) (\s@FieldPosition' {} a -> s {below = a} :: FieldPosition)

-- | The field position is fixed and doesn\'t change in relation to other
-- fields.
fieldPosition_fixed :: Lens.Lens' FieldPosition (Prelude.Maybe FixedPosition)
fieldPosition_fixed = Lens.lens (\FieldPosition' {fixed} -> fixed) (\s@FieldPosition' {} a -> s {fixed = a} :: FieldPosition)

-- | The field position is to the right of the field specified by the string.
fieldPosition_rightOf :: Lens.Lens' FieldPosition (Prelude.Maybe Prelude.Text)
fieldPosition_rightOf = Lens.lens (\FieldPosition' {rightOf} -> rightOf) (\s@FieldPosition' {} a -> s {rightOf = a} :: FieldPosition)

instance Data.FromJSON FieldPosition where
  parseJSON =
    Data.withObject
      "FieldPosition"
      ( \x ->
          FieldPosition'
            Prelude.<$> (x Data..:? "below")
            Prelude.<*> (x Data..:? "fixed")
            Prelude.<*> (x Data..:? "rightOf")
      )

instance Prelude.Hashable FieldPosition where
  hashWithSalt _salt FieldPosition' {..} =
    _salt `Prelude.hashWithSalt` below
      `Prelude.hashWithSalt` fixed
      `Prelude.hashWithSalt` rightOf

instance Prelude.NFData FieldPosition where
  rnf FieldPosition' {..} =
    Prelude.rnf below
      `Prelude.seq` Prelude.rnf fixed
      `Prelude.seq` Prelude.rnf rightOf

instance Data.ToJSON FieldPosition where
  toJSON FieldPosition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("below" Data..=) Prelude.<$> below,
            ("fixed" Data..=) Prelude.<$> fixed,
            ("rightOf" Data..=) Prelude.<$> rightOf
          ]
      )
