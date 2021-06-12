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
-- Module      : Network.AWS.LexModels.Types.EnumerationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.EnumerationValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Each slot type can have a set of values. Each enumeration value
-- represents a value the slot type can take.
--
-- For example, a pizza ordering bot could have a slot type that specifies
-- the type of crust that the pizza should have. The slot type could
-- include the values
--
-- -   thick
--
-- -   thin
--
-- -   stuffed
--
-- /See:/ 'newEnumerationValue' smart constructor.
data EnumerationValue = EnumerationValue'
  { -- | Additional values related to the slot type value.
    synonyms :: Core.Maybe [Core.Text],
    -- | The value of the slot type.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnumerationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'synonyms', 'enumerationValue_synonyms' - Additional values related to the slot type value.
--
-- 'value', 'enumerationValue_value' - The value of the slot type.
newEnumerationValue ::
  -- | 'value'
  Core.Text ->
  EnumerationValue
newEnumerationValue pValue_ =
  EnumerationValue'
    { synonyms = Core.Nothing,
      value = pValue_
    }

-- | Additional values related to the slot type value.
enumerationValue_synonyms :: Lens.Lens' EnumerationValue (Core.Maybe [Core.Text])
enumerationValue_synonyms = Lens.lens (\EnumerationValue' {synonyms} -> synonyms) (\s@EnumerationValue' {} a -> s {synonyms = a} :: EnumerationValue) Core.. Lens.mapping Lens._Coerce

-- | The value of the slot type.
enumerationValue_value :: Lens.Lens' EnumerationValue Core.Text
enumerationValue_value = Lens.lens (\EnumerationValue' {value} -> value) (\s@EnumerationValue' {} a -> s {value = a} :: EnumerationValue)

instance Core.FromJSON EnumerationValue where
  parseJSON =
    Core.withObject
      "EnumerationValue"
      ( \x ->
          EnumerationValue'
            Core.<$> (x Core..:? "synonyms" Core..!= Core.mempty)
            Core.<*> (x Core..: "value")
      )

instance Core.Hashable EnumerationValue

instance Core.NFData EnumerationValue

instance Core.ToJSON EnumerationValue where
  toJSON EnumerationValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("synonyms" Core..=) Core.<$> synonyms,
            Core.Just ("value" Core..= value)
          ]
      )
