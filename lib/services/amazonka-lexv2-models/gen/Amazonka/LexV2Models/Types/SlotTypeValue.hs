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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.SampleValue
import qualified Amazonka.Prelude as Prelude

-- | Each slot type can have a set of values. Each @SlotTypeValue@ represents
-- a value that the slot type can take.
--
-- /See:/ 'newSlotTypeValue' smart constructor.
data SlotTypeValue = SlotTypeValue'
  { -- | The value of the slot type entry.
    sampleValue :: Prelude.Maybe SampleValue,
    -- | Additional values related to the slot type entry.
    synonyms :: Prelude.Maybe (Prelude.NonEmpty SampleValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleValue', 'slotTypeValue_sampleValue' - The value of the slot type entry.
--
-- 'synonyms', 'slotTypeValue_synonyms' - Additional values related to the slot type entry.
newSlotTypeValue ::
  SlotTypeValue
newSlotTypeValue =
  SlotTypeValue'
    { sampleValue = Prelude.Nothing,
      synonyms = Prelude.Nothing
    }

-- | The value of the slot type entry.
slotTypeValue_sampleValue :: Lens.Lens' SlotTypeValue (Prelude.Maybe SampleValue)
slotTypeValue_sampleValue = Lens.lens (\SlotTypeValue' {sampleValue} -> sampleValue) (\s@SlotTypeValue' {} a -> s {sampleValue = a} :: SlotTypeValue)

-- | Additional values related to the slot type entry.
slotTypeValue_synonyms :: Lens.Lens' SlotTypeValue (Prelude.Maybe (Prelude.NonEmpty SampleValue))
slotTypeValue_synonyms = Lens.lens (\SlotTypeValue' {synonyms} -> synonyms) (\s@SlotTypeValue' {} a -> s {synonyms = a} :: SlotTypeValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SlotTypeValue where
  parseJSON =
    Data.withObject
      "SlotTypeValue"
      ( \x ->
          SlotTypeValue'
            Prelude.<$> (x Data..:? "sampleValue")
            Prelude.<*> (x Data..:? "synonyms")
      )

instance Prelude.Hashable SlotTypeValue where
  hashWithSalt _salt SlotTypeValue' {..} =
    _salt `Prelude.hashWithSalt` sampleValue
      `Prelude.hashWithSalt` synonyms

instance Prelude.NFData SlotTypeValue where
  rnf SlotTypeValue' {..} =
    Prelude.rnf sampleValue
      `Prelude.seq` Prelude.rnf synonyms

instance Data.ToJSON SlotTypeValue where
  toJSON SlotTypeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sampleValue" Data..=) Prelude.<$> sampleValue,
            ("synonyms" Data..=) Prelude.<$> synonyms
          ]
      )
