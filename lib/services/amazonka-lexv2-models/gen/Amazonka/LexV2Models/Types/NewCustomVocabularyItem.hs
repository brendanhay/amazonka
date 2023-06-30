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
-- Module      : Amazonka.LexV2Models.Types.NewCustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.NewCustomVocabularyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The new custom vocabulary item from the custom vocabulary list.
--
-- /See:/ 'newNewCustomVocabularyItem' smart constructor.
data NewCustomVocabularyItem = NewCustomVocabularyItem'
  { -- | The display as value assigned to the new custom vocabulary item from the
    -- custom vocabulary list.
    displayAs :: Prelude.Maybe Prelude.Text,
    -- | The weight assigned to the new custom vocabulary item from the custom
    -- vocabulary list.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The unique phrase for the new custom vocabulary item from the custom
    -- vocabulary list.
    phrase :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewCustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayAs', 'newCustomVocabularyItem_displayAs' - The display as value assigned to the new custom vocabulary item from the
-- custom vocabulary list.
--
-- 'weight', 'newCustomVocabularyItem_weight' - The weight assigned to the new custom vocabulary item from the custom
-- vocabulary list.
--
-- 'phrase', 'newCustomVocabularyItem_phrase' - The unique phrase for the new custom vocabulary item from the custom
-- vocabulary list.
newNewCustomVocabularyItem ::
  -- | 'phrase'
  Prelude.Text ->
  NewCustomVocabularyItem
newNewCustomVocabularyItem pPhrase_ =
  NewCustomVocabularyItem'
    { displayAs =
        Prelude.Nothing,
      weight = Prelude.Nothing,
      phrase = pPhrase_
    }

-- | The display as value assigned to the new custom vocabulary item from the
-- custom vocabulary list.
newCustomVocabularyItem_displayAs :: Lens.Lens' NewCustomVocabularyItem (Prelude.Maybe Prelude.Text)
newCustomVocabularyItem_displayAs = Lens.lens (\NewCustomVocabularyItem' {displayAs} -> displayAs) (\s@NewCustomVocabularyItem' {} a -> s {displayAs = a} :: NewCustomVocabularyItem)

-- | The weight assigned to the new custom vocabulary item from the custom
-- vocabulary list.
newCustomVocabularyItem_weight :: Lens.Lens' NewCustomVocabularyItem (Prelude.Maybe Prelude.Natural)
newCustomVocabularyItem_weight = Lens.lens (\NewCustomVocabularyItem' {weight} -> weight) (\s@NewCustomVocabularyItem' {} a -> s {weight = a} :: NewCustomVocabularyItem)

-- | The unique phrase for the new custom vocabulary item from the custom
-- vocabulary list.
newCustomVocabularyItem_phrase :: Lens.Lens' NewCustomVocabularyItem Prelude.Text
newCustomVocabularyItem_phrase = Lens.lens (\NewCustomVocabularyItem' {phrase} -> phrase) (\s@NewCustomVocabularyItem' {} a -> s {phrase = a} :: NewCustomVocabularyItem)

instance Prelude.Hashable NewCustomVocabularyItem where
  hashWithSalt _salt NewCustomVocabularyItem' {..} =
    _salt
      `Prelude.hashWithSalt` displayAs
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` phrase

instance Prelude.NFData NewCustomVocabularyItem where
  rnf NewCustomVocabularyItem' {..} =
    Prelude.rnf displayAs
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf phrase

instance Data.ToJSON NewCustomVocabularyItem where
  toJSON NewCustomVocabularyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("displayAs" Data..=) Prelude.<$> displayAs,
            ("weight" Data..=) Prelude.<$> weight,
            Prelude.Just ("phrase" Data..= phrase)
          ]
      )
