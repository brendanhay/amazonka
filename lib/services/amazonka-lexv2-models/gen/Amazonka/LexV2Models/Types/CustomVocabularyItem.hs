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
-- Module      : Amazonka.LexV2Models.Types.CustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomVocabularyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The unique custom vocabulary item from the custom vocabulary list.
--
-- /See:/ 'newCustomVocabularyItem' smart constructor.
data CustomVocabularyItem = CustomVocabularyItem'
  { -- | The display as value for the custom vocabulary item from the custom
    -- vocabulary list.
    displayAs :: Prelude.Maybe Prelude.Text,
    -- | The weight assigned for the custom vocabulary item from the custom
    -- vocabulary list.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | The unique item identifer for the custom vocabulary item from the custom
    -- vocabulary list.
    itemId :: Prelude.Text,
    -- | The unique phrase for the custom vocabulary item from the custom
    -- vocabulary list.
    phrase :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayAs', 'customVocabularyItem_displayAs' - The display as value for the custom vocabulary item from the custom
-- vocabulary list.
--
-- 'weight', 'customVocabularyItem_weight' - The weight assigned for the custom vocabulary item from the custom
-- vocabulary list.
--
-- 'itemId', 'customVocabularyItem_itemId' - The unique item identifer for the custom vocabulary item from the custom
-- vocabulary list.
--
-- 'phrase', 'customVocabularyItem_phrase' - The unique phrase for the custom vocabulary item from the custom
-- vocabulary list.
newCustomVocabularyItem ::
  -- | 'itemId'
  Prelude.Text ->
  -- | 'phrase'
  Prelude.Text ->
  CustomVocabularyItem
newCustomVocabularyItem pItemId_ pPhrase_ =
  CustomVocabularyItem'
    { displayAs = Prelude.Nothing,
      weight = Prelude.Nothing,
      itemId = pItemId_,
      phrase = pPhrase_
    }

-- | The display as value for the custom vocabulary item from the custom
-- vocabulary list.
customVocabularyItem_displayAs :: Lens.Lens' CustomVocabularyItem (Prelude.Maybe Prelude.Text)
customVocabularyItem_displayAs = Lens.lens (\CustomVocabularyItem' {displayAs} -> displayAs) (\s@CustomVocabularyItem' {} a -> s {displayAs = a} :: CustomVocabularyItem)

-- | The weight assigned for the custom vocabulary item from the custom
-- vocabulary list.
customVocabularyItem_weight :: Lens.Lens' CustomVocabularyItem (Prelude.Maybe Prelude.Natural)
customVocabularyItem_weight = Lens.lens (\CustomVocabularyItem' {weight} -> weight) (\s@CustomVocabularyItem' {} a -> s {weight = a} :: CustomVocabularyItem)

-- | The unique item identifer for the custom vocabulary item from the custom
-- vocabulary list.
customVocabularyItem_itemId :: Lens.Lens' CustomVocabularyItem Prelude.Text
customVocabularyItem_itemId = Lens.lens (\CustomVocabularyItem' {itemId} -> itemId) (\s@CustomVocabularyItem' {} a -> s {itemId = a} :: CustomVocabularyItem)

-- | The unique phrase for the custom vocabulary item from the custom
-- vocabulary list.
customVocabularyItem_phrase :: Lens.Lens' CustomVocabularyItem Prelude.Text
customVocabularyItem_phrase = Lens.lens (\CustomVocabularyItem' {phrase} -> phrase) (\s@CustomVocabularyItem' {} a -> s {phrase = a} :: CustomVocabularyItem)

instance Data.FromJSON CustomVocabularyItem where
  parseJSON =
    Data.withObject
      "CustomVocabularyItem"
      ( \x ->
          CustomVocabularyItem'
            Prelude.<$> (x Data..:? "displayAs")
            Prelude.<*> (x Data..:? "weight")
            Prelude.<*> (x Data..: "itemId")
            Prelude.<*> (x Data..: "phrase")
      )

instance Prelude.Hashable CustomVocabularyItem where
  hashWithSalt _salt CustomVocabularyItem' {..} =
    _salt
      `Prelude.hashWithSalt` displayAs
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` itemId
      `Prelude.hashWithSalt` phrase

instance Prelude.NFData CustomVocabularyItem where
  rnf CustomVocabularyItem' {..} =
    Prelude.rnf displayAs
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf itemId
      `Prelude.seq` Prelude.rnf phrase

instance Data.ToJSON CustomVocabularyItem where
  toJSON CustomVocabularyItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("displayAs" Data..=) Prelude.<$> displayAs,
            ("weight" Data..=) Prelude.<$> weight,
            Prelude.Just ("itemId" Data..= itemId),
            Prelude.Just ("phrase" Data..= phrase)
          ]
      )
