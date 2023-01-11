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
-- Module      : Amazonka.LexV2Models.Types.CustomVocabularyEntryId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomVocabularyEntryId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The unique entry identifier for the custom vocabulary items.
--
-- /See:/ 'newCustomVocabularyEntryId' smart constructor.
data CustomVocabularyEntryId = CustomVocabularyEntryId'
  { -- | The unique item identifier for the custom vocabulary items.
    itemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVocabularyEntryId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemId', 'customVocabularyEntryId_itemId' - The unique item identifier for the custom vocabulary items.
newCustomVocabularyEntryId ::
  -- | 'itemId'
  Prelude.Text ->
  CustomVocabularyEntryId
newCustomVocabularyEntryId pItemId_ =
  CustomVocabularyEntryId' {itemId = pItemId_}

-- | The unique item identifier for the custom vocabulary items.
customVocabularyEntryId_itemId :: Lens.Lens' CustomVocabularyEntryId Prelude.Text
customVocabularyEntryId_itemId = Lens.lens (\CustomVocabularyEntryId' {itemId} -> itemId) (\s@CustomVocabularyEntryId' {} a -> s {itemId = a} :: CustomVocabularyEntryId)

instance Prelude.Hashable CustomVocabularyEntryId where
  hashWithSalt _salt CustomVocabularyEntryId' {..} =
    _salt `Prelude.hashWithSalt` itemId

instance Prelude.NFData CustomVocabularyEntryId where
  rnf CustomVocabularyEntryId' {..} = Prelude.rnf itemId

instance Data.ToJSON CustomVocabularyEntryId where
  toJSON CustomVocabularyEntryId' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("itemId" Data..= itemId)]
      )
