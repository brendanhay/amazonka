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
-- Module      : Amazonka.LexV2Models.Types.FailedCustomVocabularyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.FailedCustomVocabularyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | The unique failed custom vocabulary item from the custom vocabulary
-- list.
--
-- /See:/ 'newFailedCustomVocabularyItem' smart constructor.
data FailedCustomVocabularyItem = FailedCustomVocabularyItem'
  { -- | The unique error code for the failed custom vocabulary item from the
    -- custom vocabulary list.
    errorCode :: Prelude.Maybe ErrorCode,
    -- | The error message for the failed custom vocabulary item from the custom
    -- vocabulary list.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique item identifer for the failed custom vocabulary item from the
    -- custom vocabulary list.
    itemId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCustomVocabularyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedCustomVocabularyItem_errorCode' - The unique error code for the failed custom vocabulary item from the
-- custom vocabulary list.
--
-- 'errorMessage', 'failedCustomVocabularyItem_errorMessage' - The error message for the failed custom vocabulary item from the custom
-- vocabulary list.
--
-- 'itemId', 'failedCustomVocabularyItem_itemId' - The unique item identifer for the failed custom vocabulary item from the
-- custom vocabulary list.
newFailedCustomVocabularyItem ::
  FailedCustomVocabularyItem
newFailedCustomVocabularyItem =
  FailedCustomVocabularyItem'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      itemId = Prelude.Nothing
    }

-- | The unique error code for the failed custom vocabulary item from the
-- custom vocabulary list.
failedCustomVocabularyItem_errorCode :: Lens.Lens' FailedCustomVocabularyItem (Prelude.Maybe ErrorCode)
failedCustomVocabularyItem_errorCode = Lens.lens (\FailedCustomVocabularyItem' {errorCode} -> errorCode) (\s@FailedCustomVocabularyItem' {} a -> s {errorCode = a} :: FailedCustomVocabularyItem)

-- | The error message for the failed custom vocabulary item from the custom
-- vocabulary list.
failedCustomVocabularyItem_errorMessage :: Lens.Lens' FailedCustomVocabularyItem (Prelude.Maybe Prelude.Text)
failedCustomVocabularyItem_errorMessage = Lens.lens (\FailedCustomVocabularyItem' {errorMessage} -> errorMessage) (\s@FailedCustomVocabularyItem' {} a -> s {errorMessage = a} :: FailedCustomVocabularyItem)

-- | The unique item identifer for the failed custom vocabulary item from the
-- custom vocabulary list.
failedCustomVocabularyItem_itemId :: Lens.Lens' FailedCustomVocabularyItem (Prelude.Maybe Prelude.Text)
failedCustomVocabularyItem_itemId = Lens.lens (\FailedCustomVocabularyItem' {itemId} -> itemId) (\s@FailedCustomVocabularyItem' {} a -> s {itemId = a} :: FailedCustomVocabularyItem)

instance Data.FromJSON FailedCustomVocabularyItem where
  parseJSON =
    Data.withObject
      "FailedCustomVocabularyItem"
      ( \x ->
          FailedCustomVocabularyItem'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "itemId")
      )

instance Prelude.Hashable FailedCustomVocabularyItem where
  hashWithSalt _salt FailedCustomVocabularyItem' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` itemId

instance Prelude.NFData FailedCustomVocabularyItem where
  rnf FailedCustomVocabularyItem' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf itemId
