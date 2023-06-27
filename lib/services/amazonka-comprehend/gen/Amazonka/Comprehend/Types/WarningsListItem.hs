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
-- Module      : Amazonka.Comprehend.Types.WarningsListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.WarningsListItem where

import Amazonka.Comprehend.Types.PageBasedWarningCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The system identified one of the following warnings while processing the
-- input document:
--
-- -   The document to classify is plain text, but the classifier is a
--     native model.
--
-- -   The document to classify is semi-structured, but the classifier is a
--     plain-text model.
--
-- /See:/ 'newWarningsListItem' smart constructor.
data WarningsListItem = WarningsListItem'
  { -- | Page number in the input document.
    page :: Prelude.Maybe Prelude.Int,
    -- | The type of warning.
    warnCode :: Prelude.Maybe PageBasedWarningCode,
    -- | Text message associated with the warning.
    warnMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WarningsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'page', 'warningsListItem_page' - Page number in the input document.
--
-- 'warnCode', 'warningsListItem_warnCode' - The type of warning.
--
-- 'warnMessage', 'warningsListItem_warnMessage' - Text message associated with the warning.
newWarningsListItem ::
  WarningsListItem
newWarningsListItem =
  WarningsListItem'
    { page = Prelude.Nothing,
      warnCode = Prelude.Nothing,
      warnMessage = Prelude.Nothing
    }

-- | Page number in the input document.
warningsListItem_page :: Lens.Lens' WarningsListItem (Prelude.Maybe Prelude.Int)
warningsListItem_page = Lens.lens (\WarningsListItem' {page} -> page) (\s@WarningsListItem' {} a -> s {page = a} :: WarningsListItem)

-- | The type of warning.
warningsListItem_warnCode :: Lens.Lens' WarningsListItem (Prelude.Maybe PageBasedWarningCode)
warningsListItem_warnCode = Lens.lens (\WarningsListItem' {warnCode} -> warnCode) (\s@WarningsListItem' {} a -> s {warnCode = a} :: WarningsListItem)

-- | Text message associated with the warning.
warningsListItem_warnMessage :: Lens.Lens' WarningsListItem (Prelude.Maybe Prelude.Text)
warningsListItem_warnMessage = Lens.lens (\WarningsListItem' {warnMessage} -> warnMessage) (\s@WarningsListItem' {} a -> s {warnMessage = a} :: WarningsListItem)

instance Data.FromJSON WarningsListItem where
  parseJSON =
    Data.withObject
      "WarningsListItem"
      ( \x ->
          WarningsListItem'
            Prelude.<$> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "WarnCode")
            Prelude.<*> (x Data..:? "WarnMessage")
      )

instance Prelude.Hashable WarningsListItem where
  hashWithSalt _salt WarningsListItem' {..} =
    _salt
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` warnCode
      `Prelude.hashWithSalt` warnMessage

instance Prelude.NFData WarningsListItem where
  rnf WarningsListItem' {..} =
    Prelude.rnf page
      `Prelude.seq` Prelude.rnf warnCode
      `Prelude.seq` Prelude.rnf warnMessage
