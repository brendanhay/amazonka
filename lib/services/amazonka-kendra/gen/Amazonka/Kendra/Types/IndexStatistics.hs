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
-- Module      : Amazonka.Kendra.Types.IndexStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.IndexStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.FaqStatistics
import Amazonka.Kendra.Types.TextDocumentStatistics
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the number of documents and the number of
-- questions and answers in an index.
--
-- /See:/ 'newIndexStatistics' smart constructor.
data IndexStatistics = IndexStatistics'
  { -- | The number of question and answer topics in the index.
    faqStatistics :: FaqStatistics,
    -- | The number of text documents indexed.
    textDocumentStatistics :: TextDocumentStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faqStatistics', 'indexStatistics_faqStatistics' - The number of question and answer topics in the index.
--
-- 'textDocumentStatistics', 'indexStatistics_textDocumentStatistics' - The number of text documents indexed.
newIndexStatistics ::
  -- | 'faqStatistics'
  FaqStatistics ->
  -- | 'textDocumentStatistics'
  TextDocumentStatistics ->
  IndexStatistics
newIndexStatistics
  pFaqStatistics_
  pTextDocumentStatistics_ =
    IndexStatistics'
      { faqStatistics = pFaqStatistics_,
        textDocumentStatistics = pTextDocumentStatistics_
      }

-- | The number of question and answer topics in the index.
indexStatistics_faqStatistics :: Lens.Lens' IndexStatistics FaqStatistics
indexStatistics_faqStatistics = Lens.lens (\IndexStatistics' {faqStatistics} -> faqStatistics) (\s@IndexStatistics' {} a -> s {faqStatistics = a} :: IndexStatistics)

-- | The number of text documents indexed.
indexStatistics_textDocumentStatistics :: Lens.Lens' IndexStatistics TextDocumentStatistics
indexStatistics_textDocumentStatistics = Lens.lens (\IndexStatistics' {textDocumentStatistics} -> textDocumentStatistics) (\s@IndexStatistics' {} a -> s {textDocumentStatistics = a} :: IndexStatistics)

instance Data.FromJSON IndexStatistics where
  parseJSON =
    Data.withObject
      "IndexStatistics"
      ( \x ->
          IndexStatistics'
            Prelude.<$> (x Data..: "FaqStatistics")
            Prelude.<*> (x Data..: "TextDocumentStatistics")
      )

instance Prelude.Hashable IndexStatistics where
  hashWithSalt _salt IndexStatistics' {..} =
    _salt `Prelude.hashWithSalt` faqStatistics
      `Prelude.hashWithSalt` textDocumentStatistics

instance Prelude.NFData IndexStatistics where
  rnf IndexStatistics' {..} =
    Prelude.rnf faqStatistics
      `Prelude.seq` Prelude.rnf textDocumentStatistics
