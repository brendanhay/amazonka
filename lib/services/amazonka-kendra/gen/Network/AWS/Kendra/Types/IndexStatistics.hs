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
-- Module      : Network.AWS.Kendra.Types.IndexStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.IndexStatistics where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.FaqStatistics
import Network.AWS.Kendra.Types.TextDocumentStatistics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON IndexStatistics where
  parseJSON =
    Core.withObject
      "IndexStatistics"
      ( \x ->
          IndexStatistics'
            Prelude.<$> (x Core..: "FaqStatistics")
            Prelude.<*> (x Core..: "TextDocumentStatistics")
      )

instance Prelude.Hashable IndexStatistics

instance Prelude.NFData IndexStatistics
