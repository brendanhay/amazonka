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
-- Module      : Network.AWS.Kendra.Types.TextDocumentStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.TextDocumentStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about text documents indexed in an index.
--
-- /See:/ 'newTextDocumentStatistics' smart constructor.
data TextDocumentStatistics = TextDocumentStatistics'
  { -- | The number of text documents indexed.
    indexedTextDocumentsCount :: Prelude.Natural,
    -- | The total size, in bytes, of the indexed documents.
    indexedTextBytes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextDocumentStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexedTextDocumentsCount', 'textDocumentStatistics_indexedTextDocumentsCount' - The number of text documents indexed.
--
-- 'indexedTextBytes', 'textDocumentStatistics_indexedTextBytes' - The total size, in bytes, of the indexed documents.
newTextDocumentStatistics ::
  -- | 'indexedTextDocumentsCount'
  Prelude.Natural ->
  -- | 'indexedTextBytes'
  Prelude.Natural ->
  TextDocumentStatistics
newTextDocumentStatistics
  pIndexedTextDocumentsCount_
  pIndexedTextBytes_ =
    TextDocumentStatistics'
      { indexedTextDocumentsCount =
          pIndexedTextDocumentsCount_,
        indexedTextBytes = pIndexedTextBytes_
      }

-- | The number of text documents indexed.
textDocumentStatistics_indexedTextDocumentsCount :: Lens.Lens' TextDocumentStatistics Prelude.Natural
textDocumentStatistics_indexedTextDocumentsCount = Lens.lens (\TextDocumentStatistics' {indexedTextDocumentsCount} -> indexedTextDocumentsCount) (\s@TextDocumentStatistics' {} a -> s {indexedTextDocumentsCount = a} :: TextDocumentStatistics)

-- | The total size, in bytes, of the indexed documents.
textDocumentStatistics_indexedTextBytes :: Lens.Lens' TextDocumentStatistics Prelude.Natural
textDocumentStatistics_indexedTextBytes = Lens.lens (\TextDocumentStatistics' {indexedTextBytes} -> indexedTextBytes) (\s@TextDocumentStatistics' {} a -> s {indexedTextBytes = a} :: TextDocumentStatistics)

instance Core.FromJSON TextDocumentStatistics where
  parseJSON =
    Core.withObject
      "TextDocumentStatistics"
      ( \x ->
          TextDocumentStatistics'
            Prelude.<$> (x Core..: "IndexedTextDocumentsCount")
            Prelude.<*> (x Core..: "IndexedTextBytes")
      )

instance Prelude.Hashable TextDocumentStatistics

instance Prelude.NFData TextDocumentStatistics
