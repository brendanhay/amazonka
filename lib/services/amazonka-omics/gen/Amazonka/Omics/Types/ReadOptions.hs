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
-- Module      : Amazonka.Omics.Types.ReadOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Read options for an annotation import job.
--
-- /See:/ 'newReadOptions' smart constructor.
data ReadOptions = ReadOptions'
  { -- | The file\'s comment character.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The file\'s encoding.
    encoding :: Prelude.Maybe Prelude.Text,
    -- | A character for escaping quotes in the file.
    escape :: Prelude.Maybe Prelude.Text,
    -- | Whether quotes need to be escaped in the file.
    escapeQuotes :: Prelude.Maybe Prelude.Bool,
    -- | Whether the file has a header row.
    header :: Prelude.Maybe Prelude.Bool,
    -- | A line separator for the file.
    lineSep :: Prelude.Maybe Prelude.Text,
    -- | The file\'s quote character.
    quote :: Prelude.Maybe Prelude.Text,
    -- | Whether all values need to be quoted, or just those that contain quotes.
    quoteAll :: Prelude.Maybe Prelude.Bool,
    -- | The file\'s field separator.
    sep :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'readOptions_comment' - The file\'s comment character.
--
-- 'encoding', 'readOptions_encoding' - The file\'s encoding.
--
-- 'escape', 'readOptions_escape' - A character for escaping quotes in the file.
--
-- 'escapeQuotes', 'readOptions_escapeQuotes' - Whether quotes need to be escaped in the file.
--
-- 'header', 'readOptions_header' - Whether the file has a header row.
--
-- 'lineSep', 'readOptions_lineSep' - A line separator for the file.
--
-- 'quote', 'readOptions_quote' - The file\'s quote character.
--
-- 'quoteAll', 'readOptions_quoteAll' - Whether all values need to be quoted, or just those that contain quotes.
--
-- 'sep', 'readOptions_sep' - The file\'s field separator.
newReadOptions ::
  ReadOptions
newReadOptions =
  ReadOptions'
    { comment = Prelude.Nothing,
      encoding = Prelude.Nothing,
      escape = Prelude.Nothing,
      escapeQuotes = Prelude.Nothing,
      header = Prelude.Nothing,
      lineSep = Prelude.Nothing,
      quote = Prelude.Nothing,
      quoteAll = Prelude.Nothing,
      sep = Prelude.Nothing
    }

-- | The file\'s comment character.
readOptions_comment :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_comment = Lens.lens (\ReadOptions' {comment} -> comment) (\s@ReadOptions' {} a -> s {comment = a} :: ReadOptions)

-- | The file\'s encoding.
readOptions_encoding :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_encoding = Lens.lens (\ReadOptions' {encoding} -> encoding) (\s@ReadOptions' {} a -> s {encoding = a} :: ReadOptions)

-- | A character for escaping quotes in the file.
readOptions_escape :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_escape = Lens.lens (\ReadOptions' {escape} -> escape) (\s@ReadOptions' {} a -> s {escape = a} :: ReadOptions)

-- | Whether quotes need to be escaped in the file.
readOptions_escapeQuotes :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Bool)
readOptions_escapeQuotes = Lens.lens (\ReadOptions' {escapeQuotes} -> escapeQuotes) (\s@ReadOptions' {} a -> s {escapeQuotes = a} :: ReadOptions)

-- | Whether the file has a header row.
readOptions_header :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Bool)
readOptions_header = Lens.lens (\ReadOptions' {header} -> header) (\s@ReadOptions' {} a -> s {header = a} :: ReadOptions)

-- | A line separator for the file.
readOptions_lineSep :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_lineSep = Lens.lens (\ReadOptions' {lineSep} -> lineSep) (\s@ReadOptions' {} a -> s {lineSep = a} :: ReadOptions)

-- | The file\'s quote character.
readOptions_quote :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_quote = Lens.lens (\ReadOptions' {quote} -> quote) (\s@ReadOptions' {} a -> s {quote = a} :: ReadOptions)

-- | Whether all values need to be quoted, or just those that contain quotes.
readOptions_quoteAll :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Bool)
readOptions_quoteAll = Lens.lens (\ReadOptions' {quoteAll} -> quoteAll) (\s@ReadOptions' {} a -> s {quoteAll = a} :: ReadOptions)

-- | The file\'s field separator.
readOptions_sep :: Lens.Lens' ReadOptions (Prelude.Maybe Prelude.Text)
readOptions_sep = Lens.lens (\ReadOptions' {sep} -> sep) (\s@ReadOptions' {} a -> s {sep = a} :: ReadOptions)

instance Data.FromJSON ReadOptions where
  parseJSON =
    Data.withObject
      "ReadOptions"
      ( \x ->
          ReadOptions'
            Prelude.<$> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "encoding")
            Prelude.<*> (x Data..:? "escape")
            Prelude.<*> (x Data..:? "escapeQuotes")
            Prelude.<*> (x Data..:? "header")
            Prelude.<*> (x Data..:? "lineSep")
            Prelude.<*> (x Data..:? "quote")
            Prelude.<*> (x Data..:? "quoteAll")
            Prelude.<*> (x Data..:? "sep")
      )

instance Prelude.Hashable ReadOptions where
  hashWithSalt _salt ReadOptions' {..} =
    _salt `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` escape
      `Prelude.hashWithSalt` escapeQuotes
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` lineSep
      `Prelude.hashWithSalt` quote
      `Prelude.hashWithSalt` quoteAll
      `Prelude.hashWithSalt` sep

instance Prelude.NFData ReadOptions where
  rnf ReadOptions' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf escape
      `Prelude.seq` Prelude.rnf escapeQuotes
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf lineSep
      `Prelude.seq` Prelude.rnf quote
      `Prelude.seq` Prelude.rnf quoteAll
      `Prelude.seq` Prelude.rnf sep

instance Data.ToJSON ReadOptions where
  toJSON ReadOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("encoding" Data..=) Prelude.<$> encoding,
            ("escape" Data..=) Prelude.<$> escape,
            ("escapeQuotes" Data..=) Prelude.<$> escapeQuotes,
            ("header" Data..=) Prelude.<$> header,
            ("lineSep" Data..=) Prelude.<$> lineSep,
            ("quote" Data..=) Prelude.<$> quote,
            ("quoteAll" Data..=) Prelude.<$> quoteAll,
            ("sep" Data..=) Prelude.<$> sep
          ]
      )
