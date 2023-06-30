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
-- Module      : Amazonka.SecurityHub.Types.Page
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Page where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Range

-- | An occurrence of sensitive data in an Adobe Portable Document Format
-- (PDF) file.
--
-- /See:/ 'newPage' smart constructor.
data Page = Page'
  { -- | An occurrence of sensitive data detected in a non-binary text file or a
    -- Microsoft Word file. Non-binary text files include files such as HTML,
    -- XML, JSON, and TXT files.
    lineRange :: Prelude.Maybe Range,
    -- | An occurrence of sensitive data detected in a binary text file.
    offsetRange :: Prelude.Maybe Range,
    -- | The page number of the page that contains the sensitive data.
    pageNumber :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Page' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineRange', 'page_lineRange' - An occurrence of sensitive data detected in a non-binary text file or a
-- Microsoft Word file. Non-binary text files include files such as HTML,
-- XML, JSON, and TXT files.
--
-- 'offsetRange', 'page_offsetRange' - An occurrence of sensitive data detected in a binary text file.
--
-- 'pageNumber', 'page_pageNumber' - The page number of the page that contains the sensitive data.
newPage ::
  Page
newPage =
  Page'
    { lineRange = Prelude.Nothing,
      offsetRange = Prelude.Nothing,
      pageNumber = Prelude.Nothing
    }

-- | An occurrence of sensitive data detected in a non-binary text file or a
-- Microsoft Word file. Non-binary text files include files such as HTML,
-- XML, JSON, and TXT files.
page_lineRange :: Lens.Lens' Page (Prelude.Maybe Range)
page_lineRange = Lens.lens (\Page' {lineRange} -> lineRange) (\s@Page' {} a -> s {lineRange = a} :: Page)

-- | An occurrence of sensitive data detected in a binary text file.
page_offsetRange :: Lens.Lens' Page (Prelude.Maybe Range)
page_offsetRange = Lens.lens (\Page' {offsetRange} -> offsetRange) (\s@Page' {} a -> s {offsetRange = a} :: Page)

-- | The page number of the page that contains the sensitive data.
page_pageNumber :: Lens.Lens' Page (Prelude.Maybe Prelude.Integer)
page_pageNumber = Lens.lens (\Page' {pageNumber} -> pageNumber) (\s@Page' {} a -> s {pageNumber = a} :: Page)

instance Data.FromJSON Page where
  parseJSON =
    Data.withObject
      "Page"
      ( \x ->
          Page'
            Prelude.<$> (x Data..:? "LineRange")
            Prelude.<*> (x Data..:? "OffsetRange")
            Prelude.<*> (x Data..:? "PageNumber")
      )

instance Prelude.Hashable Page where
  hashWithSalt _salt Page' {..} =
    _salt
      `Prelude.hashWithSalt` lineRange
      `Prelude.hashWithSalt` offsetRange
      `Prelude.hashWithSalt` pageNumber

instance Prelude.NFData Page where
  rnf Page' {..} =
    Prelude.rnf lineRange
      `Prelude.seq` Prelude.rnf offsetRange
      `Prelude.seq` Prelude.rnf pageNumber

instance Data.ToJSON Page where
  toJSON Page' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LineRange" Data..=) Prelude.<$> lineRange,
            ("OffsetRange" Data..=) Prelude.<$> offsetRange,
            ("PageNumber" Data..=) Prelude.<$> pageNumber
          ]
      )
