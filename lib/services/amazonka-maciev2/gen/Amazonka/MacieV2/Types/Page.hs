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
-- Module      : Amazonka.MacieV2.Types.Page
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Page where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Range
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of an occurrence of sensitive data in an Adobe
-- Portable Document Format file.
--
-- /See:/ 'newPage' smart constructor.
data Page = Page'
  { -- | Reserved for future use.
    lineRange :: Prelude.Maybe Range,
    -- | Reserved for future use.
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
-- 'lineRange', 'page_lineRange' - Reserved for future use.
--
-- 'offsetRange', 'page_offsetRange' - Reserved for future use.
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

-- | Reserved for future use.
page_lineRange :: Lens.Lens' Page (Prelude.Maybe Range)
page_lineRange = Lens.lens (\Page' {lineRange} -> lineRange) (\s@Page' {} a -> s {lineRange = a} :: Page)

-- | Reserved for future use.
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
            Prelude.<$> (x Data..:? "lineRange")
            Prelude.<*> (x Data..:? "offsetRange")
            Prelude.<*> (x Data..:? "pageNumber")
      )

instance Prelude.Hashable Page where
  hashWithSalt _salt Page' {..} =
    _salt
      `Prelude.hashWithSalt` lineRange
      `Prelude.hashWithSalt` offsetRange
      `Prelude.hashWithSalt` pageNumber

instance Prelude.NFData Page where
  rnf Page' {..} =
    Prelude.rnf lineRange `Prelude.seq`
      Prelude.rnf offsetRange `Prelude.seq`
        Prelude.rnf pageNumber
