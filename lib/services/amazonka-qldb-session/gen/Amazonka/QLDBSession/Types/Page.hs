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
-- Module      : Amazonka.QLDBSession.Types.Page
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.Page where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.ValueHolder

-- | Contains details of the fetched page.
--
-- /See:/ 'newPage' smart constructor.
data Page = Page'
  { -- | The token of the next page.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains values in multiple encoding formats.
    values :: Prelude.Maybe [ValueHolder]
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
-- 'nextPageToken', 'page_nextPageToken' - The token of the next page.
--
-- 'values', 'page_values' - A structure that contains values in multiple encoding formats.
newPage ::
  Page
newPage =
  Page'
    { nextPageToken = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The token of the next page.
page_nextPageToken :: Lens.Lens' Page (Prelude.Maybe Prelude.Text)
page_nextPageToken = Lens.lens (\Page' {nextPageToken} -> nextPageToken) (\s@Page' {} a -> s {nextPageToken = a} :: Page)

-- | A structure that contains values in multiple encoding formats.
page_values :: Lens.Lens' Page (Prelude.Maybe [ValueHolder])
page_values = Lens.lens (\Page' {values} -> values) (\s@Page' {} a -> s {values = a} :: Page) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Page where
  parseJSON =
    Data.withObject
      "Page"
      ( \x ->
          Page'
            Prelude.<$> (x Data..:? "NextPageToken")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Page where
  hashWithSalt _salt Page' {..} =
    _salt
      `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` values

instance Prelude.NFData Page where
  rnf Page' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf values
