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
-- Module      : Amazonka.Textract.Types.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Query where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Each query contains the question you want to ask in the Text and the
-- alias you want to associate.
--
-- /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | Alias attached to the query, for ease of location.
    alias :: Prelude.Maybe Prelude.Text,
    -- | Pages is a parameter that the user inputs to specify which pages to
    -- apply a query to. The following is a list of rules for using this
    -- parameter.
    --
    -- -   If a page is not specified, it is set to @[\"1\"]@ by default.
    --
    -- -   The following characters are allowed in the parameter\'s string:
    --     @0 1 2 3 4 5 6 7 8 9 - *@. No whitespace is allowed.
    --
    -- -   When using * to indicate all pages, it must be the only element in
    --     the list.
    --
    -- -   You can use page intervals, such as @[“1-3”, “1-1”, “4-*”]@. Where
    --     @*@ indicates last page of document.
    --
    -- -   Specified pages must be greater than 0 and less than or equal to the
    --     number of pages in the document.
    pages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Question that Amazon Textract will apply to the document. An example
    -- would be \"What is the customer\'s SSN?\"
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Query' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'query_alias' - Alias attached to the query, for ease of location.
--
-- 'pages', 'query_pages' - Pages is a parameter that the user inputs to specify which pages to
-- apply a query to. The following is a list of rules for using this
-- parameter.
--
-- -   If a page is not specified, it is set to @[\"1\"]@ by default.
--
-- -   The following characters are allowed in the parameter\'s string:
--     @0 1 2 3 4 5 6 7 8 9 - *@. No whitespace is allowed.
--
-- -   When using * to indicate all pages, it must be the only element in
--     the list.
--
-- -   You can use page intervals, such as @[“1-3”, “1-1”, “4-*”]@. Where
--     @*@ indicates last page of document.
--
-- -   Specified pages must be greater than 0 and less than or equal to the
--     number of pages in the document.
--
-- 'text', 'query_text' - Question that Amazon Textract will apply to the document. An example
-- would be \"What is the customer\'s SSN?\"
newQuery ::
  -- | 'text'
  Prelude.Text ->
  Query
newQuery pText_ =
  Query'
    { alias = Prelude.Nothing,
      pages = Prelude.Nothing,
      text = pText_
    }

-- | Alias attached to the query, for ease of location.
query_alias :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_alias = Lens.lens (\Query' {alias} -> alias) (\s@Query' {} a -> s {alias = a} :: Query)

-- | Pages is a parameter that the user inputs to specify which pages to
-- apply a query to. The following is a list of rules for using this
-- parameter.
--
-- -   If a page is not specified, it is set to @[\"1\"]@ by default.
--
-- -   The following characters are allowed in the parameter\'s string:
--     @0 1 2 3 4 5 6 7 8 9 - *@. No whitespace is allowed.
--
-- -   When using * to indicate all pages, it must be the only element in
--     the list.
--
-- -   You can use page intervals, such as @[“1-3”, “1-1”, “4-*”]@. Where
--     @*@ indicates last page of document.
--
-- -   Specified pages must be greater than 0 and less than or equal to the
--     number of pages in the document.
query_pages :: Lens.Lens' Query (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
query_pages = Lens.lens (\Query' {pages} -> pages) (\s@Query' {} a -> s {pages = a} :: Query) Prelude.. Lens.mapping Lens.coerced

-- | Question that Amazon Textract will apply to the document. An example
-- would be \"What is the customer\'s SSN?\"
query_text :: Lens.Lens' Query Prelude.Text
query_text = Lens.lens (\Query' {text} -> text) (\s@Query' {} a -> s {text = a} :: Query)

instance Data.FromJSON Query where
  parseJSON =
    Data.withObject
      "Query"
      ( \x ->
          Query'
            Prelude.<$> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "Pages")
            Prelude.<*> (x Data..: "Text")
      )

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` pages
      `Prelude.hashWithSalt` text

instance Prelude.NFData Query where
  rnf Query' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf pages
      `Prelude.seq` Prelude.rnf text

instance Data.ToJSON Query where
  toJSON Query' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Alias" Data..=) Prelude.<$> alias,
            ("Pages" Data..=) Prelude.<$> pages,
            Prelude.Just ("Text" Data..= text)
          ]
      )
