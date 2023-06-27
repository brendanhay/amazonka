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
-- Module      : Amazonka.TimeStreamWrite.Types.CsvConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.CsvConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A delimited data format where the column separator can be a comma and
-- the record separator is a newline character.
--
-- /See:/ 'newCsvConfiguration' smart constructor.
data CsvConfiguration = CsvConfiguration'
  { -- | Column separator can be one of comma (\',\'), pipe (\'|), semicolon
    -- (\';\'), tab(\'\/t\'), or blank space (\' \').
    columnSeparator :: Prelude.Maybe Prelude.Text,
    -- | Escape character can be one of
    escapeChar :: Prelude.Maybe Prelude.Text,
    -- | Can be blank space (\' \').
    nullValue :: Prelude.Maybe Prelude.Text,
    -- | Can be single quote (\') or double quote (\").
    quoteChar :: Prelude.Maybe Prelude.Text,
    -- | Specifies to trim leading and trailing white space.
    trimWhiteSpace :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsvConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnSeparator', 'csvConfiguration_columnSeparator' - Column separator can be one of comma (\',\'), pipe (\'|), semicolon
-- (\';\'), tab(\'\/t\'), or blank space (\' \').
--
-- 'escapeChar', 'csvConfiguration_escapeChar' - Escape character can be one of
--
-- 'nullValue', 'csvConfiguration_nullValue' - Can be blank space (\' \').
--
-- 'quoteChar', 'csvConfiguration_quoteChar' - Can be single quote (\') or double quote (\").
--
-- 'trimWhiteSpace', 'csvConfiguration_trimWhiteSpace' - Specifies to trim leading and trailing white space.
newCsvConfiguration ::
  CsvConfiguration
newCsvConfiguration =
  CsvConfiguration'
    { columnSeparator =
        Prelude.Nothing,
      escapeChar = Prelude.Nothing,
      nullValue = Prelude.Nothing,
      quoteChar = Prelude.Nothing,
      trimWhiteSpace = Prelude.Nothing
    }

-- | Column separator can be one of comma (\',\'), pipe (\'|), semicolon
-- (\';\'), tab(\'\/t\'), or blank space (\' \').
csvConfiguration_columnSeparator :: Lens.Lens' CsvConfiguration (Prelude.Maybe Prelude.Text)
csvConfiguration_columnSeparator = Lens.lens (\CsvConfiguration' {columnSeparator} -> columnSeparator) (\s@CsvConfiguration' {} a -> s {columnSeparator = a} :: CsvConfiguration)

-- | Escape character can be one of
csvConfiguration_escapeChar :: Lens.Lens' CsvConfiguration (Prelude.Maybe Prelude.Text)
csvConfiguration_escapeChar = Lens.lens (\CsvConfiguration' {escapeChar} -> escapeChar) (\s@CsvConfiguration' {} a -> s {escapeChar = a} :: CsvConfiguration)

-- | Can be blank space (\' \').
csvConfiguration_nullValue :: Lens.Lens' CsvConfiguration (Prelude.Maybe Prelude.Text)
csvConfiguration_nullValue = Lens.lens (\CsvConfiguration' {nullValue} -> nullValue) (\s@CsvConfiguration' {} a -> s {nullValue = a} :: CsvConfiguration)

-- | Can be single quote (\') or double quote (\").
csvConfiguration_quoteChar :: Lens.Lens' CsvConfiguration (Prelude.Maybe Prelude.Text)
csvConfiguration_quoteChar = Lens.lens (\CsvConfiguration' {quoteChar} -> quoteChar) (\s@CsvConfiguration' {} a -> s {quoteChar = a} :: CsvConfiguration)

-- | Specifies to trim leading and trailing white space.
csvConfiguration_trimWhiteSpace :: Lens.Lens' CsvConfiguration (Prelude.Maybe Prelude.Bool)
csvConfiguration_trimWhiteSpace = Lens.lens (\CsvConfiguration' {trimWhiteSpace} -> trimWhiteSpace) (\s@CsvConfiguration' {} a -> s {trimWhiteSpace = a} :: CsvConfiguration)

instance Data.FromJSON CsvConfiguration where
  parseJSON =
    Data.withObject
      "CsvConfiguration"
      ( \x ->
          CsvConfiguration'
            Prelude.<$> (x Data..:? "ColumnSeparator")
            Prelude.<*> (x Data..:? "EscapeChar")
            Prelude.<*> (x Data..:? "NullValue")
            Prelude.<*> (x Data..:? "QuoteChar")
            Prelude.<*> (x Data..:? "TrimWhiteSpace")
      )

instance Prelude.Hashable CsvConfiguration where
  hashWithSalt _salt CsvConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` columnSeparator
      `Prelude.hashWithSalt` escapeChar
      `Prelude.hashWithSalt` nullValue
      `Prelude.hashWithSalt` quoteChar
      `Prelude.hashWithSalt` trimWhiteSpace

instance Prelude.NFData CsvConfiguration where
  rnf CsvConfiguration' {..} =
    Prelude.rnf columnSeparator
      `Prelude.seq` Prelude.rnf escapeChar
      `Prelude.seq` Prelude.rnf nullValue
      `Prelude.seq` Prelude.rnf quoteChar
      `Prelude.seq` Prelude.rnf trimWhiteSpace

instance Data.ToJSON CsvConfiguration where
  toJSON CsvConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnSeparator" Data..=)
              Prelude.<$> columnSeparator,
            ("EscapeChar" Data..=) Prelude.<$> escapeChar,
            ("NullValue" Data..=) Prelude.<$> nullValue,
            ("QuoteChar" Data..=) Prelude.<$> quoteChar,
            ("TrimWhiteSpace" Data..=)
              Prelude.<$> trimWhiteSpace
          ]
      )
