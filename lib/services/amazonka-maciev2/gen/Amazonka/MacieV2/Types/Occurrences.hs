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
-- Module      : Amazonka.MacieV2.Types.Occurrences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Occurrences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Cell
import Amazonka.MacieV2.Types.Page
import Amazonka.MacieV2.Types.Range
import Amazonka.MacieV2.Types.Record
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of 1-15 occurrences of sensitive data that was
-- detected by a managed data identifier or a custom data identifier and
-- produced a sensitive data finding.
--
-- /See:/ 'newOccurrences' smart constructor.
data Occurrences = Occurrences'
  { -- | An array of objects, one for each occurrence of sensitive data in a
    -- Microsoft Excel workbook, CSV file, or TSV file. This value is null for
    -- all other types of files.
    --
    -- Each Cell object specifies a cell or field that contains the sensitive
    -- data.
    cells :: Prelude.Maybe [Cell],
    -- | An array of objects, one for each occurrence of sensitive data in a
    -- non-binary text file, such as an HTML, TXT, or XML file. Each Range
    -- object specifies a line or inclusive range of lines that contains the
    -- sensitive data, and the position of the data on the specified line or
    -- lines.
    --
    -- This value is often null for file types that are supported by Cell,
    -- Page, or Record objects. Exceptions are the location of sensitive data
    -- in: unstructured sections of an otherwise structured file, such as a
    -- comment in a file; a malformed file that Amazon Macie analyzes as plain
    -- text; and, a CSV or TSV file that has any column names that contain
    -- sensitive data.
    lineRanges :: Prelude.Maybe [Range],
    -- | Reserved for future use.
    offsetRanges :: Prelude.Maybe [Range],
    -- | An array of objects, one for each occurrence of sensitive data in an
    -- Adobe Portable Document Format file. This value is null for all other
    -- types of files.
    --
    -- Each Page object specifies a page that contains the sensitive data.
    pages :: Prelude.Maybe [Page],
    -- | An array of objects, one for each occurrence of sensitive data in an
    -- Apache Avro object container, Apache Parquet file, JSON file, or JSON
    -- Lines file. This value is null for all other types of files.
    --
    -- For an Avro object container or Parquet file, each Record object
    -- specifies a record index and the path to a field in a record that
    -- contains the sensitive data. For a JSON or JSON Lines file, each Record
    -- object specifies the path to a field or array that contains the
    -- sensitive data. For a JSON Lines file, it also specifies the index of
    -- the line that contains the data.
    records :: Prelude.Maybe [Record]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Occurrences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'occurrences_cells' - An array of objects, one for each occurrence of sensitive data in a
-- Microsoft Excel workbook, CSV file, or TSV file. This value is null for
-- all other types of files.
--
-- Each Cell object specifies a cell or field that contains the sensitive
-- data.
--
-- 'lineRanges', 'occurrences_lineRanges' - An array of objects, one for each occurrence of sensitive data in a
-- non-binary text file, such as an HTML, TXT, or XML file. Each Range
-- object specifies a line or inclusive range of lines that contains the
-- sensitive data, and the position of the data on the specified line or
-- lines.
--
-- This value is often null for file types that are supported by Cell,
-- Page, or Record objects. Exceptions are the location of sensitive data
-- in: unstructured sections of an otherwise structured file, such as a
-- comment in a file; a malformed file that Amazon Macie analyzes as plain
-- text; and, a CSV or TSV file that has any column names that contain
-- sensitive data.
--
-- 'offsetRanges', 'occurrences_offsetRanges' - Reserved for future use.
--
-- 'pages', 'occurrences_pages' - An array of objects, one for each occurrence of sensitive data in an
-- Adobe Portable Document Format file. This value is null for all other
-- types of files.
--
-- Each Page object specifies a page that contains the sensitive data.
--
-- 'records', 'occurrences_records' - An array of objects, one for each occurrence of sensitive data in an
-- Apache Avro object container, Apache Parquet file, JSON file, or JSON
-- Lines file. This value is null for all other types of files.
--
-- For an Avro object container or Parquet file, each Record object
-- specifies a record index and the path to a field in a record that
-- contains the sensitive data. For a JSON or JSON Lines file, each Record
-- object specifies the path to a field or array that contains the
-- sensitive data. For a JSON Lines file, it also specifies the index of
-- the line that contains the data.
newOccurrences ::
  Occurrences
newOccurrences =
  Occurrences'
    { cells = Prelude.Nothing,
      lineRanges = Prelude.Nothing,
      offsetRanges = Prelude.Nothing,
      pages = Prelude.Nothing,
      records = Prelude.Nothing
    }

-- | An array of objects, one for each occurrence of sensitive data in a
-- Microsoft Excel workbook, CSV file, or TSV file. This value is null for
-- all other types of files.
--
-- Each Cell object specifies a cell or field that contains the sensitive
-- data.
occurrences_cells :: Lens.Lens' Occurrences (Prelude.Maybe [Cell])
occurrences_cells = Lens.lens (\Occurrences' {cells} -> cells) (\s@Occurrences' {} a -> s {cells = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each occurrence of sensitive data in a
-- non-binary text file, such as an HTML, TXT, or XML file. Each Range
-- object specifies a line or inclusive range of lines that contains the
-- sensitive data, and the position of the data on the specified line or
-- lines.
--
-- This value is often null for file types that are supported by Cell,
-- Page, or Record objects. Exceptions are the location of sensitive data
-- in: unstructured sections of an otherwise structured file, such as a
-- comment in a file; a malformed file that Amazon Macie analyzes as plain
-- text; and, a CSV or TSV file that has any column names that contain
-- sensitive data.
occurrences_lineRanges :: Lens.Lens' Occurrences (Prelude.Maybe [Range])
occurrences_lineRanges = Lens.lens (\Occurrences' {lineRanges} -> lineRanges) (\s@Occurrences' {} a -> s {lineRanges = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | Reserved for future use.
occurrences_offsetRanges :: Lens.Lens' Occurrences (Prelude.Maybe [Range])
occurrences_offsetRanges = Lens.lens (\Occurrences' {offsetRanges} -> offsetRanges) (\s@Occurrences' {} a -> s {offsetRanges = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each occurrence of sensitive data in an
-- Adobe Portable Document Format file. This value is null for all other
-- types of files.
--
-- Each Page object specifies a page that contains the sensitive data.
occurrences_pages :: Lens.Lens' Occurrences (Prelude.Maybe [Page])
occurrences_pages = Lens.lens (\Occurrences' {pages} -> pages) (\s@Occurrences' {} a -> s {pages = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects, one for each occurrence of sensitive data in an
-- Apache Avro object container, Apache Parquet file, JSON file, or JSON
-- Lines file. This value is null for all other types of files.
--
-- For an Avro object container or Parquet file, each Record object
-- specifies a record index and the path to a field in a record that
-- contains the sensitive data. For a JSON or JSON Lines file, each Record
-- object specifies the path to a field or array that contains the
-- sensitive data. For a JSON Lines file, it also specifies the index of
-- the line that contains the data.
occurrences_records :: Lens.Lens' Occurrences (Prelude.Maybe [Record])
occurrences_records = Lens.lens (\Occurrences' {records} -> records) (\s@Occurrences' {} a -> s {records = a} :: Occurrences) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Occurrences where
  parseJSON =
    Data.withObject
      "Occurrences"
      ( \x ->
          Occurrences'
            Prelude.<$> (x Data..:? "cells" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lineRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "offsetRanges" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "pages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "records" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Occurrences where
  hashWithSalt _salt Occurrences' {..} =
    _salt `Prelude.hashWithSalt` cells
      `Prelude.hashWithSalt` lineRanges
      `Prelude.hashWithSalt` offsetRanges
      `Prelude.hashWithSalt` pages
      `Prelude.hashWithSalt` records

instance Prelude.NFData Occurrences where
  rnf Occurrences' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf lineRanges
      `Prelude.seq` Prelude.rnf offsetRanges
      `Prelude.seq` Prelude.rnf pages
      `Prelude.seq` Prelude.rnf records
