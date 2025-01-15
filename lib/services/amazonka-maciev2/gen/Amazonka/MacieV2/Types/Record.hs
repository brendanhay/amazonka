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
-- Module      : Amazonka.MacieV2.Types.Record
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the location of an occurrence of sensitive data in an Apache
-- Avro object container, Apache Parquet file, JSON file, or JSON Lines
-- file.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The path, as a JSONPath expression, to the sensitive data. For an Avro
    -- object container or Parquet file, this is the path to the field in the
    -- record (recordIndex) that contains the data. For a JSON or JSON Lines
    -- file, this is the path to the field or array that contains the data. If
    -- the data is a value in an array, the path also indicates which value
    -- contains the data.
    --
    -- If Amazon Macie detects sensitive data in the name of any element in the
    -- path, Macie omits this field. If the name of an element exceeds 20
    -- characters, Macie truncates the name by removing characters from the
    -- beginning of the name. If the resulting full path exceeds 250
    -- characters, Macie also truncates the path, starting with the first
    -- element in the path, until the path contains 250 or fewer characters.
    jsonPath :: Prelude.Maybe Prelude.Text,
    -- | For an Avro object container or Parquet file, the record index, starting
    -- from 0, for the record that contains the sensitive data. For a JSON
    -- Lines file, the line index, starting from 0, for the line that contains
    -- the sensitive data. This value is always 0 for JSON files.
    recordIndex :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Record' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonPath', 'record_jsonPath' - The path, as a JSONPath expression, to the sensitive data. For an Avro
-- object container or Parquet file, this is the path to the field in the
-- record (recordIndex) that contains the data. For a JSON or JSON Lines
-- file, this is the path to the field or array that contains the data. If
-- the data is a value in an array, the path also indicates which value
-- contains the data.
--
-- If Amazon Macie detects sensitive data in the name of any element in the
-- path, Macie omits this field. If the name of an element exceeds 20
-- characters, Macie truncates the name by removing characters from the
-- beginning of the name. If the resulting full path exceeds 250
-- characters, Macie also truncates the path, starting with the first
-- element in the path, until the path contains 250 or fewer characters.
--
-- 'recordIndex', 'record_recordIndex' - For an Avro object container or Parquet file, the record index, starting
-- from 0, for the record that contains the sensitive data. For a JSON
-- Lines file, the line index, starting from 0, for the line that contains
-- the sensitive data. This value is always 0 for JSON files.
newRecord ::
  Record
newRecord =
  Record'
    { jsonPath = Prelude.Nothing,
      recordIndex = Prelude.Nothing
    }

-- | The path, as a JSONPath expression, to the sensitive data. For an Avro
-- object container or Parquet file, this is the path to the field in the
-- record (recordIndex) that contains the data. For a JSON or JSON Lines
-- file, this is the path to the field or array that contains the data. If
-- the data is a value in an array, the path also indicates which value
-- contains the data.
--
-- If Amazon Macie detects sensitive data in the name of any element in the
-- path, Macie omits this field. If the name of an element exceeds 20
-- characters, Macie truncates the name by removing characters from the
-- beginning of the name. If the resulting full path exceeds 250
-- characters, Macie also truncates the path, starting with the first
-- element in the path, until the path contains 250 or fewer characters.
record_jsonPath :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_jsonPath = Lens.lens (\Record' {jsonPath} -> jsonPath) (\s@Record' {} a -> s {jsonPath = a} :: Record)

-- | For an Avro object container or Parquet file, the record index, starting
-- from 0, for the record that contains the sensitive data. For a JSON
-- Lines file, the line index, starting from 0, for the line that contains
-- the sensitive data. This value is always 0 for JSON files.
record_recordIndex :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_recordIndex = Lens.lens (\Record' {recordIndex} -> recordIndex) (\s@Record' {} a -> s {recordIndex = a} :: Record)

instance Data.FromJSON Record where
  parseJSON =
    Data.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Data..:? "jsonPath")
            Prelude.<*> (x Data..:? "recordIndex")
      )

instance Prelude.Hashable Record where
  hashWithSalt _salt Record' {..} =
    _salt
      `Prelude.hashWithSalt` jsonPath
      `Prelude.hashWithSalt` recordIndex

instance Prelude.NFData Record where
  rnf Record' {..} =
    Prelude.rnf jsonPath `Prelude.seq`
      Prelude.rnf recordIndex
