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
-- Module      : Amazonka.SecurityHub.Types.Record
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Record where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An occurrence of sensitive data in an Apache Avro object container or an
-- Apache Parquet file.
--
-- /See:/ 'newRecord' smart constructor.
data Record = Record'
  { -- | The path, as a JSONPath expression, to the field in the record that
    -- contains the data. If the field name is longer than 20 characters, it is
    -- truncated. If the path is longer than 250 characters, it is truncated.
    jsonPath :: Prelude.Maybe Prelude.Text,
    -- | The record index, starting from 0, for the record that contains the
    -- data.
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
-- 'jsonPath', 'record_jsonPath' - The path, as a JSONPath expression, to the field in the record that
-- contains the data. If the field name is longer than 20 characters, it is
-- truncated. If the path is longer than 250 characters, it is truncated.
--
-- 'recordIndex', 'record_recordIndex' - The record index, starting from 0, for the record that contains the
-- data.
newRecord ::
  Record
newRecord =
  Record'
    { jsonPath = Prelude.Nothing,
      recordIndex = Prelude.Nothing
    }

-- | The path, as a JSONPath expression, to the field in the record that
-- contains the data. If the field name is longer than 20 characters, it is
-- truncated. If the path is longer than 250 characters, it is truncated.
record_jsonPath :: Lens.Lens' Record (Prelude.Maybe Prelude.Text)
record_jsonPath = Lens.lens (\Record' {jsonPath} -> jsonPath) (\s@Record' {} a -> s {jsonPath = a} :: Record)

-- | The record index, starting from 0, for the record that contains the
-- data.
record_recordIndex :: Lens.Lens' Record (Prelude.Maybe Prelude.Integer)
record_recordIndex = Lens.lens (\Record' {recordIndex} -> recordIndex) (\s@Record' {} a -> s {recordIndex = a} :: Record)

instance Core.FromJSON Record where
  parseJSON =
    Core.withObject
      "Record"
      ( \x ->
          Record'
            Prelude.<$> (x Core..:? "JsonPath")
            Prelude.<*> (x Core..:? "RecordIndex")
      )

instance Prelude.Hashable Record where
  hashWithSalt _salt Record' {..} =
    _salt `Prelude.hashWithSalt` jsonPath
      `Prelude.hashWithSalt` recordIndex

instance Prelude.NFData Record where
  rnf Record' {..} =
    Prelude.rnf jsonPath
      `Prelude.seq` Prelude.rnf recordIndex

instance Core.ToJSON Record where
  toJSON Record' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("JsonPath" Core..=) Prelude.<$> jsonPath,
            ("RecordIndex" Core..=) Prelude.<$> recordIndex
          ]
      )
