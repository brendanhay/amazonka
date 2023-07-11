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
-- Module      : Amazonka.KinesisAnalytics.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.CSVMappingParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides additional mapping information when the record format uses
-- delimiters, such as CSV. For example, the following sample records use
-- CSV format, where the records use the /\'\\n\'/ as the row delimiter and
-- a comma (\",\") as the column delimiter:
--
-- @\"name1\", \"address1\"@
--
-- @\"name2\", \"address2\"@
--
-- /See:/ 'newCSVMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { -- | Row delimiter. For example, in a CSV format, /\'\\n\'/ is the typical
    -- row delimiter.
    recordRowDelimiter :: Prelude.Text,
    -- | Column delimiter. For example, in a CSV format, a comma (\",\") is the
    -- typical column delimiter.
    recordColumnDelimiter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CSVMappingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordRowDelimiter', 'cSVMappingParameters_recordRowDelimiter' - Row delimiter. For example, in a CSV format, /\'\\n\'/ is the typical
-- row delimiter.
--
-- 'recordColumnDelimiter', 'cSVMappingParameters_recordColumnDelimiter' - Column delimiter. For example, in a CSV format, a comma (\",\") is the
-- typical column delimiter.
newCSVMappingParameters ::
  -- | 'recordRowDelimiter'
  Prelude.Text ->
  -- | 'recordColumnDelimiter'
  Prelude.Text ->
  CSVMappingParameters
newCSVMappingParameters
  pRecordRowDelimiter_
  pRecordColumnDelimiter_ =
    CSVMappingParameters'
      { recordRowDelimiter =
          pRecordRowDelimiter_,
        recordColumnDelimiter = pRecordColumnDelimiter_
      }

-- | Row delimiter. For example, in a CSV format, /\'\\n\'/ is the typical
-- row delimiter.
cSVMappingParameters_recordRowDelimiter :: Lens.Lens' CSVMappingParameters Prelude.Text
cSVMappingParameters_recordRowDelimiter = Lens.lens (\CSVMappingParameters' {recordRowDelimiter} -> recordRowDelimiter) (\s@CSVMappingParameters' {} a -> s {recordRowDelimiter = a} :: CSVMappingParameters)

-- | Column delimiter. For example, in a CSV format, a comma (\",\") is the
-- typical column delimiter.
cSVMappingParameters_recordColumnDelimiter :: Lens.Lens' CSVMappingParameters Prelude.Text
cSVMappingParameters_recordColumnDelimiter = Lens.lens (\CSVMappingParameters' {recordColumnDelimiter} -> recordColumnDelimiter) (\s@CSVMappingParameters' {} a -> s {recordColumnDelimiter = a} :: CSVMappingParameters)

instance Data.FromJSON CSVMappingParameters where
  parseJSON =
    Data.withObject
      "CSVMappingParameters"
      ( \x ->
          CSVMappingParameters'
            Prelude.<$> (x Data..: "RecordRowDelimiter")
            Prelude.<*> (x Data..: "RecordColumnDelimiter")
      )

instance Prelude.Hashable CSVMappingParameters where
  hashWithSalt _salt CSVMappingParameters' {..} =
    _salt
      `Prelude.hashWithSalt` recordRowDelimiter
      `Prelude.hashWithSalt` recordColumnDelimiter

instance Prelude.NFData CSVMappingParameters where
  rnf CSVMappingParameters' {..} =
    Prelude.rnf recordRowDelimiter
      `Prelude.seq` Prelude.rnf recordColumnDelimiter

instance Data.ToJSON CSVMappingParameters where
  toJSON CSVMappingParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RecordRowDelimiter" Data..= recordRowDelimiter),
            Prelude.Just
              ( "RecordColumnDelimiter"
                  Data..= recordColumnDelimiter
              )
          ]
      )
