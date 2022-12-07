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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CSVMappingParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, provides additional
-- mapping information when the record format uses delimiters, such as CSV.
-- For example, the following sample records use CSV format, where the
-- records use the /\'\\n\'/ as the row delimiter and a comma (\",\") as
-- the column delimiter:
--
-- @\"name1\", \"address1\"@
--
-- @\"name2\", \"address2\"@
--
-- /See:/ 'newCSVMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { -- | The row delimiter. For example, in a CSV format, /\'\\n\'/ is the
    -- typical row delimiter.
    recordRowDelimiter :: Prelude.Text,
    -- | The column delimiter. For example, in a CSV format, a comma (\",\") is
    -- the typical column delimiter.
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
-- 'recordRowDelimiter', 'cSVMappingParameters_recordRowDelimiter' - The row delimiter. For example, in a CSV format, /\'\\n\'/ is the
-- typical row delimiter.
--
-- 'recordColumnDelimiter', 'cSVMappingParameters_recordColumnDelimiter' - The column delimiter. For example, in a CSV format, a comma (\",\") is
-- the typical column delimiter.
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

-- | The row delimiter. For example, in a CSV format, /\'\\n\'/ is the
-- typical row delimiter.
cSVMappingParameters_recordRowDelimiter :: Lens.Lens' CSVMappingParameters Prelude.Text
cSVMappingParameters_recordRowDelimiter = Lens.lens (\CSVMappingParameters' {recordRowDelimiter} -> recordRowDelimiter) (\s@CSVMappingParameters' {} a -> s {recordRowDelimiter = a} :: CSVMappingParameters)

-- | The column delimiter. For example, in a CSV format, a comma (\",\") is
-- the typical column delimiter.
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
    _salt `Prelude.hashWithSalt` recordRowDelimiter
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
