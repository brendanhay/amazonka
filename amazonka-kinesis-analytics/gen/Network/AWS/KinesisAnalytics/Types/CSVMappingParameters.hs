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
-- Module      : Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CSVMappingParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    recordRowDelimiter :: Core.Text,
    -- | Column delimiter. For example, in a CSV format, a comma (\",\") is the
    -- typical column delimiter.
    recordColumnDelimiter :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'recordColumnDelimiter'
  Core.Text ->
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
cSVMappingParameters_recordRowDelimiter :: Lens.Lens' CSVMappingParameters Core.Text
cSVMappingParameters_recordRowDelimiter = Lens.lens (\CSVMappingParameters' {recordRowDelimiter} -> recordRowDelimiter) (\s@CSVMappingParameters' {} a -> s {recordRowDelimiter = a} :: CSVMappingParameters)

-- | Column delimiter. For example, in a CSV format, a comma (\",\") is the
-- typical column delimiter.
cSVMappingParameters_recordColumnDelimiter :: Lens.Lens' CSVMappingParameters Core.Text
cSVMappingParameters_recordColumnDelimiter = Lens.lens (\CSVMappingParameters' {recordColumnDelimiter} -> recordColumnDelimiter) (\s@CSVMappingParameters' {} a -> s {recordColumnDelimiter = a} :: CSVMappingParameters)

instance Core.FromJSON CSVMappingParameters where
  parseJSON =
    Core.withObject
      "CSVMappingParameters"
      ( \x ->
          CSVMappingParameters'
            Core.<$> (x Core..: "RecordRowDelimiter")
            Core.<*> (x Core..: "RecordColumnDelimiter")
      )

instance Core.Hashable CSVMappingParameters

instance Core.NFData CSVMappingParameters

instance Core.ToJSON CSVMappingParameters where
  toJSON CSVMappingParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RecordRowDelimiter" Core..= recordRowDelimiter),
            Core.Just
              ( "RecordColumnDelimiter"
                  Core..= recordColumnDelimiter
              )
          ]
      )
