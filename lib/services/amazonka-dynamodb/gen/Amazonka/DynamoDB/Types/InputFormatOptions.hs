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
-- Module      : Amazonka.DynamoDB.Types.InputFormatOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.InputFormatOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.CsvOptions
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | The format options for the data that was imported into the target table.
-- There is one value, CsvOption.
--
-- /See:/ 'newInputFormatOptions' smart constructor.
data InputFormatOptions = InputFormatOptions'
  { -- | The options for imported source files in CSV format. The values are
    -- Delimiter and HeaderList.
    csv :: Prelude.Maybe CsvOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputFormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'inputFormatOptions_csv' - The options for imported source files in CSV format. The values are
-- Delimiter and HeaderList.
newInputFormatOptions ::
  InputFormatOptions
newInputFormatOptions =
  InputFormatOptions' {csv = Prelude.Nothing}

-- | The options for imported source files in CSV format. The values are
-- Delimiter and HeaderList.
inputFormatOptions_csv :: Lens.Lens' InputFormatOptions (Prelude.Maybe CsvOptions)
inputFormatOptions_csv = Lens.lens (\InputFormatOptions' {csv} -> csv) (\s@InputFormatOptions' {} a -> s {csv = a} :: InputFormatOptions)

instance Data.FromJSON InputFormatOptions where
  parseJSON =
    Data.withObject
      "InputFormatOptions"
      ( \x ->
          InputFormatOptions' Prelude.<$> (x Data..:? "Csv")
      )

instance Prelude.Hashable InputFormatOptions where
  hashWithSalt _salt InputFormatOptions' {..} =
    _salt `Prelude.hashWithSalt` csv

instance Prelude.NFData InputFormatOptions where
  rnf InputFormatOptions' {..} = Prelude.rnf csv

instance Data.ToJSON InputFormatOptions where
  toJSON InputFormatOptions' {..} =
    Data.object
      (Prelude.catMaybes [("Csv" Data..=) Prelude.<$> csv])
