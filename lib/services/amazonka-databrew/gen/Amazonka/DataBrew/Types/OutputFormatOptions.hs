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
-- Module      : Amazonka.DataBrew.Types.OutputFormatOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.OutputFormatOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.CsvOutputOptions
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of options that define the structure of comma-separated
-- (CSV) job output.
--
-- /See:/ 'newOutputFormatOptions' smart constructor.
data OutputFormatOptions = OutputFormatOptions'
  { -- | Represents a set of options that define the structure of comma-separated
    -- value (CSV) job output.
    csv :: Prelude.Maybe CsvOutputOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputFormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'outputFormatOptions_csv' - Represents a set of options that define the structure of comma-separated
-- value (CSV) job output.
newOutputFormatOptions ::
  OutputFormatOptions
newOutputFormatOptions =
  OutputFormatOptions' {csv = Prelude.Nothing}

-- | Represents a set of options that define the structure of comma-separated
-- value (CSV) job output.
outputFormatOptions_csv :: Lens.Lens' OutputFormatOptions (Prelude.Maybe CsvOutputOptions)
outputFormatOptions_csv = Lens.lens (\OutputFormatOptions' {csv} -> csv) (\s@OutputFormatOptions' {} a -> s {csv = a} :: OutputFormatOptions)

instance Data.FromJSON OutputFormatOptions where
  parseJSON =
    Data.withObject
      "OutputFormatOptions"
      ( \x ->
          OutputFormatOptions' Prelude.<$> (x Data..:? "Csv")
      )

instance Prelude.Hashable OutputFormatOptions where
  hashWithSalt _salt OutputFormatOptions' {..} =
    _salt `Prelude.hashWithSalt` csv

instance Prelude.NFData OutputFormatOptions where
  rnf OutputFormatOptions' {..} = Prelude.rnf csv

instance Data.ToJSON OutputFormatOptions where
  toJSON OutputFormatOptions' {..} =
    Data.object
      (Prelude.catMaybes [("Csv" Data..=) Prelude.<$> csv])
