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
-- Module      : Amazonka.DataBrew.Types.FormatOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.FormatOptions where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.CsvOptions
import Amazonka.DataBrew.Types.ExcelOptions
import Amazonka.DataBrew.Types.JsonOptions
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a set of options that define the structure of either
-- comma-separated value (CSV), Excel, or JSON input.
--
-- /See:/ 'newFormatOptions' smart constructor.
data FormatOptions = FormatOptions'
  { -- | Options that define how JSON input is to be interpreted by DataBrew.
    json :: Prelude.Maybe JsonOptions,
    -- | Options that define how CSV input is to be interpreted by DataBrew.
    csv :: Prelude.Maybe CsvOptions,
    -- | Options that define how Excel input is to be interpreted by DataBrew.
    excel :: Prelude.Maybe ExcelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormatOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'json', 'formatOptions_json' - Options that define how JSON input is to be interpreted by DataBrew.
--
-- 'csv', 'formatOptions_csv' - Options that define how CSV input is to be interpreted by DataBrew.
--
-- 'excel', 'formatOptions_excel' - Options that define how Excel input is to be interpreted by DataBrew.
newFormatOptions ::
  FormatOptions
newFormatOptions =
  FormatOptions'
    { json = Prelude.Nothing,
      csv = Prelude.Nothing,
      excel = Prelude.Nothing
    }

-- | Options that define how JSON input is to be interpreted by DataBrew.
formatOptions_json :: Lens.Lens' FormatOptions (Prelude.Maybe JsonOptions)
formatOptions_json = Lens.lens (\FormatOptions' {json} -> json) (\s@FormatOptions' {} a -> s {json = a} :: FormatOptions)

-- | Options that define how CSV input is to be interpreted by DataBrew.
formatOptions_csv :: Lens.Lens' FormatOptions (Prelude.Maybe CsvOptions)
formatOptions_csv = Lens.lens (\FormatOptions' {csv} -> csv) (\s@FormatOptions' {} a -> s {csv = a} :: FormatOptions)

-- | Options that define how Excel input is to be interpreted by DataBrew.
formatOptions_excel :: Lens.Lens' FormatOptions (Prelude.Maybe ExcelOptions)
formatOptions_excel = Lens.lens (\FormatOptions' {excel} -> excel) (\s@FormatOptions' {} a -> s {excel = a} :: FormatOptions)

instance Core.FromJSON FormatOptions where
  parseJSON =
    Core.withObject
      "FormatOptions"
      ( \x ->
          FormatOptions'
            Prelude.<$> (x Core..:? "Json")
            Prelude.<*> (x Core..:? "Csv")
            Prelude.<*> (x Core..:? "Excel")
      )

instance Prelude.Hashable FormatOptions where
  hashWithSalt salt' FormatOptions' {..} =
    salt' `Prelude.hashWithSalt` excel
      `Prelude.hashWithSalt` csv
      `Prelude.hashWithSalt` json

instance Prelude.NFData FormatOptions where
  rnf FormatOptions' {..} =
    Prelude.rnf json `Prelude.seq` Prelude.rnf excel
      `Prelude.seq` Prelude.rnf csv

instance Core.ToJSON FormatOptions where
  toJSON FormatOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Json" Core..=) Prelude.<$> json,
            ("Csv" Core..=) Prelude.<$> csv,
            ("Excel" Core..=) Prelude.<$> excel
          ]
      )
