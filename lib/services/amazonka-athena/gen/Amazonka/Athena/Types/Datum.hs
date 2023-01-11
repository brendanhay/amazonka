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
-- Module      : Amazonka.Athena.Types.Datum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.Datum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A piece of data (a field in the table).
--
-- /See:/ 'newDatum' smart constructor.
data Datum = Datum'
  { -- | The value of the datum.
    varCharValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Datum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'varCharValue', 'datum_varCharValue' - The value of the datum.
newDatum ::
  Datum
newDatum = Datum' {varCharValue = Prelude.Nothing}

-- | The value of the datum.
datum_varCharValue :: Lens.Lens' Datum (Prelude.Maybe Prelude.Text)
datum_varCharValue = Lens.lens (\Datum' {varCharValue} -> varCharValue) (\s@Datum' {} a -> s {varCharValue = a} :: Datum)

instance Data.FromJSON Datum where
  parseJSON =
    Data.withObject
      "Datum"
      ( \x ->
          Datum' Prelude.<$> (x Data..:? "VarCharValue")
      )

instance Prelude.Hashable Datum where
  hashWithSalt _salt Datum' {..} =
    _salt `Prelude.hashWithSalt` varCharValue

instance Prelude.NFData Datum where
  rnf Datum' {..} = Prelude.rnf varCharValue
