{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Athena.Types.Datum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Datum where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A piece of data (a field in the table).
--
-- /See:/ 'newDatum' smart constructor.
data Datum = Datum'
  { -- | The value of the datum.
    varCharValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Datum where
  parseJSON =
    Prelude.withObject
      "Datum"
      ( \x ->
          Datum' Prelude.<$> (x Prelude..:? "VarCharValue")
      )

instance Prelude.Hashable Datum

instance Prelude.NFData Datum
