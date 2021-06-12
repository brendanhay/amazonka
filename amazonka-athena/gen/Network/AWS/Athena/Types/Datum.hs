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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A piece of data (a field in the table).
--
-- /See:/ 'newDatum' smart constructor.
data Datum = Datum'
  { -- | The value of the datum.
    varCharValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newDatum = Datum' {varCharValue = Core.Nothing}

-- | The value of the datum.
datum_varCharValue :: Lens.Lens' Datum (Core.Maybe Core.Text)
datum_varCharValue = Lens.lens (\Datum' {varCharValue} -> varCharValue) (\s@Datum' {} a -> s {varCharValue = a} :: Datum)

instance Core.FromJSON Datum where
  parseJSON =
    Core.withObject
      "Datum"
      (\x -> Datum' Core.<$> (x Core..:? "VarCharValue"))

instance Core.Hashable Datum

instance Core.NFData Datum
