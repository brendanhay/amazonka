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
-- Module      : Amazonka.TimeStreamWrite.Types.DimensionMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.DimensionMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newDimensionMapping' smart constructor.
data DimensionMapping = DimensionMapping'
  { destinationColumn :: Prelude.Maybe Prelude.Text,
    sourceColumn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationColumn', 'dimensionMapping_destinationColumn' -
--
-- 'sourceColumn', 'dimensionMapping_sourceColumn' -
newDimensionMapping ::
  DimensionMapping
newDimensionMapping =
  DimensionMapping'
    { destinationColumn =
        Prelude.Nothing,
      sourceColumn = Prelude.Nothing
    }

dimensionMapping_destinationColumn :: Lens.Lens' DimensionMapping (Prelude.Maybe Prelude.Text)
dimensionMapping_destinationColumn = Lens.lens (\DimensionMapping' {destinationColumn} -> destinationColumn) (\s@DimensionMapping' {} a -> s {destinationColumn = a} :: DimensionMapping)

dimensionMapping_sourceColumn :: Lens.Lens' DimensionMapping (Prelude.Maybe Prelude.Text)
dimensionMapping_sourceColumn = Lens.lens (\DimensionMapping' {sourceColumn} -> sourceColumn) (\s@DimensionMapping' {} a -> s {sourceColumn = a} :: DimensionMapping)

instance Data.FromJSON DimensionMapping where
  parseJSON =
    Data.withObject
      "DimensionMapping"
      ( \x ->
          DimensionMapping'
            Prelude.<$> (x Data..:? "DestinationColumn")
            Prelude.<*> (x Data..:? "SourceColumn")
      )

instance Prelude.Hashable DimensionMapping where
  hashWithSalt _salt DimensionMapping' {..} =
    _salt
      `Prelude.hashWithSalt` destinationColumn
      `Prelude.hashWithSalt` sourceColumn

instance Prelude.NFData DimensionMapping where
  rnf DimensionMapping' {..} =
    Prelude.rnf destinationColumn
      `Prelude.seq` Prelude.rnf sourceColumn

instance Data.ToJSON DimensionMapping where
  toJSON DimensionMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationColumn" Data..=)
              Prelude.<$> destinationColumn,
            ("SourceColumn" Data..=) Prelude.<$> sourceColumn
          ]
      )
