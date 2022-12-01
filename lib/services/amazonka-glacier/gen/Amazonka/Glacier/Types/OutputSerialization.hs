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
-- Module      : Amazonka.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.OutputSerialization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types.CSVOutput
import qualified Amazonka.Prelude as Prelude

-- | Describes how the select output is serialized.
--
-- /See:/ 'newOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded query results.
    csv :: Prelude.Maybe CSVOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputSerialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'outputSerialization_csv' - Describes the serialization of CSV-encoded query results.
newOutputSerialization ::
  OutputSerialization
newOutputSerialization =
  OutputSerialization' {csv = Prelude.Nothing}

-- | Describes the serialization of CSV-encoded query results.
outputSerialization_csv :: Lens.Lens' OutputSerialization (Prelude.Maybe CSVOutput)
outputSerialization_csv = Lens.lens (\OutputSerialization' {csv} -> csv) (\s@OutputSerialization' {} a -> s {csv = a} :: OutputSerialization)

instance Core.FromJSON OutputSerialization where
  parseJSON =
    Core.withObject
      "OutputSerialization"
      ( \x ->
          OutputSerialization' Prelude.<$> (x Core..:? "csv")
      )

instance Prelude.Hashable OutputSerialization where
  hashWithSalt _salt OutputSerialization' {..} =
    _salt `Prelude.hashWithSalt` csv

instance Prelude.NFData OutputSerialization where
  rnf OutputSerialization' {..} = Prelude.rnf csv

instance Core.ToJSON OutputSerialization where
  toJSON OutputSerialization' {..} =
    Core.object
      (Prelude.catMaybes [("csv" Core..=) Prelude.<$> csv])
