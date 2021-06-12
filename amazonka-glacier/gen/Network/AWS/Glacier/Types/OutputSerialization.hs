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
-- Module      : Network.AWS.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputSerialization where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.CSVOutput
import qualified Network.AWS.Lens as Lens

-- | Describes how the select output is serialized.
--
-- /See:/ 'newOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded query results.
    csv :: Core.Maybe CSVOutput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  OutputSerialization' {csv = Core.Nothing}

-- | Describes the serialization of CSV-encoded query results.
outputSerialization_csv :: Lens.Lens' OutputSerialization (Core.Maybe CSVOutput)
outputSerialization_csv = Lens.lens (\OutputSerialization' {csv} -> csv) (\s@OutputSerialization' {} a -> s {csv = a} :: OutputSerialization)

instance Core.FromJSON OutputSerialization where
  parseJSON =
    Core.withObject
      "OutputSerialization"
      ( \x ->
          OutputSerialization' Core.<$> (x Core..:? "csv")
      )

instance Core.Hashable OutputSerialization

instance Core.NFData OutputSerialization

instance Core.ToJSON OutputSerialization where
  toJSON OutputSerialization' {..} =
    Core.object
      (Core.catMaybes [("csv" Core..=) Core.<$> csv])
