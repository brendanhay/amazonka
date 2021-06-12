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
-- Module      : Network.AWS.Glacier.Types.InputSerialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InputSerialization where

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.Types.CSVInput
import qualified Network.AWS.Lens as Lens

-- | Describes how the archive is serialized.
--
-- /See:/ 'newInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { -- | Describes the serialization of a CSV-encoded object.
    csv :: Core.Maybe CSVInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputSerialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'inputSerialization_csv' - Describes the serialization of a CSV-encoded object.
newInputSerialization ::
  InputSerialization
newInputSerialization =
  InputSerialization' {csv = Core.Nothing}

-- | Describes the serialization of a CSV-encoded object.
inputSerialization_csv :: Lens.Lens' InputSerialization (Core.Maybe CSVInput)
inputSerialization_csv = Lens.lens (\InputSerialization' {csv} -> csv) (\s@InputSerialization' {} a -> s {csv = a} :: InputSerialization)

instance Core.FromJSON InputSerialization where
  parseJSON =
    Core.withObject
      "InputSerialization"
      ( \x ->
          InputSerialization' Core.<$> (x Core..:? "csv")
      )

instance Core.Hashable InputSerialization

instance Core.NFData InputSerialization

instance Core.ToJSON InputSerialization where
  toJSON InputSerialization' {..} =
    Core.object
      (Core.catMaybes [("csv" Core..=) Core.<$> csv])
