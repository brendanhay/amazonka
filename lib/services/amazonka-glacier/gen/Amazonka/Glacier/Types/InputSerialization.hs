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
-- Module      : Amazonka.Glacier.Types.InputSerialization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.InputSerialization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types.CSVInput
import qualified Amazonka.Prelude as Prelude

-- | Describes how the archive is serialized.
--
-- /See:/ 'newInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { -- | Describes the serialization of a CSV-encoded object.
    csv :: Prelude.Maybe CSVInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  InputSerialization' {csv = Prelude.Nothing}

-- | Describes the serialization of a CSV-encoded object.
inputSerialization_csv :: Lens.Lens' InputSerialization (Prelude.Maybe CSVInput)
inputSerialization_csv = Lens.lens (\InputSerialization' {csv} -> csv) (\s@InputSerialization' {} a -> s {csv = a} :: InputSerialization)

instance Data.FromJSON InputSerialization where
  parseJSON =
    Data.withObject
      "InputSerialization"
      ( \x ->
          InputSerialization' Prelude.<$> (x Data..:? "csv")
      )

instance Prelude.Hashable InputSerialization where
  hashWithSalt _salt InputSerialization' {..} =
    _salt `Prelude.hashWithSalt` csv

instance Prelude.NFData InputSerialization where
  rnf InputSerialization' {..} = Prelude.rnf csv

instance Data.ToJSON InputSerialization where
  toJSON InputSerialization' {..} =
    Data.object
      (Prelude.catMaybes [("csv" Data..=) Prelude.<$> csv])
