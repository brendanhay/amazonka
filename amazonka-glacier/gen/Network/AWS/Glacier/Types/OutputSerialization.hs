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
-- Module      : Network.AWS.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputSerialization where

import Network.AWS.Glacier.Types.CSVOutput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes how the select output is serialized.
--
-- /See:/ 'newOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded query results.
    csv :: Prelude.Maybe CSVOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON OutputSerialization where
  parseJSON =
    Prelude.withObject
      "OutputSerialization"
      ( \x ->
          OutputSerialization'
            Prelude.<$> (x Prelude..:? "csv")
      )

instance Prelude.Hashable OutputSerialization

instance Prelude.NFData OutputSerialization

instance Prelude.ToJSON OutputSerialization where
  toJSON OutputSerialization' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("csv" Prelude..=) Prelude.<$> csv]
      )
