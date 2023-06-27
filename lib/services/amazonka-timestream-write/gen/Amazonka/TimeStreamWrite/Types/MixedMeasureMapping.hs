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
-- Module      : Amazonka.TimeStreamWrite.Types.MixedMeasureMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MixedMeasureMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.MeasureValueType
import Amazonka.TimeStreamWrite.Types.MultiMeasureAttributeMapping

-- |
--
-- /See:/ 'newMixedMeasureMapping' smart constructor.
data MixedMeasureMapping = MixedMeasureMapping'
  { measureName :: Prelude.Maybe Prelude.Text,
    multiMeasureAttributeMappings :: Prelude.Maybe (Prelude.NonEmpty MultiMeasureAttributeMapping),
    sourceColumn :: Prelude.Maybe Prelude.Text,
    targetMeasureName :: Prelude.Maybe Prelude.Text,
    measureValueType :: MeasureValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MixedMeasureMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'measureName', 'mixedMeasureMapping_measureName' -
--
-- 'multiMeasureAttributeMappings', 'mixedMeasureMapping_multiMeasureAttributeMappings' -
--
-- 'sourceColumn', 'mixedMeasureMapping_sourceColumn' -
--
-- 'targetMeasureName', 'mixedMeasureMapping_targetMeasureName' -
--
-- 'measureValueType', 'mixedMeasureMapping_measureValueType' -
newMixedMeasureMapping ::
  -- | 'measureValueType'
  MeasureValueType ->
  MixedMeasureMapping
newMixedMeasureMapping pMeasureValueType_ =
  MixedMeasureMapping'
    { measureName = Prelude.Nothing,
      multiMeasureAttributeMappings = Prelude.Nothing,
      sourceColumn = Prelude.Nothing,
      targetMeasureName = Prelude.Nothing,
      measureValueType = pMeasureValueType_
    }

mixedMeasureMapping_measureName :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_measureName = Lens.lens (\MixedMeasureMapping' {measureName} -> measureName) (\s@MixedMeasureMapping' {} a -> s {measureName = a} :: MixedMeasureMapping)

mixedMeasureMapping_multiMeasureAttributeMappings :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe (Prelude.NonEmpty MultiMeasureAttributeMapping))
mixedMeasureMapping_multiMeasureAttributeMappings = Lens.lens (\MixedMeasureMapping' {multiMeasureAttributeMappings} -> multiMeasureAttributeMappings) (\s@MixedMeasureMapping' {} a -> s {multiMeasureAttributeMappings = a} :: MixedMeasureMapping) Prelude.. Lens.mapping Lens.coerced

mixedMeasureMapping_sourceColumn :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_sourceColumn = Lens.lens (\MixedMeasureMapping' {sourceColumn} -> sourceColumn) (\s@MixedMeasureMapping' {} a -> s {sourceColumn = a} :: MixedMeasureMapping)

mixedMeasureMapping_targetMeasureName :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_targetMeasureName = Lens.lens (\MixedMeasureMapping' {targetMeasureName} -> targetMeasureName) (\s@MixedMeasureMapping' {} a -> s {targetMeasureName = a} :: MixedMeasureMapping)

mixedMeasureMapping_measureValueType :: Lens.Lens' MixedMeasureMapping MeasureValueType
mixedMeasureMapping_measureValueType = Lens.lens (\MixedMeasureMapping' {measureValueType} -> measureValueType) (\s@MixedMeasureMapping' {} a -> s {measureValueType = a} :: MixedMeasureMapping)

instance Data.FromJSON MixedMeasureMapping where
  parseJSON =
    Data.withObject
      "MixedMeasureMapping"
      ( \x ->
          MixedMeasureMapping'
            Prelude.<$> (x Data..:? "MeasureName")
            Prelude.<*> (x Data..:? "MultiMeasureAttributeMappings")
            Prelude.<*> (x Data..:? "SourceColumn")
            Prelude.<*> (x Data..:? "TargetMeasureName")
            Prelude.<*> (x Data..: "MeasureValueType")
      )

instance Prelude.Hashable MixedMeasureMapping where
  hashWithSalt _salt MixedMeasureMapping' {..} =
    _salt
      `Prelude.hashWithSalt` measureName
      `Prelude.hashWithSalt` multiMeasureAttributeMappings
      `Prelude.hashWithSalt` sourceColumn
      `Prelude.hashWithSalt` targetMeasureName
      `Prelude.hashWithSalt` measureValueType

instance Prelude.NFData MixedMeasureMapping where
  rnf MixedMeasureMapping' {..} =
    Prelude.rnf measureName
      `Prelude.seq` Prelude.rnf multiMeasureAttributeMappings
      `Prelude.seq` Prelude.rnf sourceColumn
      `Prelude.seq` Prelude.rnf targetMeasureName
      `Prelude.seq` Prelude.rnf measureValueType

instance Data.ToJSON MixedMeasureMapping where
  toJSON MixedMeasureMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MeasureName" Data..=) Prelude.<$> measureName,
            ("MultiMeasureAttributeMappings" Data..=)
              Prelude.<$> multiMeasureAttributeMappings,
            ("SourceColumn" Data..=) Prelude.<$> sourceColumn,
            ("TargetMeasureName" Data..=)
              Prelude.<$> targetMeasureName,
            Prelude.Just
              ("MeasureValueType" Data..= measureValueType)
          ]
      )
