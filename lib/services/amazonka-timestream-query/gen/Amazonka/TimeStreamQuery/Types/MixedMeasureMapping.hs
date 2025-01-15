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
-- Module      : Amazonka.TimeStreamQuery.Types.MixedMeasureMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.MixedMeasureMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.MeasureValueType
import Amazonka.TimeStreamQuery.Types.MultiMeasureAttributeMapping

-- | MixedMeasureMappings are mappings that can be used to ingest data into a
-- mixture of narrow and multi measures in the derived table.
--
-- /See:/ 'newMixedMeasureMapping' smart constructor.
data MixedMeasureMapping = MixedMeasureMapping'
  { -- | Refers to the value of measure_name in a result row. This field is
    -- required if MeasureNameColumn is provided.
    measureName :: Prelude.Maybe Prelude.Text,
    -- | Required when measureValueType is MULTI. Attribute mappings for MULTI
    -- value measures.
    multiMeasureAttributeMappings :: Prelude.Maybe (Prelude.NonEmpty MultiMeasureAttributeMapping),
    -- | This field refers to the source column from which measure-value is to be
    -- read for result materialization.
    sourceColumn :: Prelude.Maybe Prelude.Text,
    -- | Target measure name to be used. If not provided, the target measure name
    -- by default would be measure-name if provided, or sourceColumn otherwise.
    targetMeasureName :: Prelude.Maybe Prelude.Text,
    -- | Type of the value that is to be read from sourceColumn. If the mapping
    -- is for MULTI, use MeasureValueType.MULTI.
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
-- 'measureName', 'mixedMeasureMapping_measureName' - Refers to the value of measure_name in a result row. This field is
-- required if MeasureNameColumn is provided.
--
-- 'multiMeasureAttributeMappings', 'mixedMeasureMapping_multiMeasureAttributeMappings' - Required when measureValueType is MULTI. Attribute mappings for MULTI
-- value measures.
--
-- 'sourceColumn', 'mixedMeasureMapping_sourceColumn' - This field refers to the source column from which measure-value is to be
-- read for result materialization.
--
-- 'targetMeasureName', 'mixedMeasureMapping_targetMeasureName' - Target measure name to be used. If not provided, the target measure name
-- by default would be measure-name if provided, or sourceColumn otherwise.
--
-- 'measureValueType', 'mixedMeasureMapping_measureValueType' - Type of the value that is to be read from sourceColumn. If the mapping
-- is for MULTI, use MeasureValueType.MULTI.
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

-- | Refers to the value of measure_name in a result row. This field is
-- required if MeasureNameColumn is provided.
mixedMeasureMapping_measureName :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_measureName = Lens.lens (\MixedMeasureMapping' {measureName} -> measureName) (\s@MixedMeasureMapping' {} a -> s {measureName = a} :: MixedMeasureMapping)

-- | Required when measureValueType is MULTI. Attribute mappings for MULTI
-- value measures.
mixedMeasureMapping_multiMeasureAttributeMappings :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe (Prelude.NonEmpty MultiMeasureAttributeMapping))
mixedMeasureMapping_multiMeasureAttributeMappings = Lens.lens (\MixedMeasureMapping' {multiMeasureAttributeMappings} -> multiMeasureAttributeMappings) (\s@MixedMeasureMapping' {} a -> s {multiMeasureAttributeMappings = a} :: MixedMeasureMapping) Prelude.. Lens.mapping Lens.coerced

-- | This field refers to the source column from which measure-value is to be
-- read for result materialization.
mixedMeasureMapping_sourceColumn :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_sourceColumn = Lens.lens (\MixedMeasureMapping' {sourceColumn} -> sourceColumn) (\s@MixedMeasureMapping' {} a -> s {sourceColumn = a} :: MixedMeasureMapping)

-- | Target measure name to be used. If not provided, the target measure name
-- by default would be measure-name if provided, or sourceColumn otherwise.
mixedMeasureMapping_targetMeasureName :: Lens.Lens' MixedMeasureMapping (Prelude.Maybe Prelude.Text)
mixedMeasureMapping_targetMeasureName = Lens.lens (\MixedMeasureMapping' {targetMeasureName} -> targetMeasureName) (\s@MixedMeasureMapping' {} a -> s {targetMeasureName = a} :: MixedMeasureMapping)

-- | Type of the value that is to be read from sourceColumn. If the mapping
-- is for MULTI, use MeasureValueType.MULTI.
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
    Prelude.rnf measureName `Prelude.seq`
      Prelude.rnf multiMeasureAttributeMappings `Prelude.seq`
        Prelude.rnf sourceColumn `Prelude.seq`
          Prelude.rnf targetMeasureName `Prelude.seq`
            Prelude.rnf measureValueType

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
