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
-- Module      : Amazonka.TimeStreamQuery.Types.MultiMeasureAttributeMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.MultiMeasureAttributeMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.ScalarMeasureValueType

-- | Attribute mapping for MULTI value measures.
--
-- /See:/ 'newMultiMeasureAttributeMapping' smart constructor.
data MultiMeasureAttributeMapping = MultiMeasureAttributeMapping'
  { -- | Custom name to be used for attribute name in derived table. If not
    -- provided, source column name would be used.
    targetMultiMeasureAttributeName :: Prelude.Maybe Prelude.Text,
    -- | Source column from where the attribute value is to be read.
    sourceColumn :: Prelude.Text,
    -- | Type of the attribute to be read from the source column.
    measureValueType :: ScalarMeasureValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiMeasureAttributeMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetMultiMeasureAttributeName', 'multiMeasureAttributeMapping_targetMultiMeasureAttributeName' - Custom name to be used for attribute name in derived table. If not
-- provided, source column name would be used.
--
-- 'sourceColumn', 'multiMeasureAttributeMapping_sourceColumn' - Source column from where the attribute value is to be read.
--
-- 'measureValueType', 'multiMeasureAttributeMapping_measureValueType' - Type of the attribute to be read from the source column.
newMultiMeasureAttributeMapping ::
  -- | 'sourceColumn'
  Prelude.Text ->
  -- | 'measureValueType'
  ScalarMeasureValueType ->
  MultiMeasureAttributeMapping
newMultiMeasureAttributeMapping
  pSourceColumn_
  pMeasureValueType_ =
    MultiMeasureAttributeMapping'
      { targetMultiMeasureAttributeName =
          Prelude.Nothing,
        sourceColumn = pSourceColumn_,
        measureValueType = pMeasureValueType_
      }

-- | Custom name to be used for attribute name in derived table. If not
-- provided, source column name would be used.
multiMeasureAttributeMapping_targetMultiMeasureAttributeName :: Lens.Lens' MultiMeasureAttributeMapping (Prelude.Maybe Prelude.Text)
multiMeasureAttributeMapping_targetMultiMeasureAttributeName = Lens.lens (\MultiMeasureAttributeMapping' {targetMultiMeasureAttributeName} -> targetMultiMeasureAttributeName) (\s@MultiMeasureAttributeMapping' {} a -> s {targetMultiMeasureAttributeName = a} :: MultiMeasureAttributeMapping)

-- | Source column from where the attribute value is to be read.
multiMeasureAttributeMapping_sourceColumn :: Lens.Lens' MultiMeasureAttributeMapping Prelude.Text
multiMeasureAttributeMapping_sourceColumn = Lens.lens (\MultiMeasureAttributeMapping' {sourceColumn} -> sourceColumn) (\s@MultiMeasureAttributeMapping' {} a -> s {sourceColumn = a} :: MultiMeasureAttributeMapping)

-- | Type of the attribute to be read from the source column.
multiMeasureAttributeMapping_measureValueType :: Lens.Lens' MultiMeasureAttributeMapping ScalarMeasureValueType
multiMeasureAttributeMapping_measureValueType = Lens.lens (\MultiMeasureAttributeMapping' {measureValueType} -> measureValueType) (\s@MultiMeasureAttributeMapping' {} a -> s {measureValueType = a} :: MultiMeasureAttributeMapping)

instance Data.FromJSON MultiMeasureAttributeMapping where
  parseJSON =
    Data.withObject
      "MultiMeasureAttributeMapping"
      ( \x ->
          MultiMeasureAttributeMapping'
            Prelude.<$> (x Data..:? "TargetMultiMeasureAttributeName")
            Prelude.<*> (x Data..: "SourceColumn")
            Prelude.<*> (x Data..: "MeasureValueType")
      )

instance
  Prelude.Hashable
    MultiMeasureAttributeMapping
  where
  hashWithSalt _salt MultiMeasureAttributeMapping' {..} =
    _salt
      `Prelude.hashWithSalt` targetMultiMeasureAttributeName
      `Prelude.hashWithSalt` sourceColumn
      `Prelude.hashWithSalt` measureValueType

instance Prelude.NFData MultiMeasureAttributeMapping where
  rnf MultiMeasureAttributeMapping' {..} =
    Prelude.rnf targetMultiMeasureAttributeName `Prelude.seq`
      Prelude.rnf sourceColumn `Prelude.seq`
        Prelude.rnf measureValueType

instance Data.ToJSON MultiMeasureAttributeMapping where
  toJSON MultiMeasureAttributeMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetMultiMeasureAttributeName" Data..=)
              Prelude.<$> targetMultiMeasureAttributeName,
            Prelude.Just ("SourceColumn" Data..= sourceColumn),
            Prelude.Just
              ("MeasureValueType" Data..= measureValueType)
          ]
      )
