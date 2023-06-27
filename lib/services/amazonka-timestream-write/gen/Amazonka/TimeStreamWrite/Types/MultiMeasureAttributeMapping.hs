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
-- Module      : Amazonka.TimeStreamWrite.Types.MultiMeasureAttributeMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.MultiMeasureAttributeMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.ScalarMeasureValueType

-- |
--
-- /See:/ 'newMultiMeasureAttributeMapping' smart constructor.
data MultiMeasureAttributeMapping = MultiMeasureAttributeMapping'
  { measureValueType :: Prelude.Maybe ScalarMeasureValueType,
    targetMultiMeasureAttributeName :: Prelude.Maybe Prelude.Text,
    sourceColumn :: Prelude.Text
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
-- 'measureValueType', 'multiMeasureAttributeMapping_measureValueType' -
--
-- 'targetMultiMeasureAttributeName', 'multiMeasureAttributeMapping_targetMultiMeasureAttributeName' -
--
-- 'sourceColumn', 'multiMeasureAttributeMapping_sourceColumn' -
newMultiMeasureAttributeMapping ::
  -- | 'sourceColumn'
  Prelude.Text ->
  MultiMeasureAttributeMapping
newMultiMeasureAttributeMapping pSourceColumn_ =
  MultiMeasureAttributeMapping'
    { measureValueType =
        Prelude.Nothing,
      targetMultiMeasureAttributeName =
        Prelude.Nothing,
      sourceColumn = pSourceColumn_
    }

multiMeasureAttributeMapping_measureValueType :: Lens.Lens' MultiMeasureAttributeMapping (Prelude.Maybe ScalarMeasureValueType)
multiMeasureAttributeMapping_measureValueType = Lens.lens (\MultiMeasureAttributeMapping' {measureValueType} -> measureValueType) (\s@MultiMeasureAttributeMapping' {} a -> s {measureValueType = a} :: MultiMeasureAttributeMapping)

multiMeasureAttributeMapping_targetMultiMeasureAttributeName :: Lens.Lens' MultiMeasureAttributeMapping (Prelude.Maybe Prelude.Text)
multiMeasureAttributeMapping_targetMultiMeasureAttributeName = Lens.lens (\MultiMeasureAttributeMapping' {targetMultiMeasureAttributeName} -> targetMultiMeasureAttributeName) (\s@MultiMeasureAttributeMapping' {} a -> s {targetMultiMeasureAttributeName = a} :: MultiMeasureAttributeMapping)

multiMeasureAttributeMapping_sourceColumn :: Lens.Lens' MultiMeasureAttributeMapping Prelude.Text
multiMeasureAttributeMapping_sourceColumn = Lens.lens (\MultiMeasureAttributeMapping' {sourceColumn} -> sourceColumn) (\s@MultiMeasureAttributeMapping' {} a -> s {sourceColumn = a} :: MultiMeasureAttributeMapping)

instance Data.FromJSON MultiMeasureAttributeMapping where
  parseJSON =
    Data.withObject
      "MultiMeasureAttributeMapping"
      ( \x ->
          MultiMeasureAttributeMapping'
            Prelude.<$> (x Data..:? "MeasureValueType")
            Prelude.<*> (x Data..:? "TargetMultiMeasureAttributeName")
            Prelude.<*> (x Data..: "SourceColumn")
      )

instance
  Prelude.Hashable
    MultiMeasureAttributeMapping
  where
  hashWithSalt _salt MultiMeasureAttributeMapping' {..} =
    _salt
      `Prelude.hashWithSalt` measureValueType
      `Prelude.hashWithSalt` targetMultiMeasureAttributeName
      `Prelude.hashWithSalt` sourceColumn

instance Prelude.NFData MultiMeasureAttributeMapping where
  rnf MultiMeasureAttributeMapping' {..} =
    Prelude.rnf measureValueType
      `Prelude.seq` Prelude.rnf targetMultiMeasureAttributeName
      `Prelude.seq` Prelude.rnf sourceColumn

instance Data.ToJSON MultiMeasureAttributeMapping where
  toJSON MultiMeasureAttributeMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MeasureValueType" Data..=)
              Prelude.<$> measureValueType,
            ("TargetMultiMeasureAttributeName" Data..=)
              Prelude.<$> targetMultiMeasureAttributeName,
            Prelude.Just ("SourceColumn" Data..= sourceColumn)
          ]
      )
