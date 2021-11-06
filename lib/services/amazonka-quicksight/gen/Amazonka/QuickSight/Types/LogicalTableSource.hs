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
-- Module      : Amazonka.QuickSight.Types.LogicalTableSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LogicalTableSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.JoinInstruction

-- | Information about the source of a logical table. This is a variant type
-- structure. For this structure to be valid, only one of the attributes
-- can be non-null.
--
-- /See:/ 'newLogicalTableSource' smart constructor.
data LogicalTableSource = LogicalTableSource'
  { -- | The Amazon Resource Number (ARN) of the parent dataset.
    dataSetArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the result of a join of two logical tables.
    joinInstruction :: Prelude.Maybe JoinInstruction,
    -- | Physical table ID.
    physicalTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogicalTableSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetArn', 'logicalTableSource_dataSetArn' - The Amazon Resource Number (ARN) of the parent dataset.
--
-- 'joinInstruction', 'logicalTableSource_joinInstruction' - Specifies the result of a join of two logical tables.
--
-- 'physicalTableId', 'logicalTableSource_physicalTableId' - Physical table ID.
newLogicalTableSource ::
  LogicalTableSource
newLogicalTableSource =
  LogicalTableSource'
    { dataSetArn = Prelude.Nothing,
      joinInstruction = Prelude.Nothing,
      physicalTableId = Prelude.Nothing
    }

-- | The Amazon Resource Number (ARN) of the parent dataset.
logicalTableSource_dataSetArn :: Lens.Lens' LogicalTableSource (Prelude.Maybe Prelude.Text)
logicalTableSource_dataSetArn = Lens.lens (\LogicalTableSource' {dataSetArn} -> dataSetArn) (\s@LogicalTableSource' {} a -> s {dataSetArn = a} :: LogicalTableSource)

-- | Specifies the result of a join of two logical tables.
logicalTableSource_joinInstruction :: Lens.Lens' LogicalTableSource (Prelude.Maybe JoinInstruction)
logicalTableSource_joinInstruction = Lens.lens (\LogicalTableSource' {joinInstruction} -> joinInstruction) (\s@LogicalTableSource' {} a -> s {joinInstruction = a} :: LogicalTableSource)

-- | Physical table ID.
logicalTableSource_physicalTableId :: Lens.Lens' LogicalTableSource (Prelude.Maybe Prelude.Text)
logicalTableSource_physicalTableId = Lens.lens (\LogicalTableSource' {physicalTableId} -> physicalTableId) (\s@LogicalTableSource' {} a -> s {physicalTableId = a} :: LogicalTableSource)

instance Core.FromJSON LogicalTableSource where
  parseJSON =
    Core.withObject
      "LogicalTableSource"
      ( \x ->
          LogicalTableSource'
            Prelude.<$> (x Core..:? "DataSetArn")
            Prelude.<*> (x Core..:? "JoinInstruction")
            Prelude.<*> (x Core..:? "PhysicalTableId")
      )

instance Prelude.Hashable LogicalTableSource

instance Prelude.NFData LogicalTableSource

instance Core.ToJSON LogicalTableSource where
  toJSON LogicalTableSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataSetArn" Core..=) Prelude.<$> dataSetArn,
            ("JoinInstruction" Core..=)
              Prelude.<$> joinInstruction,
            ("PhysicalTableId" Core..=)
              Prelude.<$> physicalTableId
          ]
      )
