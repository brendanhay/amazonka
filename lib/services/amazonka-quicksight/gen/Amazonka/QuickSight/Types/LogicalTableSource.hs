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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LogicalTableSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.JoinInstruction

-- | Information about the source of a logical table. This is a variant type
-- structure. For this structure to be valid, only one of the attributes
-- can be non-null.
--
-- /See:/ 'newLogicalTableSource' smart constructor.
data LogicalTableSource = LogicalTableSource'
  { -- | Physical table ID.
    physicalTableId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the result of a join of two logical tables.
    joinInstruction :: Prelude.Maybe JoinInstruction,
    -- | The Amazon Resource Number (ARN) of the parent dataset.
    dataSetArn :: Prelude.Maybe Prelude.Text
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
-- 'physicalTableId', 'logicalTableSource_physicalTableId' - Physical table ID.
--
-- 'joinInstruction', 'logicalTableSource_joinInstruction' - Specifies the result of a join of two logical tables.
--
-- 'dataSetArn', 'logicalTableSource_dataSetArn' - The Amazon Resource Number (ARN) of the parent dataset.
newLogicalTableSource ::
  LogicalTableSource
newLogicalTableSource =
  LogicalTableSource'
    { physicalTableId =
        Prelude.Nothing,
      joinInstruction = Prelude.Nothing,
      dataSetArn = Prelude.Nothing
    }

-- | Physical table ID.
logicalTableSource_physicalTableId :: Lens.Lens' LogicalTableSource (Prelude.Maybe Prelude.Text)
logicalTableSource_physicalTableId = Lens.lens (\LogicalTableSource' {physicalTableId} -> physicalTableId) (\s@LogicalTableSource' {} a -> s {physicalTableId = a} :: LogicalTableSource)

-- | Specifies the result of a join of two logical tables.
logicalTableSource_joinInstruction :: Lens.Lens' LogicalTableSource (Prelude.Maybe JoinInstruction)
logicalTableSource_joinInstruction = Lens.lens (\LogicalTableSource' {joinInstruction} -> joinInstruction) (\s@LogicalTableSource' {} a -> s {joinInstruction = a} :: LogicalTableSource)

-- | The Amazon Resource Number (ARN) of the parent dataset.
logicalTableSource_dataSetArn :: Lens.Lens' LogicalTableSource (Prelude.Maybe Prelude.Text)
logicalTableSource_dataSetArn = Lens.lens (\LogicalTableSource' {dataSetArn} -> dataSetArn) (\s@LogicalTableSource' {} a -> s {dataSetArn = a} :: LogicalTableSource)

instance Data.FromJSON LogicalTableSource where
  parseJSON =
    Data.withObject
      "LogicalTableSource"
      ( \x ->
          LogicalTableSource'
            Prelude.<$> (x Data..:? "PhysicalTableId")
            Prelude.<*> (x Data..:? "JoinInstruction")
            Prelude.<*> (x Data..:? "DataSetArn")
      )

instance Prelude.Hashable LogicalTableSource where
  hashWithSalt _salt LogicalTableSource' {..} =
    _salt `Prelude.hashWithSalt` physicalTableId
      `Prelude.hashWithSalt` joinInstruction
      `Prelude.hashWithSalt` dataSetArn

instance Prelude.NFData LogicalTableSource where
  rnf LogicalTableSource' {..} =
    Prelude.rnf physicalTableId
      `Prelude.seq` Prelude.rnf joinInstruction
      `Prelude.seq` Prelude.rnf dataSetArn

instance Data.ToJSON LogicalTableSource where
  toJSON LogicalTableSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PhysicalTableId" Data..=)
              Prelude.<$> physicalTableId,
            ("JoinInstruction" Data..=)
              Prelude.<$> joinInstruction,
            ("DataSetArn" Data..=) Prelude.<$> dataSetArn
          ]
      )
