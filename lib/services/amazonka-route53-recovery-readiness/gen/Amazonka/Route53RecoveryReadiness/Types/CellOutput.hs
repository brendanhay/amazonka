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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.CellOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.CellOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a cell.
--
-- /See:/ 'newCellOutput' smart constructor.
data CellOutput = CellOutput'
  { -- | Tags on the resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The readiness scope for the cell, which can be a cell Amazon Resource
    -- Name (ARN) or a recovery group ARN. This is a list but currently can
    -- have only one element.
    parentReadinessScopes :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) for the cell.
    cellArn :: Prelude.Text,
    -- | The name of the cell.
    cellName :: Prelude.Text,
    -- | A list of cell ARNs.
    cells :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CellOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'cellOutput_tags' - Tags on the resources.
--
-- 'parentReadinessScopes', 'cellOutput_parentReadinessScopes' - The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
--
-- 'cellArn', 'cellOutput_cellArn' - The Amazon Resource Name (ARN) for the cell.
--
-- 'cellName', 'cellOutput_cellName' - The name of the cell.
--
-- 'cells', 'cellOutput_cells' - A list of cell ARNs.
newCellOutput ::
  -- | 'cellArn'
  Prelude.Text ->
  -- | 'cellName'
  Prelude.Text ->
  CellOutput
newCellOutput pCellArn_ pCellName_ =
  CellOutput'
    { tags = Prelude.Nothing,
      parentReadinessScopes = Prelude.mempty,
      cellArn = pCellArn_,
      cellName = pCellName_,
      cells = Prelude.mempty
    }

-- | Tags on the resources.
cellOutput_tags :: Lens.Lens' CellOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cellOutput_tags = Lens.lens (\CellOutput' {tags} -> tags) (\s@CellOutput' {} a -> s {tags = a} :: CellOutput) Prelude.. Lens.mapping Lens.coerced

-- | The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
cellOutput_parentReadinessScopes :: Lens.Lens' CellOutput [Prelude.Text]
cellOutput_parentReadinessScopes = Lens.lens (\CellOutput' {parentReadinessScopes} -> parentReadinessScopes) (\s@CellOutput' {} a -> s {parentReadinessScopes = a} :: CellOutput) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) for the cell.
cellOutput_cellArn :: Lens.Lens' CellOutput Prelude.Text
cellOutput_cellArn = Lens.lens (\CellOutput' {cellArn} -> cellArn) (\s@CellOutput' {} a -> s {cellArn = a} :: CellOutput)

-- | The name of the cell.
cellOutput_cellName :: Lens.Lens' CellOutput Prelude.Text
cellOutput_cellName = Lens.lens (\CellOutput' {cellName} -> cellName) (\s@CellOutput' {} a -> s {cellName = a} :: CellOutput)

-- | A list of cell ARNs.
cellOutput_cells :: Lens.Lens' CellOutput [Prelude.Text]
cellOutput_cells = Lens.lens (\CellOutput' {cells} -> cells) (\s@CellOutput' {} a -> s {cells = a} :: CellOutput) Prelude.. Lens.coerced

instance Data.FromJSON CellOutput where
  parseJSON =
    Data.withObject
      "CellOutput"
      ( \x ->
          CellOutput'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "parentReadinessScopes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "cellArn")
            Prelude.<*> (x Data..: "cellName")
            Prelude.<*> (x Data..:? "cells" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CellOutput where
  hashWithSalt _salt CellOutput' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` parentReadinessScopes
      `Prelude.hashWithSalt` cellArn
      `Prelude.hashWithSalt` cellName
      `Prelude.hashWithSalt` cells

instance Prelude.NFData CellOutput where
  rnf CellOutput' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf parentReadinessScopes
      `Prelude.seq` Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cells
