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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.CellOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A Cell and its properties
--
-- /See:/ 'newCellOutput' smart constructor.
data CellOutput = CellOutput'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of Cell ARNs and\/or RecoveryGroup ARNs
    parentReadinessScopes :: [Prelude.Text],
    -- | The arn for the Cell
    cellArn :: Prelude.Text,
    -- | The name of the Cell
    cellName :: Prelude.Text,
    -- | A list of Cell arns
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
-- 'tags', 'cellOutput_tags' - Undocumented member.
--
-- 'parentReadinessScopes', 'cellOutput_parentReadinessScopes' - A list of Cell ARNs and\/or RecoveryGroup ARNs
--
-- 'cellArn', 'cellOutput_cellArn' - The arn for the Cell
--
-- 'cellName', 'cellOutput_cellName' - The name of the Cell
--
-- 'cells', 'cellOutput_cells' - A list of Cell arns
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

-- | Undocumented member.
cellOutput_tags :: Lens.Lens' CellOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cellOutput_tags = Lens.lens (\CellOutput' {tags} -> tags) (\s@CellOutput' {} a -> s {tags = a} :: CellOutput) Prelude.. Lens.mapping Lens.coerced

-- | A list of Cell ARNs and\/or RecoveryGroup ARNs
cellOutput_parentReadinessScopes :: Lens.Lens' CellOutput [Prelude.Text]
cellOutput_parentReadinessScopes = Lens.lens (\CellOutput' {parentReadinessScopes} -> parentReadinessScopes) (\s@CellOutput' {} a -> s {parentReadinessScopes = a} :: CellOutput) Prelude.. Lens.coerced

-- | The arn for the Cell
cellOutput_cellArn :: Lens.Lens' CellOutput Prelude.Text
cellOutput_cellArn = Lens.lens (\CellOutput' {cellArn} -> cellArn) (\s@CellOutput' {} a -> s {cellArn = a} :: CellOutput)

-- | The name of the Cell
cellOutput_cellName :: Lens.Lens' CellOutput Prelude.Text
cellOutput_cellName = Lens.lens (\CellOutput' {cellName} -> cellName) (\s@CellOutput' {} a -> s {cellName = a} :: CellOutput)

-- | A list of Cell arns
cellOutput_cells :: Lens.Lens' CellOutput [Prelude.Text]
cellOutput_cells = Lens.lens (\CellOutput' {cells} -> cells) (\s@CellOutput' {} a -> s {cells = a} :: CellOutput) Prelude.. Lens.coerced

instance Core.FromJSON CellOutput where
  parseJSON =
    Core.withObject
      "CellOutput"
      ( \x ->
          CellOutput'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "parentReadinessScopes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "cellArn")
            Prelude.<*> (x Core..: "cellName")
            Prelude.<*> (x Core..:? "cells" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CellOutput where
  hashWithSalt salt' CellOutput' {..} =
    salt' `Prelude.hashWithSalt` cells
      `Prelude.hashWithSalt` cellName
      `Prelude.hashWithSalt` cellArn
      `Prelude.hashWithSalt` parentReadinessScopes
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CellOutput where
  rnf CellOutput' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf parentReadinessScopes
