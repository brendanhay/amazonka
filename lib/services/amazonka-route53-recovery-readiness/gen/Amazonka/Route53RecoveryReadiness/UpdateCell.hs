{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryReadiness.UpdateCell
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Cell.
module Amazonka.Route53RecoveryReadiness.UpdateCell
  ( -- * Creating a Request
    UpdateCell (..),
    newUpdateCell,

    -- * Request Lenses
    updateCell_cellName,
    updateCell_cells,

    -- * Destructuring the Response
    UpdateCellResponse (..),
    newUpdateCellResponse,

    -- * Response Lenses
    updateCellResponse_tags,
    updateCellResponse_parentReadinessScopes,
    updateCellResponse_cellArn,
    updateCellResponse_cellName,
    updateCellResponse_cells,
    updateCellResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | Parameters to update for the Cell
--
-- /See:/ 'newUpdateCell' smart constructor.
data UpdateCell = UpdateCell'
  { -- | The Cell to update
    cellName :: Prelude.Text,
    -- | A list of Cell arns, completely replaces previous list
    cells :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellName', 'updateCell_cellName' - The Cell to update
--
-- 'cells', 'updateCell_cells' - A list of Cell arns, completely replaces previous list
newUpdateCell ::
  -- | 'cellName'
  Prelude.Text ->
  UpdateCell
newUpdateCell pCellName_ =
  UpdateCell'
    { cellName = pCellName_,
      cells = Prelude.mempty
    }

-- | The Cell to update
updateCell_cellName :: Lens.Lens' UpdateCell Prelude.Text
updateCell_cellName = Lens.lens (\UpdateCell' {cellName} -> cellName) (\s@UpdateCell' {} a -> s {cellName = a} :: UpdateCell)

-- | A list of Cell arns, completely replaces previous list
updateCell_cells :: Lens.Lens' UpdateCell [Prelude.Text]
updateCell_cells = Lens.lens (\UpdateCell' {cells} -> cells) (\s@UpdateCell' {} a -> s {cells = a} :: UpdateCell) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateCell where
  type AWSResponse UpdateCell = UpdateCellResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCellResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "parentReadinessScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "cellArn")
            Prelude.<*> (x Core..?> "cellName")
            Prelude.<*> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCell where
  hashWithSalt _salt UpdateCell' {..} =
    _salt `Prelude.hashWithSalt` cellName
      `Prelude.hashWithSalt` cells

instance Prelude.NFData UpdateCell where
  rnf UpdateCell' {..} =
    Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cells

instance Core.ToHeaders UpdateCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCell where
  toJSON UpdateCell' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("cells" Core..= cells)]
      )

instance Core.ToPath UpdateCell where
  toPath UpdateCell' {..} =
    Prelude.mconcat ["/cells/", Core.toBS cellName]

instance Core.ToQuery UpdateCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCellResponse' smart constructor.
data UpdateCellResponse = UpdateCellResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of Cell ARNs and\/or RecoveryGroup ARNs
    parentReadinessScopes :: Prelude.Maybe [Prelude.Text],
    -- | The arn for the Cell
    cellArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Cell
    cellName :: Prelude.Maybe Prelude.Text,
    -- | A list of Cell arns
    cells :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCellResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateCellResponse_tags' - Undocumented member.
--
-- 'parentReadinessScopes', 'updateCellResponse_parentReadinessScopes' - A list of Cell ARNs and\/or RecoveryGroup ARNs
--
-- 'cellArn', 'updateCellResponse_cellArn' - The arn for the Cell
--
-- 'cellName', 'updateCellResponse_cellName' - The name of the Cell
--
-- 'cells', 'updateCellResponse_cells' - A list of Cell arns
--
-- 'httpStatus', 'updateCellResponse_httpStatus' - The response's http status code.
newUpdateCellResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCellResponse
newUpdateCellResponse pHttpStatus_ =
  UpdateCellResponse'
    { tags = Prelude.Nothing,
      parentReadinessScopes = Prelude.Nothing,
      cellArn = Prelude.Nothing,
      cellName = Prelude.Nothing,
      cells = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateCellResponse_tags :: Lens.Lens' UpdateCellResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateCellResponse_tags = Lens.lens (\UpdateCellResponse' {tags} -> tags) (\s@UpdateCellResponse' {} a -> s {tags = a} :: UpdateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of Cell ARNs and\/or RecoveryGroup ARNs
updateCellResponse_parentReadinessScopes :: Lens.Lens' UpdateCellResponse (Prelude.Maybe [Prelude.Text])
updateCellResponse_parentReadinessScopes = Lens.lens (\UpdateCellResponse' {parentReadinessScopes} -> parentReadinessScopes) (\s@UpdateCellResponse' {} a -> s {parentReadinessScopes = a} :: UpdateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The arn for the Cell
updateCellResponse_cellArn :: Lens.Lens' UpdateCellResponse (Prelude.Maybe Prelude.Text)
updateCellResponse_cellArn = Lens.lens (\UpdateCellResponse' {cellArn} -> cellArn) (\s@UpdateCellResponse' {} a -> s {cellArn = a} :: UpdateCellResponse)

-- | The name of the Cell
updateCellResponse_cellName :: Lens.Lens' UpdateCellResponse (Prelude.Maybe Prelude.Text)
updateCellResponse_cellName = Lens.lens (\UpdateCellResponse' {cellName} -> cellName) (\s@UpdateCellResponse' {} a -> s {cellName = a} :: UpdateCellResponse)

-- | A list of Cell arns
updateCellResponse_cells :: Lens.Lens' UpdateCellResponse (Prelude.Maybe [Prelude.Text])
updateCellResponse_cells = Lens.lens (\UpdateCellResponse' {cells} -> cells) (\s@UpdateCellResponse' {} a -> s {cells = a} :: UpdateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateCellResponse_httpStatus :: Lens.Lens' UpdateCellResponse Prelude.Int
updateCellResponse_httpStatus = Lens.lens (\UpdateCellResponse' {httpStatus} -> httpStatus) (\s@UpdateCellResponse' {} a -> s {httpStatus = a} :: UpdateCellResponse)

instance Prelude.NFData UpdateCellResponse where
  rnf UpdateCellResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf parentReadinessScopes
      `Prelude.seq` Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf httpStatus
