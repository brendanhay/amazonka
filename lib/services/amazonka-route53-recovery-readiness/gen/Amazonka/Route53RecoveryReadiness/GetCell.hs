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
-- Module      : Amazonka.Route53RecoveryReadiness.GetCell
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a cell including cell name, cell Amazon Resource
-- Name (ARN), ARNs of nested cells for this cell, and a list of those cell
-- ARNs with their associated recovery group ARNs.
module Amazonka.Route53RecoveryReadiness.GetCell
  ( -- * Creating a Request
    GetCell (..),
    newGetCell,

    -- * Request Lenses
    getCell_cellName,

    -- * Destructuring the Response
    GetCellResponse (..),
    newGetCellResponse,

    -- * Response Lenses
    getCellResponse_cellArn,
    getCellResponse_cellName,
    getCellResponse_cells,
    getCellResponse_parentReadinessScopes,
    getCellResponse_tags,
    getCellResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetCell' smart constructor.
data GetCell = GetCell'
  { -- | The name of the cell.
    cellName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellName', 'getCell_cellName' - The name of the cell.
newGetCell ::
  -- | 'cellName'
  Prelude.Text ->
  GetCell
newGetCell pCellName_ =
  GetCell' {cellName = pCellName_}

-- | The name of the cell.
getCell_cellName :: Lens.Lens' GetCell Prelude.Text
getCell_cellName = Lens.lens (\GetCell' {cellName} -> cellName) (\s@GetCell' {} a -> s {cellName = a} :: GetCell)

instance Core.AWSRequest GetCell where
  type AWSResponse GetCell = GetCellResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCellResponse'
            Prelude.<$> (x Data..?> "cellArn")
            Prelude.<*> (x Data..?> "cellName")
            Prelude.<*> (x Data..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "parentReadinessScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCell where
  hashWithSalt _salt GetCell' {..} =
    _salt `Prelude.hashWithSalt` cellName

instance Prelude.NFData GetCell where
  rnf GetCell' {..} = Prelude.rnf cellName

instance Data.ToHeaders GetCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCell where
  toPath GetCell' {..} =
    Prelude.mconcat ["/cells/", Data.toBS cellName]

instance Data.ToQuery GetCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCellResponse' smart constructor.
data GetCellResponse = GetCellResponse'
  { -- | The Amazon Resource Name (ARN) for the cell.
    cellArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cell.
    cellName :: Prelude.Maybe Prelude.Text,
    -- | A list of cell ARNs.
    cells :: Prelude.Maybe [Prelude.Text],
    -- | The readiness scope for the cell, which can be a cell Amazon Resource
    -- Name (ARN) or a recovery group ARN. This is a list but currently can
    -- have only one element.
    parentReadinessScopes :: Prelude.Maybe [Prelude.Text],
    -- | Tags on the resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCellResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellArn', 'getCellResponse_cellArn' - The Amazon Resource Name (ARN) for the cell.
--
-- 'cellName', 'getCellResponse_cellName' - The name of the cell.
--
-- 'cells', 'getCellResponse_cells' - A list of cell ARNs.
--
-- 'parentReadinessScopes', 'getCellResponse_parentReadinessScopes' - The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
--
-- 'tags', 'getCellResponse_tags' - Tags on the resources.
--
-- 'httpStatus', 'getCellResponse_httpStatus' - The response's http status code.
newGetCellResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCellResponse
newGetCellResponse pHttpStatus_ =
  GetCellResponse'
    { cellArn = Prelude.Nothing,
      cellName = Prelude.Nothing,
      cells = Prelude.Nothing,
      parentReadinessScopes = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the cell.
getCellResponse_cellArn :: Lens.Lens' GetCellResponse (Prelude.Maybe Prelude.Text)
getCellResponse_cellArn = Lens.lens (\GetCellResponse' {cellArn} -> cellArn) (\s@GetCellResponse' {} a -> s {cellArn = a} :: GetCellResponse)

-- | The name of the cell.
getCellResponse_cellName :: Lens.Lens' GetCellResponse (Prelude.Maybe Prelude.Text)
getCellResponse_cellName = Lens.lens (\GetCellResponse' {cellName} -> cellName) (\s@GetCellResponse' {} a -> s {cellName = a} :: GetCellResponse)

-- | A list of cell ARNs.
getCellResponse_cells :: Lens.Lens' GetCellResponse (Prelude.Maybe [Prelude.Text])
getCellResponse_cells = Lens.lens (\GetCellResponse' {cells} -> cells) (\s@GetCellResponse' {} a -> s {cells = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
getCellResponse_parentReadinessScopes :: Lens.Lens' GetCellResponse (Prelude.Maybe [Prelude.Text])
getCellResponse_parentReadinessScopes = Lens.lens (\GetCellResponse' {parentReadinessScopes} -> parentReadinessScopes) (\s@GetCellResponse' {} a -> s {parentReadinessScopes = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | Tags on the resources.
getCellResponse_tags :: Lens.Lens' GetCellResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCellResponse_tags = Lens.lens (\GetCellResponse' {tags} -> tags) (\s@GetCellResponse' {} a -> s {tags = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCellResponse_httpStatus :: Lens.Lens' GetCellResponse Prelude.Int
getCellResponse_httpStatus = Lens.lens (\GetCellResponse' {httpStatus} -> httpStatus) (\s@GetCellResponse' {} a -> s {httpStatus = a} :: GetCellResponse)

instance Prelude.NFData GetCellResponse where
  rnf GetCellResponse' {..} =
    Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf parentReadinessScopes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
