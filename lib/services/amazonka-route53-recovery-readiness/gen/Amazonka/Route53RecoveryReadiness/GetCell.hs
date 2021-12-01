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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a Cell.
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
    getCellResponse_cells,
    getCellResponse_parentReadinessScopes,
    getCellResponse_cellName,
    getCellResponse_cellArn,
    getCellResponse_tags,
    getCellResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetCell' smart constructor.
data GetCell = GetCell'
  { -- | The Cell to get
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
-- 'cellName', 'getCell_cellName' - The Cell to get
newGetCell ::
  -- | 'cellName'
  Prelude.Text ->
  GetCell
newGetCell pCellName_ =
  GetCell' {cellName = pCellName_}

-- | The Cell to get
getCell_cellName :: Lens.Lens' GetCell Prelude.Text
getCell_cellName = Lens.lens (\GetCell' {cellName} -> cellName) (\s@GetCell' {} a -> s {cellName = a} :: GetCell)

instance Core.AWSRequest GetCell where
  type AWSResponse GetCell = GetCellResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCellResponse'
            Prelude.<$> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "parentReadinessScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "cellName")
            Prelude.<*> (x Core..?> "cellArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCell where
  hashWithSalt salt' GetCell' {..} =
    salt' `Prelude.hashWithSalt` cellName

instance Prelude.NFData GetCell where
  rnf GetCell' {..} = Prelude.rnf cellName

instance Core.ToHeaders GetCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCell where
  toPath GetCell' {..} =
    Prelude.mconcat ["/cells/", Core.toBS cellName]

instance Core.ToQuery GetCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCellResponse' smart constructor.
data GetCellResponse = GetCellResponse'
  { -- | A list of Cell arns
    cells :: Prelude.Maybe [Prelude.Text],
    -- | A list of Cell ARNs and\/or RecoveryGroup ARNs
    parentReadinessScopes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Cell
    cellName :: Prelude.Maybe Prelude.Text,
    -- | The arn for the Cell
    cellArn :: Prelude.Maybe Prelude.Text,
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
-- 'cells', 'getCellResponse_cells' - A list of Cell arns
--
-- 'parentReadinessScopes', 'getCellResponse_parentReadinessScopes' - A list of Cell ARNs and\/or RecoveryGroup ARNs
--
-- 'cellName', 'getCellResponse_cellName' - The name of the Cell
--
-- 'cellArn', 'getCellResponse_cellArn' - The arn for the Cell
--
-- 'tags', 'getCellResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'getCellResponse_httpStatus' - The response's http status code.
newGetCellResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCellResponse
newGetCellResponse pHttpStatus_ =
  GetCellResponse'
    { cells = Prelude.Nothing,
      parentReadinessScopes = Prelude.Nothing,
      cellName = Prelude.Nothing,
      cellArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Cell arns
getCellResponse_cells :: Lens.Lens' GetCellResponse (Prelude.Maybe [Prelude.Text])
getCellResponse_cells = Lens.lens (\GetCellResponse' {cells} -> cells) (\s@GetCellResponse' {} a -> s {cells = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of Cell ARNs and\/or RecoveryGroup ARNs
getCellResponse_parentReadinessScopes :: Lens.Lens' GetCellResponse (Prelude.Maybe [Prelude.Text])
getCellResponse_parentReadinessScopes = Lens.lens (\GetCellResponse' {parentReadinessScopes} -> parentReadinessScopes) (\s@GetCellResponse' {} a -> s {parentReadinessScopes = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Cell
getCellResponse_cellName :: Lens.Lens' GetCellResponse (Prelude.Maybe Prelude.Text)
getCellResponse_cellName = Lens.lens (\GetCellResponse' {cellName} -> cellName) (\s@GetCellResponse' {} a -> s {cellName = a} :: GetCellResponse)

-- | The arn for the Cell
getCellResponse_cellArn :: Lens.Lens' GetCellResponse (Prelude.Maybe Prelude.Text)
getCellResponse_cellArn = Lens.lens (\GetCellResponse' {cellArn} -> cellArn) (\s@GetCellResponse' {} a -> s {cellArn = a} :: GetCellResponse)

-- | Undocumented member.
getCellResponse_tags :: Lens.Lens' GetCellResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCellResponse_tags = Lens.lens (\GetCellResponse' {tags} -> tags) (\s@GetCellResponse' {} a -> s {tags = a} :: GetCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCellResponse_httpStatus :: Lens.Lens' GetCellResponse Prelude.Int
getCellResponse_httpStatus = Lens.lens (\GetCellResponse' {httpStatus} -> httpStatus) (\s@GetCellResponse' {} a -> s {httpStatus = a} :: GetCellResponse)

instance Prelude.NFData GetCellResponse where
  rnf GetCellResponse' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf parentReadinessScopes
