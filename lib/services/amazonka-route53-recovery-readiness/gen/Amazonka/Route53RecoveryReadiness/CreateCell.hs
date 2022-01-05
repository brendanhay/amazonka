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
-- Module      : Amazonka.Route53RecoveryReadiness.CreateCell
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Cell.
module Amazonka.Route53RecoveryReadiness.CreateCell
  ( -- * Creating a Request
    CreateCell (..),
    newCreateCell,

    -- * Request Lenses
    createCell_cells,
    createCell_tags,
    createCell_cellName,

    -- * Destructuring the Response
    CreateCellResponse (..),
    newCreateCellResponse,

    -- * Response Lenses
    createCellResponse_cells,
    createCellResponse_parentReadinessScopes,
    createCellResponse_cellName,
    createCellResponse_cellArn,
    createCellResponse_tags,
    createCellResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | The Cell to create
--
-- /See:/ 'newCreateCell' smart constructor.
data CreateCell = CreateCell'
  { -- | A list of Cell arns contained within this Cell (for use in nested Cells,
    -- e.g. regions within which AZs)
    cells :: Prelude.Maybe [Prelude.Text],
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the Cell to create
    cellName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'createCell_cells' - A list of Cell arns contained within this Cell (for use in nested Cells,
-- e.g. regions within which AZs)
--
-- 'tags', 'createCell_tags' - Undocumented member.
--
-- 'cellName', 'createCell_cellName' - The name of the Cell to create
newCreateCell ::
  -- | 'cellName'
  Prelude.Text ->
  CreateCell
newCreateCell pCellName_ =
  CreateCell'
    { cells = Prelude.Nothing,
      tags = Prelude.Nothing,
      cellName = pCellName_
    }

-- | A list of Cell arns contained within this Cell (for use in nested Cells,
-- e.g. regions within which AZs)
createCell_cells :: Lens.Lens' CreateCell (Prelude.Maybe [Prelude.Text])
createCell_cells = Lens.lens (\CreateCell' {cells} -> cells) (\s@CreateCell' {} a -> s {cells = a} :: CreateCell) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createCell_tags :: Lens.Lens' CreateCell (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCell_tags = Lens.lens (\CreateCell' {tags} -> tags) (\s@CreateCell' {} a -> s {tags = a} :: CreateCell) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Cell to create
createCell_cellName :: Lens.Lens' CreateCell Prelude.Text
createCell_cellName = Lens.lens (\CreateCell' {cellName} -> cellName) (\s@CreateCell' {} a -> s {cellName = a} :: CreateCell)

instance Core.AWSRequest CreateCell where
  type AWSResponse CreateCell = CreateCellResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCellResponse'
            Prelude.<$> (x Core..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "parentReadinessScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "cellName")
            Prelude.<*> (x Core..?> "cellArn")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCell where
  hashWithSalt _salt CreateCell' {..} =
    _salt `Prelude.hashWithSalt` cells
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cellName

instance Prelude.NFData CreateCell where
  rnf CreateCell' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cellName

instance Core.ToHeaders CreateCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCell where
  toJSON CreateCell' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cells" Core..=) Prelude.<$> cells,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("cellName" Core..= cellName)
          ]
      )

instance Core.ToPath CreateCell where
  toPath = Prelude.const "/cells"

instance Core.ToQuery CreateCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCellResponse' smart constructor.
data CreateCellResponse = CreateCellResponse'
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
-- Create a value of 'CreateCellResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'createCellResponse_cells' - A list of Cell arns
--
-- 'parentReadinessScopes', 'createCellResponse_parentReadinessScopes' - A list of Cell ARNs and\/or RecoveryGroup ARNs
--
-- 'cellName', 'createCellResponse_cellName' - The name of the Cell
--
-- 'cellArn', 'createCellResponse_cellArn' - The arn for the Cell
--
-- 'tags', 'createCellResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createCellResponse_httpStatus' - The response's http status code.
newCreateCellResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCellResponse
newCreateCellResponse pHttpStatus_ =
  CreateCellResponse'
    { cells = Prelude.Nothing,
      parentReadinessScopes = Prelude.Nothing,
      cellName = Prelude.Nothing,
      cellArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of Cell arns
createCellResponse_cells :: Lens.Lens' CreateCellResponse (Prelude.Maybe [Prelude.Text])
createCellResponse_cells = Lens.lens (\CreateCellResponse' {cells} -> cells) (\s@CreateCellResponse' {} a -> s {cells = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of Cell ARNs and\/or RecoveryGroup ARNs
createCellResponse_parentReadinessScopes :: Lens.Lens' CreateCellResponse (Prelude.Maybe [Prelude.Text])
createCellResponse_parentReadinessScopes = Lens.lens (\CreateCellResponse' {parentReadinessScopes} -> parentReadinessScopes) (\s@CreateCellResponse' {} a -> s {parentReadinessScopes = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Cell
createCellResponse_cellName :: Lens.Lens' CreateCellResponse (Prelude.Maybe Prelude.Text)
createCellResponse_cellName = Lens.lens (\CreateCellResponse' {cellName} -> cellName) (\s@CreateCellResponse' {} a -> s {cellName = a} :: CreateCellResponse)

-- | The arn for the Cell
createCellResponse_cellArn :: Lens.Lens' CreateCellResponse (Prelude.Maybe Prelude.Text)
createCellResponse_cellArn = Lens.lens (\CreateCellResponse' {cellArn} -> cellArn) (\s@CreateCellResponse' {} a -> s {cellArn = a} :: CreateCellResponse)

-- | Undocumented member.
createCellResponse_tags :: Lens.Lens' CreateCellResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCellResponse_tags = Lens.lens (\CreateCellResponse' {tags} -> tags) (\s@CreateCellResponse' {} a -> s {tags = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCellResponse_httpStatus :: Lens.Lens' CreateCellResponse Prelude.Int
createCellResponse_httpStatus = Lens.lens (\CreateCellResponse' {httpStatus} -> httpStatus) (\s@CreateCellResponse' {} a -> s {httpStatus = a} :: CreateCellResponse)

instance Prelude.NFData CreateCellResponse where
  rnf CreateCellResponse' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf parentReadinessScopes
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
