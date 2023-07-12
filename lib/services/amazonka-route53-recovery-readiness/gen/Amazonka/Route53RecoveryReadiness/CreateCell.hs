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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cell in an account.
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
    createCellResponse_cellArn,
    createCellResponse_cellName,
    createCellResponse_cells,
    createCellResponse_parentReadinessScopes,
    createCellResponse_tags,
    createCellResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newCreateCell' smart constructor.
data CreateCell = CreateCell'
  { -- | A list of cell Amazon Resource Names (ARNs) contained within this cell,
    -- for use in nested cells. For example, Availability Zones within specific
    -- Amazon Web Services Regions.
    cells :: Prelude.Maybe [Prelude.Text],
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cell to create.
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
-- 'cells', 'createCell_cells' - A list of cell Amazon Resource Names (ARNs) contained within this cell,
-- for use in nested cells. For example, Availability Zones within specific
-- Amazon Web Services Regions.
--
-- 'tags', 'createCell_tags' - Undocumented member.
--
-- 'cellName', 'createCell_cellName' - The name of the cell to create.
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

-- | A list of cell Amazon Resource Names (ARNs) contained within this cell,
-- for use in nested cells. For example, Availability Zones within specific
-- Amazon Web Services Regions.
createCell_cells :: Lens.Lens' CreateCell (Prelude.Maybe [Prelude.Text])
createCell_cells = Lens.lens (\CreateCell' {cells} -> cells) (\s@CreateCell' {} a -> s {cells = a} :: CreateCell) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createCell_tags :: Lens.Lens' CreateCell (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCell_tags = Lens.lens (\CreateCell' {tags} -> tags) (\s@CreateCell' {} a -> s {tags = a} :: CreateCell) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cell to create.
createCell_cellName :: Lens.Lens' CreateCell Prelude.Text
createCell_cellName = Lens.lens (\CreateCell' {cellName} -> cellName) (\s@CreateCell' {} a -> s {cellName = a} :: CreateCell)

instance Core.AWSRequest CreateCell where
  type AWSResponse CreateCell = CreateCellResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCellResponse'
            Prelude.<$> (x Data..?> "cellArn")
            Prelude.<*> (x Data..?> "cellName")
            Prelude.<*> (x Data..?> "cells" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "parentReadinessScopes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCell where
  hashWithSalt _salt CreateCell' {..} =
    _salt
      `Prelude.hashWithSalt` cells
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cellName

instance Prelude.NFData CreateCell where
  rnf CreateCell' {..} =
    Prelude.rnf cells
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cellName

instance Data.ToHeaders CreateCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCell where
  toJSON CreateCell' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cells" Data..=) Prelude.<$> cells,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("cellName" Data..= cellName)
          ]
      )

instance Data.ToPath CreateCell where
  toPath = Prelude.const "/cells"

instance Data.ToQuery CreateCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCellResponse' smart constructor.
data CreateCellResponse = CreateCellResponse'
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
-- Create a value of 'CreateCellResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellArn', 'createCellResponse_cellArn' - The Amazon Resource Name (ARN) for the cell.
--
-- 'cellName', 'createCellResponse_cellName' - The name of the cell.
--
-- 'cells', 'createCellResponse_cells' - A list of cell ARNs.
--
-- 'parentReadinessScopes', 'createCellResponse_parentReadinessScopes' - The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
--
-- 'tags', 'createCellResponse_tags' - Tags on the resources.
--
-- 'httpStatus', 'createCellResponse_httpStatus' - The response's http status code.
newCreateCellResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCellResponse
newCreateCellResponse pHttpStatus_ =
  CreateCellResponse'
    { cellArn = Prelude.Nothing,
      cellName = Prelude.Nothing,
      cells = Prelude.Nothing,
      parentReadinessScopes = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the cell.
createCellResponse_cellArn :: Lens.Lens' CreateCellResponse (Prelude.Maybe Prelude.Text)
createCellResponse_cellArn = Lens.lens (\CreateCellResponse' {cellArn} -> cellArn) (\s@CreateCellResponse' {} a -> s {cellArn = a} :: CreateCellResponse)

-- | The name of the cell.
createCellResponse_cellName :: Lens.Lens' CreateCellResponse (Prelude.Maybe Prelude.Text)
createCellResponse_cellName = Lens.lens (\CreateCellResponse' {cellName} -> cellName) (\s@CreateCellResponse' {} a -> s {cellName = a} :: CreateCellResponse)

-- | A list of cell ARNs.
createCellResponse_cells :: Lens.Lens' CreateCellResponse (Prelude.Maybe [Prelude.Text])
createCellResponse_cells = Lens.lens (\CreateCellResponse' {cells} -> cells) (\s@CreateCellResponse' {} a -> s {cells = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The readiness scope for the cell, which can be a cell Amazon Resource
-- Name (ARN) or a recovery group ARN. This is a list but currently can
-- have only one element.
createCellResponse_parentReadinessScopes :: Lens.Lens' CreateCellResponse (Prelude.Maybe [Prelude.Text])
createCellResponse_parentReadinessScopes = Lens.lens (\CreateCellResponse' {parentReadinessScopes} -> parentReadinessScopes) (\s@CreateCellResponse' {} a -> s {parentReadinessScopes = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | Tags on the resources.
createCellResponse_tags :: Lens.Lens' CreateCellResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCellResponse_tags = Lens.lens (\CreateCellResponse' {tags} -> tags) (\s@CreateCellResponse' {} a -> s {tags = a} :: CreateCellResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCellResponse_httpStatus :: Lens.Lens' CreateCellResponse Prelude.Int
createCellResponse_httpStatus = Lens.lens (\CreateCellResponse' {httpStatus} -> httpStatus) (\s@CreateCellResponse' {} a -> s {httpStatus = a} :: CreateCellResponse)

instance Prelude.NFData CreateCellResponse where
  rnf CreateCellResponse' {..} =
    Prelude.rnf cellArn
      `Prelude.seq` Prelude.rnf cellName
      `Prelude.seq` Prelude.rnf cells
      `Prelude.seq` Prelude.rnf parentReadinessScopes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
