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
-- Module      : Amazonka.Route53RecoveryReadiness.DeleteCell
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a cell. When successful, the response code is 204, with no
-- response body.
module Amazonka.Route53RecoveryReadiness.DeleteCell
  ( -- * Creating a Request
    DeleteCell (..),
    newDeleteCell,

    -- * Request Lenses
    deleteCell_cellName,

    -- * Destructuring the Response
    DeleteCellResponse (..),
    newDeleteCellResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteCell' smart constructor.
data DeleteCell = DeleteCell'
  { -- | The name of the cell.
    cellName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellName', 'deleteCell_cellName' - The name of the cell.
newDeleteCell ::
  -- | 'cellName'
  Prelude.Text ->
  DeleteCell
newDeleteCell pCellName_ =
  DeleteCell' {cellName = pCellName_}

-- | The name of the cell.
deleteCell_cellName :: Lens.Lens' DeleteCell Prelude.Text
deleteCell_cellName = Lens.lens (\DeleteCell' {cellName} -> cellName) (\s@DeleteCell' {} a -> s {cellName = a} :: DeleteCell)

instance Core.AWSRequest DeleteCell where
  type AWSResponse DeleteCell = DeleteCellResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteCellResponse'

instance Prelude.Hashable DeleteCell where
  hashWithSalt _salt DeleteCell' {..} =
    _salt `Prelude.hashWithSalt` cellName

instance Prelude.NFData DeleteCell where
  rnf DeleteCell' {..} = Prelude.rnf cellName

instance Data.ToHeaders DeleteCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCell where
  toPath DeleteCell' {..} =
    Prelude.mconcat ["/cells/", Data.toBS cellName]

instance Data.ToQuery DeleteCell where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCellResponse' smart constructor.
data DeleteCellResponse = DeleteCellResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCellResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCellResponse ::
  DeleteCellResponse
newDeleteCellResponse = DeleteCellResponse'

instance Prelude.NFData DeleteCellResponse where
  rnf _ = ()
