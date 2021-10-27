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
-- Module      : Network.AWS.Route53RecoveryReadiness.DeleteCell
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing Cell.
module Network.AWS.Route53RecoveryReadiness.DeleteCell
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteCell' smart constructor.
data DeleteCell = DeleteCell'
  { -- | The Cell to delete
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
-- 'cellName', 'deleteCell_cellName' - The Cell to delete
newDeleteCell ::
  -- | 'cellName'
  Prelude.Text ->
  DeleteCell
newDeleteCell pCellName_ =
  DeleteCell' {cellName = pCellName_}

-- | The Cell to delete
deleteCell_cellName :: Lens.Lens' DeleteCell Prelude.Text
deleteCell_cellName = Lens.lens (\DeleteCell' {cellName} -> cellName) (\s@DeleteCell' {} a -> s {cellName = a} :: DeleteCell)

instance Core.AWSRequest DeleteCell where
  type AWSResponse DeleteCell = DeleteCellResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteCellResponse'

instance Prelude.Hashable DeleteCell

instance Prelude.NFData DeleteCell

instance Core.ToHeaders DeleteCell where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteCell where
  toPath DeleteCell' {..} =
    Prelude.mconcat ["/cells/", Core.toBS cellName]

instance Core.ToQuery DeleteCell where
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

instance Prelude.NFData DeleteCellResponse
