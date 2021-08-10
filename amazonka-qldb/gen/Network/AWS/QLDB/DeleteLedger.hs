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
-- Module      : Network.AWS.QLDB.DeleteLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a ledger and all of its contents. This action is irreversible.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
module Network.AWS.QLDB.DeleteLedger
  ( -- * Creating a Request
    DeleteLedger (..),
    newDeleteLedger,

    -- * Request Lenses
    deleteLedger_name,

    -- * Destructuring the Response
    DeleteLedgerResponse (..),
    newDeleteLedgerResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLedger' smart constructor.
data DeleteLedger = DeleteLedger'
  { -- | The name of the ledger that you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteLedger_name' - The name of the ledger that you want to delete.
newDeleteLedger ::
  -- | 'name'
  Prelude.Text ->
  DeleteLedger
newDeleteLedger pName_ = DeleteLedger' {name = pName_}

-- | The name of the ledger that you want to delete.
deleteLedger_name :: Lens.Lens' DeleteLedger Prelude.Text
deleteLedger_name = Lens.lens (\DeleteLedger' {name} -> name) (\s@DeleteLedger' {} a -> s {name = a} :: DeleteLedger)

instance Core.AWSRequest DeleteLedger where
  type AWSResponse DeleteLedger = DeleteLedgerResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteLedgerResponse'

instance Prelude.Hashable DeleteLedger

instance Prelude.NFData DeleteLedger

instance Core.ToHeaders DeleteLedger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteLedger where
  toPath DeleteLedger' {..} =
    Prelude.mconcat ["/ledgers/", Core.toBS name]

instance Core.ToQuery DeleteLedger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLedgerResponse' smart constructor.
data DeleteLedgerResponse = DeleteLedgerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLedgerResponse ::
  DeleteLedgerResponse
newDeleteLedgerResponse = DeleteLedgerResponse'

instance Prelude.NFData DeleteLedgerResponse
