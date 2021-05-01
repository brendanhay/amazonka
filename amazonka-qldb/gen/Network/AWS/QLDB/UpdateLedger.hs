{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.QLDB.UpdateLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates properties on a ledger.
module Network.AWS.QLDB.UpdateLedger
  ( -- * Creating a Request
    UpdateLedger (..),
    newUpdateLedger,

    -- * Request Lenses
    updateLedger_deletionProtection,
    updateLedger_name,

    -- * Destructuring the Response
    UpdateLedgerResponse (..),
    newUpdateLedgerResponse,

    -- * Response Lenses
    updateLedgerResponse_deletionProtection,
    updateLedgerResponse_arn,
    updateLedgerResponse_state,
    updateLedgerResponse_name,
    updateLedgerResponse_creationDateTime,
    updateLedgerResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLedger' smart constructor.
data UpdateLedger = UpdateLedger'
  { -- | The flag that prevents a ledger from being deleted by any user. If not
    -- provided on ledger creation, this feature is enabled (@true@) by
    -- default.
    --
    -- If deletion protection is enabled, you must first disable it before you
    -- can delete the ledger using the QLDB API or the AWS Command Line
    -- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
    -- operation to set the flag to @false@. The QLDB console disables deletion
    -- protection for you when you use it to delete a ledger.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ledger.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'updateLedger_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
--
-- 'name', 'updateLedger_name' - The name of the ledger.
newUpdateLedger ::
  -- | 'name'
  Prelude.Text ->
  UpdateLedger
newUpdateLedger pName_ =
  UpdateLedger'
    { deletionProtection = Prelude.Nothing,
      name = pName_
    }

-- | The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
updateLedger_deletionProtection :: Lens.Lens' UpdateLedger (Prelude.Maybe Prelude.Bool)
updateLedger_deletionProtection = Lens.lens (\UpdateLedger' {deletionProtection} -> deletionProtection) (\s@UpdateLedger' {} a -> s {deletionProtection = a} :: UpdateLedger)

-- | The name of the ledger.
updateLedger_name :: Lens.Lens' UpdateLedger Prelude.Text
updateLedger_name = Lens.lens (\UpdateLedger' {name} -> name) (\s@UpdateLedger' {} a -> s {name = a} :: UpdateLedger)

instance Prelude.AWSRequest UpdateLedger where
  type Rs UpdateLedger = UpdateLedgerResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLedgerResponse'
            Prelude.<$> (x Prelude..?> "DeletionProtection")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "State")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "CreationDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLedger

instance Prelude.NFData UpdateLedger

instance Prelude.ToHeaders UpdateLedger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateLedger where
  toJSON UpdateLedger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeletionProtection" Prelude..=)
              Prelude.<$> deletionProtection
          ]
      )

instance Prelude.ToPath UpdateLedger where
  toPath UpdateLedger' {..} =
    Prelude.mconcat ["/ledgers/", Prelude.toBS name]

instance Prelude.ToQuery UpdateLedger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLedgerResponse' smart constructor.
data UpdateLedgerResponse = UpdateLedgerResponse'
  { -- | The flag that prevents a ledger from being deleted by any user. If not
    -- provided on ledger creation, this feature is enabled (@true@) by
    -- default.
    --
    -- If deletion protection is enabled, you must first disable it before you
    -- can delete the ledger using the QLDB API or the AWS Command Line
    -- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
    -- operation to set the flag to @false@. The QLDB console disables deletion
    -- protection for you when you use it to delete a ledger.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the ledger.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the ledger.
    state :: Prelude.Maybe LedgerState,
    -- | The name of the ledger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in epoch time format, when the ledger was created.
    -- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
    -- January 1, 1970 UTC.)
    creationDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'updateLedgerResponse_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
--
-- 'arn', 'updateLedgerResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'state', 'updateLedgerResponse_state' - The current status of the ledger.
--
-- 'name', 'updateLedgerResponse_name' - The name of the ledger.
--
-- 'creationDateTime', 'updateLedgerResponse_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
--
-- 'httpStatus', 'updateLedgerResponse_httpStatus' - The response's http status code.
newUpdateLedgerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLedgerResponse
newUpdateLedgerResponse pHttpStatus_ =
  UpdateLedgerResponse'
    { deletionProtection =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
updateLedgerResponse_deletionProtection :: Lens.Lens' UpdateLedgerResponse (Prelude.Maybe Prelude.Bool)
updateLedgerResponse_deletionProtection = Lens.lens (\UpdateLedgerResponse' {deletionProtection} -> deletionProtection) (\s@UpdateLedgerResponse' {} a -> s {deletionProtection = a} :: UpdateLedgerResponse)

-- | The Amazon Resource Name (ARN) for the ledger.
updateLedgerResponse_arn :: Lens.Lens' UpdateLedgerResponse (Prelude.Maybe Prelude.Text)
updateLedgerResponse_arn = Lens.lens (\UpdateLedgerResponse' {arn} -> arn) (\s@UpdateLedgerResponse' {} a -> s {arn = a} :: UpdateLedgerResponse)

-- | The current status of the ledger.
updateLedgerResponse_state :: Lens.Lens' UpdateLedgerResponse (Prelude.Maybe LedgerState)
updateLedgerResponse_state = Lens.lens (\UpdateLedgerResponse' {state} -> state) (\s@UpdateLedgerResponse' {} a -> s {state = a} :: UpdateLedgerResponse)

-- | The name of the ledger.
updateLedgerResponse_name :: Lens.Lens' UpdateLedgerResponse (Prelude.Maybe Prelude.Text)
updateLedgerResponse_name = Lens.lens (\UpdateLedgerResponse' {name} -> name) (\s@UpdateLedgerResponse' {} a -> s {name = a} :: UpdateLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
updateLedgerResponse_creationDateTime :: Lens.Lens' UpdateLedgerResponse (Prelude.Maybe Prelude.UTCTime)
updateLedgerResponse_creationDateTime = Lens.lens (\UpdateLedgerResponse' {creationDateTime} -> creationDateTime) (\s@UpdateLedgerResponse' {} a -> s {creationDateTime = a} :: UpdateLedgerResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
updateLedgerResponse_httpStatus :: Lens.Lens' UpdateLedgerResponse Prelude.Int
updateLedgerResponse_httpStatus = Lens.lens (\UpdateLedgerResponse' {httpStatus} -> httpStatus) (\s@UpdateLedgerResponse' {} a -> s {httpStatus = a} :: UpdateLedgerResponse)

instance Prelude.NFData UpdateLedgerResponse
