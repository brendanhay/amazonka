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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The name of the ledger.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateLedger
newUpdateLedger pName_ =
  UpdateLedger'
    { deletionProtection = Core.Nothing,
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
updateLedger_deletionProtection :: Lens.Lens' UpdateLedger (Core.Maybe Core.Bool)
updateLedger_deletionProtection = Lens.lens (\UpdateLedger' {deletionProtection} -> deletionProtection) (\s@UpdateLedger' {} a -> s {deletionProtection = a} :: UpdateLedger)

-- | The name of the ledger.
updateLedger_name :: Lens.Lens' UpdateLedger Core.Text
updateLedger_name = Lens.lens (\UpdateLedger' {name} -> name) (\s@UpdateLedger' {} a -> s {name = a} :: UpdateLedger)

instance Core.AWSRequest UpdateLedger where
  type AWSResponse UpdateLedger = UpdateLedgerResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLedgerResponse'
            Core.<$> (x Core..?> "DeletionProtection")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "CreationDateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateLedger

instance Core.NFData UpdateLedger

instance Core.ToHeaders UpdateLedger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateLedger where
  toJSON UpdateLedger' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeletionProtection" Core..=)
              Core.<$> deletionProtection
          ]
      )

instance Core.ToPath UpdateLedger where
  toPath UpdateLedger' {..} =
    Core.mconcat ["/ledgers/", Core.toBS name]

instance Core.ToQuery UpdateLedger where
  toQuery = Core.const Core.mempty

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
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) for the ledger.
    arn :: Core.Maybe Core.Text,
    -- | The current status of the ledger.
    state :: Core.Maybe LedgerState,
    -- | The name of the ledger.
    name :: Core.Maybe Core.Text,
    -- | The date and time, in epoch time format, when the ledger was created.
    -- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
    -- January 1, 1970 UTC.)
    creationDateTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateLedgerResponse
newUpdateLedgerResponse pHttpStatus_ =
  UpdateLedgerResponse'
    { deletionProtection =
        Core.Nothing,
      arn = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      creationDateTime = Core.Nothing,
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
updateLedgerResponse_deletionProtection :: Lens.Lens' UpdateLedgerResponse (Core.Maybe Core.Bool)
updateLedgerResponse_deletionProtection = Lens.lens (\UpdateLedgerResponse' {deletionProtection} -> deletionProtection) (\s@UpdateLedgerResponse' {} a -> s {deletionProtection = a} :: UpdateLedgerResponse)

-- | The Amazon Resource Name (ARN) for the ledger.
updateLedgerResponse_arn :: Lens.Lens' UpdateLedgerResponse (Core.Maybe Core.Text)
updateLedgerResponse_arn = Lens.lens (\UpdateLedgerResponse' {arn} -> arn) (\s@UpdateLedgerResponse' {} a -> s {arn = a} :: UpdateLedgerResponse)

-- | The current status of the ledger.
updateLedgerResponse_state :: Lens.Lens' UpdateLedgerResponse (Core.Maybe LedgerState)
updateLedgerResponse_state = Lens.lens (\UpdateLedgerResponse' {state} -> state) (\s@UpdateLedgerResponse' {} a -> s {state = a} :: UpdateLedgerResponse)

-- | The name of the ledger.
updateLedgerResponse_name :: Lens.Lens' UpdateLedgerResponse (Core.Maybe Core.Text)
updateLedgerResponse_name = Lens.lens (\UpdateLedgerResponse' {name} -> name) (\s@UpdateLedgerResponse' {} a -> s {name = a} :: UpdateLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
updateLedgerResponse_creationDateTime :: Lens.Lens' UpdateLedgerResponse (Core.Maybe Core.UTCTime)
updateLedgerResponse_creationDateTime = Lens.lens (\UpdateLedgerResponse' {creationDateTime} -> creationDateTime) (\s@UpdateLedgerResponse' {} a -> s {creationDateTime = a} :: UpdateLedgerResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
updateLedgerResponse_httpStatus :: Lens.Lens' UpdateLedgerResponse Core.Int
updateLedgerResponse_httpStatus = Lens.lens (\UpdateLedgerResponse' {httpStatus} -> httpStatus) (\s@UpdateLedgerResponse' {} a -> s {httpStatus = a} :: UpdateLedgerResponse)

instance Core.NFData UpdateLedgerResponse
