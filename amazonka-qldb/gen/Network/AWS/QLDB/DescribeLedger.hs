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
-- Module      : Network.AWS.QLDB.DescribeLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a ledger, including its state and when it was
-- created.
module Network.AWS.QLDB.DescribeLedger
  ( -- * Creating a Request
    DescribeLedger (..),
    newDescribeLedger,

    -- * Request Lenses
    describeLedger_name,

    -- * Destructuring the Response
    DescribeLedgerResponse (..),
    newDescribeLedgerResponse,

    -- * Response Lenses
    describeLedgerResponse_deletionProtection,
    describeLedgerResponse_arn,
    describeLedgerResponse_state,
    describeLedgerResponse_name,
    describeLedgerResponse_creationDateTime,
    describeLedgerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLedger' smart constructor.
data DescribeLedger = DescribeLedger'
  { -- | The name of the ledger that you want to describe.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeLedger_name' - The name of the ledger that you want to describe.
newDescribeLedger ::
  -- | 'name'
  Core.Text ->
  DescribeLedger
newDescribeLedger pName_ =
  DescribeLedger' {name = pName_}

-- | The name of the ledger that you want to describe.
describeLedger_name :: Lens.Lens' DescribeLedger Core.Text
describeLedger_name = Lens.lens (\DescribeLedger' {name} -> name) (\s@DescribeLedger' {} a -> s {name = a} :: DescribeLedger)

instance Core.AWSRequest DescribeLedger where
  type
    AWSResponse DescribeLedger =
      DescribeLedgerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLedgerResponse'
            Core.<$> (x Core..?> "DeletionProtection")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "CreationDateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLedger

instance Core.NFData DescribeLedger

instance Core.ToHeaders DescribeLedger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeLedger where
  toPath DescribeLedger' {..} =
    Core.mconcat ["/ledgers/", Core.toBS name]

instance Core.ToQuery DescribeLedger where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLedgerResponse' smart constructor.
data DescribeLedgerResponse = DescribeLedgerResponse'
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
-- Create a value of 'DescribeLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'describeLedgerResponse_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
--
-- 'arn', 'describeLedgerResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'state', 'describeLedgerResponse_state' - The current status of the ledger.
--
-- 'name', 'describeLedgerResponse_name' - The name of the ledger.
--
-- 'creationDateTime', 'describeLedgerResponse_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
--
-- 'httpStatus', 'describeLedgerResponse_httpStatus' - The response's http status code.
newDescribeLedgerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLedgerResponse
newDescribeLedgerResponse pHttpStatus_ =
  DescribeLedgerResponse'
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
describeLedgerResponse_deletionProtection :: Lens.Lens' DescribeLedgerResponse (Core.Maybe Core.Bool)
describeLedgerResponse_deletionProtection = Lens.lens (\DescribeLedgerResponse' {deletionProtection} -> deletionProtection) (\s@DescribeLedgerResponse' {} a -> s {deletionProtection = a} :: DescribeLedgerResponse)

-- | The Amazon Resource Name (ARN) for the ledger.
describeLedgerResponse_arn :: Lens.Lens' DescribeLedgerResponse (Core.Maybe Core.Text)
describeLedgerResponse_arn = Lens.lens (\DescribeLedgerResponse' {arn} -> arn) (\s@DescribeLedgerResponse' {} a -> s {arn = a} :: DescribeLedgerResponse)

-- | The current status of the ledger.
describeLedgerResponse_state :: Lens.Lens' DescribeLedgerResponse (Core.Maybe LedgerState)
describeLedgerResponse_state = Lens.lens (\DescribeLedgerResponse' {state} -> state) (\s@DescribeLedgerResponse' {} a -> s {state = a} :: DescribeLedgerResponse)

-- | The name of the ledger.
describeLedgerResponse_name :: Lens.Lens' DescribeLedgerResponse (Core.Maybe Core.Text)
describeLedgerResponse_name = Lens.lens (\DescribeLedgerResponse' {name} -> name) (\s@DescribeLedgerResponse' {} a -> s {name = a} :: DescribeLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
describeLedgerResponse_creationDateTime :: Lens.Lens' DescribeLedgerResponse (Core.Maybe Core.UTCTime)
describeLedgerResponse_creationDateTime = Lens.lens (\DescribeLedgerResponse' {creationDateTime} -> creationDateTime) (\s@DescribeLedgerResponse' {} a -> s {creationDateTime = a} :: DescribeLedgerResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeLedgerResponse_httpStatus :: Lens.Lens' DescribeLedgerResponse Core.Int
describeLedgerResponse_httpStatus = Lens.lens (\DescribeLedgerResponse' {httpStatus} -> httpStatus) (\s@DescribeLedgerResponse' {} a -> s {httpStatus = a} :: DescribeLedgerResponse)

instance Core.NFData DescribeLedgerResponse
