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
-- Module      : Network.AWS.QLDB.CreateLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ledger in your AWS account.
module Network.AWS.QLDB.CreateLedger
  ( -- * Creating a Request
    CreateLedger (..),
    newCreateLedger,

    -- * Request Lenses
    createLedger_deletionProtection,
    createLedger_tags,
    createLedger_name,
    createLedger_permissionsMode,

    -- * Destructuring the Response
    CreateLedgerResponse (..),
    newCreateLedgerResponse,

    -- * Response Lenses
    createLedgerResponse_deletionProtection,
    createLedgerResponse_arn,
    createLedgerResponse_state,
    createLedgerResponse_name,
    createLedgerResponse_creationDateTime,
    createLedgerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLedger' smart constructor.
data CreateLedger = CreateLedger'
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
    -- | The key-value pairs to add as tags to the ledger that you want to
    -- create. Tag keys are case sensitive. Tag values are case sensitive and
    -- can be null.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the ledger that you want to create. The name must be unique
    -- among all of your ledgers in the current AWS Region.
    --
    -- Naming constraints for ledger names are defined in
    -- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
    -- in the /Amazon QLDB Developer Guide/.
    name :: Core.Text,
    -- | The permissions mode to assign to the ledger that you want to create.
    permissionsMode :: PermissionsMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'createLedger_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
--
-- 'tags', 'createLedger_tags' - The key-value pairs to add as tags to the ledger that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
--
-- 'name', 'createLedger_name' - The name of the ledger that you want to create. The name must be unique
-- among all of your ledgers in the current AWS Region.
--
-- Naming constraints for ledger names are defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
--
-- 'permissionsMode', 'createLedger_permissionsMode' - The permissions mode to assign to the ledger that you want to create.
newCreateLedger ::
  -- | 'name'
  Core.Text ->
  -- | 'permissionsMode'
  PermissionsMode ->
  CreateLedger
newCreateLedger pName_ pPermissionsMode_ =
  CreateLedger'
    { deletionProtection = Core.Nothing,
      tags = Core.Nothing,
      name = pName_,
      permissionsMode = pPermissionsMode_
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
createLedger_deletionProtection :: Lens.Lens' CreateLedger (Core.Maybe Core.Bool)
createLedger_deletionProtection = Lens.lens (\CreateLedger' {deletionProtection} -> deletionProtection) (\s@CreateLedger' {} a -> s {deletionProtection = a} :: CreateLedger)

-- | The key-value pairs to add as tags to the ledger that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
createLedger_tags :: Lens.Lens' CreateLedger (Core.Maybe (Core.HashMap Core.Text Core.Text))
createLedger_tags = Lens.lens (\CreateLedger' {tags} -> tags) (\s@CreateLedger' {} a -> s {tags = a} :: CreateLedger) Core.. Lens.mapping Lens._Coerce

-- | The name of the ledger that you want to create. The name must be unique
-- among all of your ledgers in the current AWS Region.
--
-- Naming constraints for ledger names are defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
createLedger_name :: Lens.Lens' CreateLedger Core.Text
createLedger_name = Lens.lens (\CreateLedger' {name} -> name) (\s@CreateLedger' {} a -> s {name = a} :: CreateLedger)

-- | The permissions mode to assign to the ledger that you want to create.
createLedger_permissionsMode :: Lens.Lens' CreateLedger PermissionsMode
createLedger_permissionsMode = Lens.lens (\CreateLedger' {permissionsMode} -> permissionsMode) (\s@CreateLedger' {} a -> s {permissionsMode = a} :: CreateLedger)

instance Core.AWSRequest CreateLedger where
  type AWSResponse CreateLedger = CreateLedgerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLedgerResponse'
            Core.<$> (x Core..?> "DeletionProtection")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "State")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "CreationDateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLedger

instance Core.NFData CreateLedger

instance Core.ToHeaders CreateLedger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateLedger where
  toJSON CreateLedger' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeletionProtection" Core..=)
              Core.<$> deletionProtection,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("PermissionsMode" Core..= permissionsMode)
          ]
      )

instance Core.ToPath CreateLedger where
  toPath = Core.const "/ledgers"

instance Core.ToQuery CreateLedger where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateLedgerResponse' smart constructor.
data CreateLedgerResponse = CreateLedgerResponse'
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
-- Create a value of 'CreateLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'createLedgerResponse_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger using the QLDB API or the AWS Command Line
-- Interface (AWS CLI). You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@. The QLDB console disables deletion
-- protection for you when you use it to delete a ledger.
--
-- 'arn', 'createLedgerResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'state', 'createLedgerResponse_state' - The current status of the ledger.
--
-- 'name', 'createLedgerResponse_name' - The name of the ledger.
--
-- 'creationDateTime', 'createLedgerResponse_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
--
-- 'httpStatus', 'createLedgerResponse_httpStatus' - The response's http status code.
newCreateLedgerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLedgerResponse
newCreateLedgerResponse pHttpStatus_ =
  CreateLedgerResponse'
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
createLedgerResponse_deletionProtection :: Lens.Lens' CreateLedgerResponse (Core.Maybe Core.Bool)
createLedgerResponse_deletionProtection = Lens.lens (\CreateLedgerResponse' {deletionProtection} -> deletionProtection) (\s@CreateLedgerResponse' {} a -> s {deletionProtection = a} :: CreateLedgerResponse)

-- | The Amazon Resource Name (ARN) for the ledger.
createLedgerResponse_arn :: Lens.Lens' CreateLedgerResponse (Core.Maybe Core.Text)
createLedgerResponse_arn = Lens.lens (\CreateLedgerResponse' {arn} -> arn) (\s@CreateLedgerResponse' {} a -> s {arn = a} :: CreateLedgerResponse)

-- | The current status of the ledger.
createLedgerResponse_state :: Lens.Lens' CreateLedgerResponse (Core.Maybe LedgerState)
createLedgerResponse_state = Lens.lens (\CreateLedgerResponse' {state} -> state) (\s@CreateLedgerResponse' {} a -> s {state = a} :: CreateLedgerResponse)

-- | The name of the ledger.
createLedgerResponse_name :: Lens.Lens' CreateLedgerResponse (Core.Maybe Core.Text)
createLedgerResponse_name = Lens.lens (\CreateLedgerResponse' {name} -> name) (\s@CreateLedgerResponse' {} a -> s {name = a} :: CreateLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
createLedgerResponse_creationDateTime :: Lens.Lens' CreateLedgerResponse (Core.Maybe Core.UTCTime)
createLedgerResponse_creationDateTime = Lens.lens (\CreateLedgerResponse' {creationDateTime} -> creationDateTime) (\s@CreateLedgerResponse' {} a -> s {creationDateTime = a} :: CreateLedgerResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
createLedgerResponse_httpStatus :: Lens.Lens' CreateLedgerResponse Core.Int
createLedgerResponse_httpStatus = Lens.lens (\CreateLedgerResponse' {httpStatus} -> httpStatus) (\s@CreateLedgerResponse' {} a -> s {httpStatus = a} :: CreateLedgerResponse)

instance Core.NFData CreateLedgerResponse
