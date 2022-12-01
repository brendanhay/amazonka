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
-- Module      : Amazonka.QLDB.DescribeLedger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a ledger, including its state, permissions
-- mode, encryption at rest settings, and when it was created.
module Amazonka.QLDB.DescribeLedger
  ( -- * Creating a Request
    DescribeLedger (..),
    newDescribeLedger,

    -- * Request Lenses
    describeLedger_name,

    -- * Destructuring the Response
    DescribeLedgerResponse (..),
    newDescribeLedgerResponse,

    -- * Response Lenses
    describeLedgerResponse_name,
    describeLedgerResponse_creationDateTime,
    describeLedgerResponse_arn,
    describeLedgerResponse_state,
    describeLedgerResponse_encryptionDescription,
    describeLedgerResponse_deletionProtection,
    describeLedgerResponse_permissionsMode,
    describeLedgerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLedger' smart constructor.
data DescribeLedger = DescribeLedger'
  { -- | The name of the ledger that you want to describe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeLedger
newDescribeLedger pName_ =
  DescribeLedger' {name = pName_}

-- | The name of the ledger that you want to describe.
describeLedger_name :: Lens.Lens' DescribeLedger Prelude.Text
describeLedger_name = Lens.lens (\DescribeLedger' {name} -> name) (\s@DescribeLedger' {} a -> s {name = a} :: DescribeLedger)

instance Core.AWSRequest DescribeLedger where
  type
    AWSResponse DescribeLedger =
      DescribeLedgerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLedgerResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "CreationDateTime")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "EncryptionDescription")
            Prelude.<*> (x Core..?> "DeletionProtection")
            Prelude.<*> (x Core..?> "PermissionsMode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLedger where
  hashWithSalt _salt DescribeLedger' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeLedger where
  rnf DescribeLedger' {..} = Prelude.rnf name

instance Core.ToHeaders DescribeLedger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeLedger where
  toPath DescribeLedger' {..} =
    Prelude.mconcat ["/ledgers/", Core.toBS name]

instance Core.ToQuery DescribeLedger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLedgerResponse' smart constructor.
data DescribeLedgerResponse = DescribeLedgerResponse'
  { -- | The name of the ledger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in epoch time format, when the ledger was created.
    -- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
    -- January 1, 1970 UTC.)
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) for the ledger.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the ledger.
    state :: Prelude.Maybe LedgerState,
    -- | Information about the encryption of data at rest in the ledger. This
    -- includes the current status, the KMS key, and when the key became
    -- inaccessible (in the case of an error).
    encryptionDescription :: Prelude.Maybe LedgerEncryptionDescription,
    -- | The flag that prevents a ledger from being deleted by any user. If not
    -- provided on ledger creation, this feature is enabled (@true@) by
    -- default.
    --
    -- If deletion protection is enabled, you must first disable it before you
    -- can delete the ledger. You can disable it by calling the @UpdateLedger@
    -- operation to set the flag to @false@.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The permissions mode of the ledger.
    permissionsMode :: Prelude.Maybe PermissionsMode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeLedgerResponse_name' - The name of the ledger.
--
-- 'creationDateTime', 'describeLedgerResponse_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
--
-- 'arn', 'describeLedgerResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'state', 'describeLedgerResponse_state' - The current status of the ledger.
--
-- 'encryptionDescription', 'describeLedgerResponse_encryptionDescription' - Information about the encryption of data at rest in the ledger. This
-- includes the current status, the KMS key, and when the key became
-- inaccessible (in the case of an error).
--
-- 'deletionProtection', 'describeLedgerResponse_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
--
-- 'permissionsMode', 'describeLedgerResponse_permissionsMode' - The permissions mode of the ledger.
--
-- 'httpStatus', 'describeLedgerResponse_httpStatus' - The response's http status code.
newDescribeLedgerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLedgerResponse
newDescribeLedgerResponse pHttpStatus_ =
  DescribeLedgerResponse'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      encryptionDescription = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      permissionsMode = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the ledger.
describeLedgerResponse_name :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe Prelude.Text)
describeLedgerResponse_name = Lens.lens (\DescribeLedgerResponse' {name} -> name) (\s@DescribeLedgerResponse' {} a -> s {name = a} :: DescribeLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
describeLedgerResponse_creationDateTime :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe Prelude.UTCTime)
describeLedgerResponse_creationDateTime = Lens.lens (\DescribeLedgerResponse' {creationDateTime} -> creationDateTime) (\s@DescribeLedgerResponse' {} a -> s {creationDateTime = a} :: DescribeLedgerResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the ledger.
describeLedgerResponse_arn :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe Prelude.Text)
describeLedgerResponse_arn = Lens.lens (\DescribeLedgerResponse' {arn} -> arn) (\s@DescribeLedgerResponse' {} a -> s {arn = a} :: DescribeLedgerResponse)

-- | The current status of the ledger.
describeLedgerResponse_state :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe LedgerState)
describeLedgerResponse_state = Lens.lens (\DescribeLedgerResponse' {state} -> state) (\s@DescribeLedgerResponse' {} a -> s {state = a} :: DescribeLedgerResponse)

-- | Information about the encryption of data at rest in the ledger. This
-- includes the current status, the KMS key, and when the key became
-- inaccessible (in the case of an error).
describeLedgerResponse_encryptionDescription :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe LedgerEncryptionDescription)
describeLedgerResponse_encryptionDescription = Lens.lens (\DescribeLedgerResponse' {encryptionDescription} -> encryptionDescription) (\s@DescribeLedgerResponse' {} a -> s {encryptionDescription = a} :: DescribeLedgerResponse)

-- | The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
describeLedgerResponse_deletionProtection :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe Prelude.Bool)
describeLedgerResponse_deletionProtection = Lens.lens (\DescribeLedgerResponse' {deletionProtection} -> deletionProtection) (\s@DescribeLedgerResponse' {} a -> s {deletionProtection = a} :: DescribeLedgerResponse)

-- | The permissions mode of the ledger.
describeLedgerResponse_permissionsMode :: Lens.Lens' DescribeLedgerResponse (Prelude.Maybe PermissionsMode)
describeLedgerResponse_permissionsMode = Lens.lens (\DescribeLedgerResponse' {permissionsMode} -> permissionsMode) (\s@DescribeLedgerResponse' {} a -> s {permissionsMode = a} :: DescribeLedgerResponse)

-- | The response's http status code.
describeLedgerResponse_httpStatus :: Lens.Lens' DescribeLedgerResponse Prelude.Int
describeLedgerResponse_httpStatus = Lens.lens (\DescribeLedgerResponse' {httpStatus} -> httpStatus) (\s@DescribeLedgerResponse' {} a -> s {httpStatus = a} :: DescribeLedgerResponse)

instance Prelude.NFData DescribeLedgerResponse where
  rnf DescribeLedgerResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf encryptionDescription
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf permissionsMode
      `Prelude.seq` Prelude.rnf httpStatus
