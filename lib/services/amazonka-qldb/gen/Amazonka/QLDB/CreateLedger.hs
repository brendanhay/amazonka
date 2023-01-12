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
-- Module      : Amazonka.QLDB.CreateLedger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ledger in your Amazon Web Services account in the current
-- Region.
module Amazonka.QLDB.CreateLedger
  ( -- * Creating a Request
    CreateLedger (..),
    newCreateLedger,

    -- * Request Lenses
    createLedger_deletionProtection,
    createLedger_kmsKey,
    createLedger_tags,
    createLedger_name,
    createLedger_permissionsMode,

    -- * Destructuring the Response
    CreateLedgerResponse (..),
    newCreateLedgerResponse,

    -- * Response Lenses
    createLedgerResponse_arn,
    createLedgerResponse_creationDateTime,
    createLedgerResponse_deletionProtection,
    createLedgerResponse_kmsKeyArn,
    createLedgerResponse_name,
    createLedgerResponse_permissionsMode,
    createLedgerResponse_state,
    createLedgerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLedger' smart constructor.
data CreateLedger = CreateLedger'
  { -- | The flag that prevents a ledger from being deleted by any user. If not
    -- provided on ledger creation, this feature is enabled (@true@) by
    -- default.
    --
    -- If deletion protection is enabled, you must first disable it before you
    -- can delete the ledger. You can disable it by calling the @UpdateLedger@
    -- operation to set the flag to @false@.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The key in Key Management Service (KMS) to use for encryption of data at
    -- rest in the ledger. For more information, see
    -- <https://docs.aws.amazon.com/qldb/latest/developerguide/encryption-at-rest.html Encryption at rest>
    -- in the /Amazon QLDB Developer Guide/.
    --
    -- Use one of the following options to specify this parameter:
    --
    -- -   @AWS_OWNED_KMS_KEY@: Use an KMS key that is owned and managed by
    --     Amazon Web Services on your behalf.
    --
    -- -   __Undefined__: By default, use an Amazon Web Services owned KMS key.
    --
    -- -   __A valid symmetric customer managed KMS key__: Use the specified
    --     KMS key in your account that you create, own, and manage.
    --
    --     Amazon QLDB does not support asymmetric keys. For more information,
    --     see
    --     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
    --     in the /Key Management Service Developer Guide/.
    --
    -- To specify a customer managed KMS key, you can use its key ID, Amazon
    -- Resource Name (ARN), alias name, or alias ARN. When using an alias name,
    -- prefix it with @\"alias\/\"@. To specify a key in a different Amazon Web
    -- Services account, you must use the key ARN or alias ARN.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>
    -- in the /Key Management Service Developer Guide/.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The key-value pairs to add as tags to the ledger that you want to
    -- create. Tag keys are case sensitive. Tag values are case sensitive and
    -- can be null.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the ledger that you want to create. The name must be unique
    -- among all of the ledgers in your Amazon Web Services account in the
    -- current Region.
    --
    -- Naming constraints for ledger names are defined in
    -- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
    -- in the /Amazon QLDB Developer Guide/.
    name :: Prelude.Text,
    -- | The permissions mode to assign to the ledger that you want to create.
    -- This parameter can have one of the following values:
    --
    -- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
    --     with API-level granularity for ledgers.
    --
    --     This mode allows users who have the @SendCommand@ API permission for
    --     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
    --     tables in the specified ledger. This mode disregards any table-level
    --     or command-level IAM permissions policies that you create for the
    --     ledger.
    --
    -- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
    --     control with finer granularity for ledgers, tables, and PartiQL
    --     commands.
    --
    --     By default, this mode denies all user requests to run any PartiQL
    --     commands on any tables in this ledger. To allow PartiQL commands to
    --     run, you must create IAM permissions policies for specific table
    --     resources and PartiQL actions, in addition to the @SendCommand@ API
    --     permission for the ledger. For information, see
    --     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
    --     in the /Amazon QLDB Developer Guide/.
    --
    -- We strongly recommend using the @STANDARD@ permissions mode to maximize
    -- the security of your ledger data.
    permissionsMode :: PermissionsMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
--
-- 'kmsKey', 'createLedger_kmsKey' - The key in Key Management Service (KMS) to use for encryption of data at
-- rest in the ledger. For more information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/encryption-at-rest.html Encryption at rest>
-- in the /Amazon QLDB Developer Guide/.
--
-- Use one of the following options to specify this parameter:
--
-- -   @AWS_OWNED_KMS_KEY@: Use an KMS key that is owned and managed by
--     Amazon Web Services on your behalf.
--
-- -   __Undefined__: By default, use an Amazon Web Services owned KMS key.
--
-- -   __A valid symmetric customer managed KMS key__: Use the specified
--     KMS key in your account that you create, own, and manage.
--
--     Amazon QLDB does not support asymmetric keys. For more information,
--     see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /Key Management Service Developer Guide/.
--
-- To specify a customer managed KMS key, you can use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. When using an alias name,
-- prefix it with @\"alias\/\"@. To specify a key in a different Amazon Web
-- Services account, you must use the key ARN or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>
-- in the /Key Management Service Developer Guide/.
--
-- 'tags', 'createLedger_tags' - The key-value pairs to add as tags to the ledger that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
--
-- 'name', 'createLedger_name' - The name of the ledger that you want to create. The name must be unique
-- among all of the ledgers in your Amazon Web Services account in the
-- current Region.
--
-- Naming constraints for ledger names are defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
--
-- 'permissionsMode', 'createLedger_permissionsMode' - The permissions mode to assign to the ledger that you want to create.
-- This parameter can have one of the following values:
--
-- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
--     with API-level granularity for ledgers.
--
--     This mode allows users who have the @SendCommand@ API permission for
--     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
--     tables in the specified ledger. This mode disregards any table-level
--     or command-level IAM permissions policies that you create for the
--     ledger.
--
-- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
--     control with finer granularity for ledgers, tables, and PartiQL
--     commands.
--
--     By default, this mode denies all user requests to run any PartiQL
--     commands on any tables in this ledger. To allow PartiQL commands to
--     run, you must create IAM permissions policies for specific table
--     resources and PartiQL actions, in addition to the @SendCommand@ API
--     permission for the ledger. For information, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
--     in the /Amazon QLDB Developer Guide/.
--
-- We strongly recommend using the @STANDARD@ permissions mode to maximize
-- the security of your ledger data.
newCreateLedger ::
  -- | 'name'
  Prelude.Text ->
  -- | 'permissionsMode'
  PermissionsMode ->
  CreateLedger
newCreateLedger pName_ pPermissionsMode_ =
  CreateLedger'
    { deletionProtection = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      permissionsMode = pPermissionsMode_
    }

-- | The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
createLedger_deletionProtection :: Lens.Lens' CreateLedger (Prelude.Maybe Prelude.Bool)
createLedger_deletionProtection = Lens.lens (\CreateLedger' {deletionProtection} -> deletionProtection) (\s@CreateLedger' {} a -> s {deletionProtection = a} :: CreateLedger)

-- | The key in Key Management Service (KMS) to use for encryption of data at
-- rest in the ledger. For more information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/encryption-at-rest.html Encryption at rest>
-- in the /Amazon QLDB Developer Guide/.
--
-- Use one of the following options to specify this parameter:
--
-- -   @AWS_OWNED_KMS_KEY@: Use an KMS key that is owned and managed by
--     Amazon Web Services on your behalf.
--
-- -   __Undefined__: By default, use an Amazon Web Services owned KMS key.
--
-- -   __A valid symmetric customer managed KMS key__: Use the specified
--     KMS key in your account that you create, own, and manage.
--
--     Amazon QLDB does not support asymmetric keys. For more information,
--     see
--     <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
--     in the /Key Management Service Developer Guide/.
--
-- To specify a customer managed KMS key, you can use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. When using an alias name,
-- prefix it with @\"alias\/\"@. To specify a key in a different Amazon Web
-- Services account, you must use the key ARN or alias ARN.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias ARN: @arn:aws:kms:us-east-2:111122223333:alias\/ExampleAlias@
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>
-- in the /Key Management Service Developer Guide/.
createLedger_kmsKey :: Lens.Lens' CreateLedger (Prelude.Maybe Prelude.Text)
createLedger_kmsKey = Lens.lens (\CreateLedger' {kmsKey} -> kmsKey) (\s@CreateLedger' {} a -> s {kmsKey = a} :: CreateLedger)

-- | The key-value pairs to add as tags to the ledger that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
createLedger_tags :: Lens.Lens' CreateLedger (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLedger_tags = Lens.lens (\CreateLedger' {tags} -> tags) (\s@CreateLedger' {} a -> s {tags = a} :: CreateLedger) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ledger that you want to create. The name must be unique
-- among all of the ledgers in your Amazon Web Services account in the
-- current Region.
--
-- Naming constraints for ledger names are defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
createLedger_name :: Lens.Lens' CreateLedger Prelude.Text
createLedger_name = Lens.lens (\CreateLedger' {name} -> name) (\s@CreateLedger' {} a -> s {name = a} :: CreateLedger)

-- | The permissions mode to assign to the ledger that you want to create.
-- This parameter can have one of the following values:
--
-- -   @ALLOW_ALL@: A legacy permissions mode that enables access control
--     with API-level granularity for ledgers.
--
--     This mode allows users who have the @SendCommand@ API permission for
--     this ledger to run all PartiQL commands (hence, @ALLOW_ALL@) on any
--     tables in the specified ledger. This mode disregards any table-level
--     or command-level IAM permissions policies that you create for the
--     ledger.
--
-- -   @STANDARD@: (/Recommended/) A permissions mode that enables access
--     control with finer granularity for ledgers, tables, and PartiQL
--     commands.
--
--     By default, this mode denies all user requests to run any PartiQL
--     commands on any tables in this ledger. To allow PartiQL commands to
--     run, you must create IAM permissions policies for specific table
--     resources and PartiQL actions, in addition to the @SendCommand@ API
--     permission for the ledger. For information, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-standard-mode.html Getting started with the standard permissions mode>
--     in the /Amazon QLDB Developer Guide/.
--
-- We strongly recommend using the @STANDARD@ permissions mode to maximize
-- the security of your ledger data.
createLedger_permissionsMode :: Lens.Lens' CreateLedger PermissionsMode
createLedger_permissionsMode = Lens.lens (\CreateLedger' {permissionsMode} -> permissionsMode) (\s@CreateLedger' {} a -> s {permissionsMode = a} :: CreateLedger)

instance Core.AWSRequest CreateLedger where
  type AWSResponse CreateLedger = CreateLedgerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLedgerResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationDateTime")
            Prelude.<*> (x Data..?> "DeletionProtection")
            Prelude.<*> (x Data..?> "KmsKeyArn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "PermissionsMode")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLedger where
  hashWithSalt _salt CreateLedger' {..} =
    _salt `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionsMode

instance Prelude.NFData CreateLedger where
  rnf CreateLedger' {..} =
    Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionsMode

instance Data.ToHeaders CreateLedger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLedger where
  toJSON CreateLedger' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeletionProtection" Data..=)
              Prelude.<$> deletionProtection,
            ("KmsKey" Data..=) Prelude.<$> kmsKey,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("PermissionsMode" Data..= permissionsMode)
          ]
      )

instance Data.ToPath CreateLedger where
  toPath = Prelude.const "/ledgers"

instance Data.ToQuery CreateLedger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLedgerResponse' smart constructor.
data CreateLedgerResponse = CreateLedgerResponse'
  { -- | The Amazon Resource Name (ARN) for the ledger.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in epoch time format, when the ledger was created.
    -- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
    -- January 1, 1970 UTC.)
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The flag that prevents a ledger from being deleted by any user. If not
    -- provided on ledger creation, this feature is enabled (@true@) by
    -- default.
    --
    -- If deletion protection is enabled, you must first disable it before you
    -- can delete the ledger. You can disable it by calling the @UpdateLedger@
    -- operation to set the flag to @false@.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the customer managed KMS key that the ledger uses for
    -- encryption at rest. If this parameter is undefined, the ledger uses an
    -- Amazon Web Services owned KMS key for encryption.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the ledger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The permissions mode of the ledger that you created.
    permissionsMode :: Prelude.Maybe PermissionsMode,
    -- | The current status of the ledger.
    state :: Prelude.Maybe LedgerState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createLedgerResponse_arn' - The Amazon Resource Name (ARN) for the ledger.
--
-- 'creationDateTime', 'createLedgerResponse_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
--
-- 'deletionProtection', 'createLedgerResponse_deletionProtection' - The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
--
-- 'kmsKeyArn', 'createLedgerResponse_kmsKeyArn' - The ARN of the customer managed KMS key that the ledger uses for
-- encryption at rest. If this parameter is undefined, the ledger uses an
-- Amazon Web Services owned KMS key for encryption.
--
-- 'name', 'createLedgerResponse_name' - The name of the ledger.
--
-- 'permissionsMode', 'createLedgerResponse_permissionsMode' - The permissions mode of the ledger that you created.
--
-- 'state', 'createLedgerResponse_state' - The current status of the ledger.
--
-- 'httpStatus', 'createLedgerResponse_httpStatus' - The response's http status code.
newCreateLedgerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLedgerResponse
newCreateLedgerResponse pHttpStatus_ =
  CreateLedgerResponse'
    { arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      name = Prelude.Nothing,
      permissionsMode = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the ledger.
createLedgerResponse_arn :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe Prelude.Text)
createLedgerResponse_arn = Lens.lens (\CreateLedgerResponse' {arn} -> arn) (\s@CreateLedgerResponse' {} a -> s {arn = a} :: CreateLedgerResponse)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
createLedgerResponse_creationDateTime :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe Prelude.UTCTime)
createLedgerResponse_creationDateTime = Lens.lens (\CreateLedgerResponse' {creationDateTime} -> creationDateTime) (\s@CreateLedgerResponse' {} a -> s {creationDateTime = a} :: CreateLedgerResponse) Prelude.. Lens.mapping Data._Time

-- | The flag that prevents a ledger from being deleted by any user. If not
-- provided on ledger creation, this feature is enabled (@true@) by
-- default.
--
-- If deletion protection is enabled, you must first disable it before you
-- can delete the ledger. You can disable it by calling the @UpdateLedger@
-- operation to set the flag to @false@.
createLedgerResponse_deletionProtection :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe Prelude.Bool)
createLedgerResponse_deletionProtection = Lens.lens (\CreateLedgerResponse' {deletionProtection} -> deletionProtection) (\s@CreateLedgerResponse' {} a -> s {deletionProtection = a} :: CreateLedgerResponse)

-- | The ARN of the customer managed KMS key that the ledger uses for
-- encryption at rest. If this parameter is undefined, the ledger uses an
-- Amazon Web Services owned KMS key for encryption.
createLedgerResponse_kmsKeyArn :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe Prelude.Text)
createLedgerResponse_kmsKeyArn = Lens.lens (\CreateLedgerResponse' {kmsKeyArn} -> kmsKeyArn) (\s@CreateLedgerResponse' {} a -> s {kmsKeyArn = a} :: CreateLedgerResponse)

-- | The name of the ledger.
createLedgerResponse_name :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe Prelude.Text)
createLedgerResponse_name = Lens.lens (\CreateLedgerResponse' {name} -> name) (\s@CreateLedgerResponse' {} a -> s {name = a} :: CreateLedgerResponse)

-- | The permissions mode of the ledger that you created.
createLedgerResponse_permissionsMode :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe PermissionsMode)
createLedgerResponse_permissionsMode = Lens.lens (\CreateLedgerResponse' {permissionsMode} -> permissionsMode) (\s@CreateLedgerResponse' {} a -> s {permissionsMode = a} :: CreateLedgerResponse)

-- | The current status of the ledger.
createLedgerResponse_state :: Lens.Lens' CreateLedgerResponse (Prelude.Maybe LedgerState)
createLedgerResponse_state = Lens.lens (\CreateLedgerResponse' {state} -> state) (\s@CreateLedgerResponse' {} a -> s {state = a} :: CreateLedgerResponse)

-- | The response's http status code.
createLedgerResponse_httpStatus :: Lens.Lens' CreateLedgerResponse Prelude.Int
createLedgerResponse_httpStatus = Lens.lens (\CreateLedgerResponse' {httpStatus} -> httpStatus) (\s@CreateLedgerResponse' {} a -> s {httpStatus = a} :: CreateLedgerResponse)

instance Prelude.NFData CreateLedgerResponse where
  rnf CreateLedgerResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionsMode
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
