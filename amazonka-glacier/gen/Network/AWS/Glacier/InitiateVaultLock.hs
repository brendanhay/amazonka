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
-- Module      : Network.AWS.Glacier.InitiateVaultLock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the vault locking process by doing the
-- following:
--
-- -   Installing a vault lock policy on the specified vault.
--
-- -   Setting the lock state of vault lock to @InProgress@.
--
-- -   Returning a lock ID, which is used to complete the vault locking
--     process.
--
-- You can set one vault lock policy for each vault and this policy can be
-- up to 20 KB in size. For more information about vault lock policies, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies>.
--
-- You must complete the vault locking process within 24 hours after the
-- vault lock enters the @InProgress@ state. After the 24 hour window ends,
-- the lock ID expires, the vault automatically exits the @InProgress@
-- state, and the vault lock policy is removed from the vault. You call
-- CompleteVaultLock to complete the vault locking process by setting the
-- state of the vault lock to @Locked@.
--
-- After a vault lock is in the @Locked@ state, you cannot initiate a new
-- vault lock for the vault.
--
-- You can abort the vault locking process by calling AbortVaultLock. You
-- can get the state of the vault lock by calling GetVaultLock. For more
-- information about the vault locking process,
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- If this operation is called when the vault lock is in the @InProgress@
-- state, the operation returns an @AccessDeniedException@ error. When the
-- vault lock is in the @InProgress@ state you must call AbortVaultLock
-- before you can initiate a new vault lock policy.
module Network.AWS.Glacier.InitiateVaultLock
  ( -- * Creating a Request
    InitiateVaultLock (..),
    newInitiateVaultLock,

    -- * Request Lenses
    initiateVaultLock_policy,
    initiateVaultLock_accountId,
    initiateVaultLock_vaultName,

    -- * Destructuring the Response
    InitiateVaultLockResponse (..),
    newInitiateVaultLockResponse,

    -- * Response Lenses
    initiateVaultLockResponse_lockId,
    initiateVaultLockResponse_httpStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @InitiateVaultLock@.
--
-- /See:/ 'newInitiateVaultLock' smart constructor.
data InitiateVaultLock = InitiateVaultLock'
  { -- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
    -- character.
    policy :: Prelude.Maybe VaultLockPolicy,
    -- | The @AccountId@ value is the AWS account ID. This value must match the
    -- AWS account ID associated with the credentials used to sign the request.
    -- You can either specify an AWS account ID or optionally a single \'@-@\'
    -- (hyphen), in which case Amazon Glacier uses the AWS account ID
    -- associated with the credentials used to sign the request. If you specify
    -- your account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InitiateVaultLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'initiateVaultLock_policy' - The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
--
-- 'accountId', 'initiateVaultLock_accountId' - The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'initiateVaultLock_vaultName' - The name of the vault.
newInitiateVaultLock ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  InitiateVaultLock
newInitiateVaultLock pAccountId_ pVaultName_ =
  InitiateVaultLock'
    { policy = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
initiateVaultLock_policy :: Lens.Lens' InitiateVaultLock (Prelude.Maybe VaultLockPolicy)
initiateVaultLock_policy = Lens.lens (\InitiateVaultLock' {policy} -> policy) (\s@InitiateVaultLock' {} a -> s {policy = a} :: InitiateVaultLock)

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single \'@-@\'
-- (hyphen), in which case Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request. If you specify
-- your account ID, do not include any hyphens (\'-\') in the ID.
initiateVaultLock_accountId :: Lens.Lens' InitiateVaultLock Prelude.Text
initiateVaultLock_accountId = Lens.lens (\InitiateVaultLock' {accountId} -> accountId) (\s@InitiateVaultLock' {} a -> s {accountId = a} :: InitiateVaultLock)

-- | The name of the vault.
initiateVaultLock_vaultName :: Lens.Lens' InitiateVaultLock Prelude.Text
initiateVaultLock_vaultName = Lens.lens (\InitiateVaultLock' {vaultName} -> vaultName) (\s@InitiateVaultLock' {} a -> s {vaultName = a} :: InitiateVaultLock)

instance Prelude.AWSRequest InitiateVaultLock where
  type Rs InitiateVaultLock = InitiateVaultLockResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateVaultLockResponse'
            Prelude.<$> (h Prelude..#? "x-amz-lock-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitiateVaultLock

instance Prelude.NFData InitiateVaultLock

instance Prelude.ToHeaders InitiateVaultLock where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON InitiateVaultLock where
  toJSON InitiateVaultLock' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("policy" Prelude..=) Prelude.<$> policy]
      )

instance Prelude.ToPath InitiateVaultLock where
  toPath InitiateVaultLock' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/lock-policy"
      ]

instance Prelude.ToQuery InitiateVaultLock where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newInitiateVaultLockResponse' smart constructor.
data InitiateVaultLockResponse = InitiateVaultLockResponse'
  { -- | The lock ID, which is used to complete the vault locking process.
    lockId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InitiateVaultLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lockId', 'initiateVaultLockResponse_lockId' - The lock ID, which is used to complete the vault locking process.
--
-- 'httpStatus', 'initiateVaultLockResponse_httpStatus' - The response's http status code.
newInitiateVaultLockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitiateVaultLockResponse
newInitiateVaultLockResponse pHttpStatus_ =
  InitiateVaultLockResponse'
    { lockId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lock ID, which is used to complete the vault locking process.
initiateVaultLockResponse_lockId :: Lens.Lens' InitiateVaultLockResponse (Prelude.Maybe Prelude.Text)
initiateVaultLockResponse_lockId = Lens.lens (\InitiateVaultLockResponse' {lockId} -> lockId) (\s@InitiateVaultLockResponse' {} a -> s {lockId = a} :: InitiateVaultLockResponse)

-- | The response's http status code.
initiateVaultLockResponse_httpStatus :: Lens.Lens' InitiateVaultLockResponse Prelude.Int
initiateVaultLockResponse_httpStatus = Lens.lens (\InitiateVaultLockResponse' {httpStatus} -> httpStatus) (\s@InitiateVaultLockResponse' {} a -> s {httpStatus = a} :: InitiateVaultLockResponse)

instance Prelude.NFData InitiateVaultLockResponse
