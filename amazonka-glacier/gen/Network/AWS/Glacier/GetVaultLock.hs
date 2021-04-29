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
-- Module      : Network.AWS.Glacier.GetVaultLock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the following attributes from the @lock-policy@
-- subresource set on the specified vault:
--
-- -   The vault lock policy set on the vault.
--
-- -   The state of the vault lock, which is either @InProgess@ or
--     @Locked@.
--
-- -   When the lock ID expires. The lock ID is used to complete the vault
--     locking process.
--
-- -   When the vault lock was initiated and put into the @InProgress@
--     state.
--
-- A vault lock is put into the @InProgress@ state by calling
-- InitiateVaultLock. A vault lock is put into the @Locked@ state by
-- calling CompleteVaultLock. You can abort the vault locking process by
-- calling AbortVaultLock. For more information about the vault locking
-- process,
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock>.
--
-- If there is no vault lock policy set on the vault, the operation returns
-- a @404 Not found@ error. For more information about vault lock policies,
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies>.
module Network.AWS.Glacier.GetVaultLock
  ( -- * Creating a Request
    GetVaultLock (..),
    newGetVaultLock,

    -- * Request Lenses
    getVaultLock_accountId,
    getVaultLock_vaultName,

    -- * Destructuring the Response
    GetVaultLockResponse (..),
    newGetVaultLockResponse,

    -- * Response Lenses
    getVaultLockResponse_creationDate,
    getVaultLockResponse_state,
    getVaultLockResponse_expirationDate,
    getVaultLockResponse_policy,
    getVaultLockResponse_httpStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @GetVaultLock@.
--
-- /See:/ 'newGetVaultLock' smart constructor.
data GetVaultLock = GetVaultLock'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetVaultLock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getVaultLock_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'getVaultLock_vaultName' - The name of the vault.
newGetVaultLock ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  GetVaultLock
newGetVaultLock pAccountId_ pVaultName_ =
  GetVaultLock'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
getVaultLock_accountId :: Lens.Lens' GetVaultLock Prelude.Text
getVaultLock_accountId = Lens.lens (\GetVaultLock' {accountId} -> accountId) (\s@GetVaultLock' {} a -> s {accountId = a} :: GetVaultLock)

-- | The name of the vault.
getVaultLock_vaultName :: Lens.Lens' GetVaultLock Prelude.Text
getVaultLock_vaultName = Lens.lens (\GetVaultLock' {vaultName} -> vaultName) (\s@GetVaultLock' {} a -> s {vaultName = a} :: GetVaultLock)

instance Prelude.AWSRequest GetVaultLock where
  type Rs GetVaultLock = GetVaultLockResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVaultLockResponse'
            Prelude.<$> (x Prelude..?> "CreationDate")
            Prelude.<*> (x Prelude..?> "State")
            Prelude.<*> (x Prelude..?> "ExpirationDate")
            Prelude.<*> (x Prelude..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVaultLock

instance Prelude.NFData GetVaultLock

instance Prelude.ToHeaders GetVaultLock where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetVaultLock where
  toPath GetVaultLock' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/lock-policy"
      ]

instance Prelude.ToQuery GetVaultLock where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newGetVaultLockResponse' smart constructor.
data GetVaultLockResponse = GetVaultLockResponse'
  { -- | The UTC date and time at which the vault lock was put into the
    -- @InProgress@ state.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The state of the vault lock. @InProgress@ or @Locked@.
    state :: Prelude.Maybe Prelude.Text,
    -- | The UTC date and time at which the lock ID expires. This value can be
    -- @null@ if the vault lock is in a @Locked@ state.
    expirationDate :: Prelude.Maybe Prelude.Text,
    -- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
    -- character.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetVaultLockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'getVaultLockResponse_creationDate' - The UTC date and time at which the vault lock was put into the
-- @InProgress@ state.
--
-- 'state', 'getVaultLockResponse_state' - The state of the vault lock. @InProgress@ or @Locked@.
--
-- 'expirationDate', 'getVaultLockResponse_expirationDate' - The UTC date and time at which the lock ID expires. This value can be
-- @null@ if the vault lock is in a @Locked@ state.
--
-- 'policy', 'getVaultLockResponse_policy' - The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
--
-- 'httpStatus', 'getVaultLockResponse_httpStatus' - The response's http status code.
newGetVaultLockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVaultLockResponse
newGetVaultLockResponse pHttpStatus_ =
  GetVaultLockResponse'
    { creationDate =
        Prelude.Nothing,
      state = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The UTC date and time at which the vault lock was put into the
-- @InProgress@ state.
getVaultLockResponse_creationDate :: Lens.Lens' GetVaultLockResponse (Prelude.Maybe Prelude.Text)
getVaultLockResponse_creationDate = Lens.lens (\GetVaultLockResponse' {creationDate} -> creationDate) (\s@GetVaultLockResponse' {} a -> s {creationDate = a} :: GetVaultLockResponse)

-- | The state of the vault lock. @InProgress@ or @Locked@.
getVaultLockResponse_state :: Lens.Lens' GetVaultLockResponse (Prelude.Maybe Prelude.Text)
getVaultLockResponse_state = Lens.lens (\GetVaultLockResponse' {state} -> state) (\s@GetVaultLockResponse' {} a -> s {state = a} :: GetVaultLockResponse)

-- | The UTC date and time at which the lock ID expires. This value can be
-- @null@ if the vault lock is in a @Locked@ state.
getVaultLockResponse_expirationDate :: Lens.Lens' GetVaultLockResponse (Prelude.Maybe Prelude.Text)
getVaultLockResponse_expirationDate = Lens.lens (\GetVaultLockResponse' {expirationDate} -> expirationDate) (\s@GetVaultLockResponse' {} a -> s {expirationDate = a} :: GetVaultLockResponse)

-- | The vault lock policy as a JSON string, which uses \"\\\" as an escape
-- character.
getVaultLockResponse_policy :: Lens.Lens' GetVaultLockResponse (Prelude.Maybe Prelude.Text)
getVaultLockResponse_policy = Lens.lens (\GetVaultLockResponse' {policy} -> policy) (\s@GetVaultLockResponse' {} a -> s {policy = a} :: GetVaultLockResponse)

-- | The response's http status code.
getVaultLockResponse_httpStatus :: Lens.Lens' GetVaultLockResponse Prelude.Int
getVaultLockResponse_httpStatus = Lens.lens (\GetVaultLockResponse' {httpStatus} -> httpStatus) (\s@GetVaultLockResponse' {} a -> s {httpStatus = a} :: GetVaultLockResponse)

instance Prelude.NFData GetVaultLockResponse
