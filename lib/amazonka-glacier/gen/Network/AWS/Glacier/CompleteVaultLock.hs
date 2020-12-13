{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.CompleteVaultLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation completes the vault locking process by transitioning the vault lock from the @InProgress@ state to the @Locked@ state, which causes the vault lock policy to become unchangeable. A vault lock is put into the @InProgress@ state by calling 'InitiateVaultLock' . You can obtain the state of the vault lock by calling 'GetVaultLock' . For more information about the vault locking process, <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> .
--
-- This operation is idempotent. This request is always successful if the vault lock is in the @Locked@ state and the provided lock ID matches the lock ID originally used to lock the vault.
-- If an invalid lock ID is passed in the request when the vault lock is in the @Locked@ state, the operation returns an @AccessDeniedException@ error. If an invalid lock ID is passed in the request when the vault lock is in the @InProgress@ state, the operation throws an @InvalidParameter@ error.
module Network.AWS.Glacier.CompleteVaultLock
  ( -- * Creating a request
    CompleteVaultLock (..),
    mkCompleteVaultLock,

    -- ** Request lenses
    cvlVaultName,
    cvlAccountId,
    cvlLockId,

    -- * Destructuring the response
    CompleteVaultLockResponse (..),
    mkCompleteVaultLockResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input values for @CompleteVaultLock@ .
--
-- /See:/ 'mkCompleteVaultLock' smart constructor.
data CompleteVaultLock = CompleteVaultLock'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text,
    -- | The @lockId@ value is the lock ID obtained from a 'InitiateVaultLock' request.
    lockId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteVaultLock' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'lockId' - The @lockId@ value is the lock ID obtained from a 'InitiateVaultLock' request.
mkCompleteVaultLock ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'lockId'
  Lude.Text ->
  CompleteVaultLock
mkCompleteVaultLock pVaultName_ pAccountId_ pLockId_ =
  CompleteVaultLock'
    { vaultName = pVaultName_,
      accountId = pAccountId_,
      lockId = pLockId_
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlVaultName :: Lens.Lens' CompleteVaultLock Lude.Text
cvlVaultName = Lens.lens (vaultName :: CompleteVaultLock -> Lude.Text) (\s a -> s {vaultName = a} :: CompleteVaultLock)
{-# DEPRECATED cvlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlAccountId :: Lens.Lens' CompleteVaultLock Lude.Text
cvlAccountId = Lens.lens (accountId :: CompleteVaultLock -> Lude.Text) (\s a -> s {accountId = a} :: CompleteVaultLock)
{-# DEPRECATED cvlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The @lockId@ value is the lock ID obtained from a 'InitiateVaultLock' request.
--
-- /Note:/ Consider using 'lockId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlLockId :: Lens.Lens' CompleteVaultLock Lude.Text
cvlLockId = Lens.lens (lockId :: CompleteVaultLock -> Lude.Text) (\s a -> s {lockId = a} :: CompleteVaultLock)
{-# DEPRECATED cvlLockId "Use generic-lens or generic-optics with 'lockId' instead." #-}

instance Lude.AWSRequest CompleteVaultLock where
  type Rs CompleteVaultLock = CompleteVaultLockResponse
  request = Req.postJSON glacierService
  response = Res.receiveNull CompleteVaultLockResponse'

instance Lude.ToHeaders CompleteVaultLock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CompleteVaultLock where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CompleteVaultLock where
  toPath CompleteVaultLock' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/lock-policy/",
        Lude.toBS lockId
      ]

instance Lude.ToQuery CompleteVaultLock where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCompleteVaultLockResponse' smart constructor.
data CompleteVaultLockResponse = CompleteVaultLockResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteVaultLockResponse' with the minimum fields required to make a request.
mkCompleteVaultLockResponse ::
  CompleteVaultLockResponse
mkCompleteVaultLockResponse = CompleteVaultLockResponse'
