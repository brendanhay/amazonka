{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateVaultLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the vault locking process by doing the following:
--
--
--     * Installing a vault lock policy on the specified vault.
--
--
--     * Setting the lock state of vault lock to @InProgress@ .
--
--
--     * Returning a lock ID, which is used to complete the vault locking process.
--
--
-- You can set one vault lock policy for each vault and this policy can be up to 20 KB in size. For more information about vault lock policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies> .
-- You must complete the vault locking process within 24 hours after the vault lock enters the @InProgress@ state. After the 24 hour window ends, the lock ID expires, the vault automatically exits the @InProgress@ state, and the vault lock policy is removed from the vault. You call 'CompleteVaultLock' to complete the vault locking process by setting the state of the vault lock to @Locked@ .
-- After a vault lock is in the @Locked@ state, you cannot initiate a new vault lock for the vault.
-- You can abort the vault locking process by calling 'AbortVaultLock' . You can get the state of the vault lock by calling 'GetVaultLock' . For more information about the vault locking process, <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> .
-- If this operation is called when the vault lock is in the @InProgress@ state, the operation returns an @AccessDeniedException@ error. When the vault lock is in the @InProgress@ state you must call 'AbortVaultLock' before you can initiate a new vault lock policy.
module Network.AWS.Glacier.InitiateVaultLock
  ( -- * Creating a request
    InitiateVaultLock (..),
    mkInitiateVaultLock,

    -- ** Request lenses
    ivlPolicy,
    ivlAccountId,
    ivlVaultName,

    -- * Destructuring the response
    InitiateVaultLockResponse (..),
    mkInitiateVaultLockResponse,

    -- ** Response lenses
    ivlrsLockId,
    ivlrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input values for @InitiateVaultLock@ .
--
-- /See:/ 'mkInitiateVaultLock' smart constructor.
data InitiateVaultLock = InitiateVaultLock'
  { policy ::
      Lude.Maybe VaultLockPolicy,
    accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateVaultLock' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'policy' - The vault lock policy as a JSON string, which uses "\" as an escape character.
-- * 'vaultName' - The name of the vault.
mkInitiateVaultLock ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  InitiateVaultLock
mkInitiateVaultLock pAccountId_ pVaultName_ =
  InitiateVaultLock'
    { policy = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The vault lock policy as a JSON string, which uses "\" as an escape character.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlPolicy :: Lens.Lens' InitiateVaultLock (Lude.Maybe VaultLockPolicy)
ivlPolicy = Lens.lens (policy :: InitiateVaultLock -> Lude.Maybe VaultLockPolicy) (\s a -> s {policy = a} :: InitiateVaultLock)
{-# DEPRECATED ivlPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlAccountId :: Lens.Lens' InitiateVaultLock Lude.Text
ivlAccountId = Lens.lens (accountId :: InitiateVaultLock -> Lude.Text) (\s a -> s {accountId = a} :: InitiateVaultLock)
{-# DEPRECATED ivlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlVaultName :: Lens.Lens' InitiateVaultLock Lude.Text
ivlVaultName = Lens.lens (vaultName :: InitiateVaultLock -> Lude.Text) (\s a -> s {vaultName = a} :: InitiateVaultLock)
{-# DEPRECATED ivlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest InitiateVaultLock where
  type Rs InitiateVaultLock = InitiateVaultLockResponse
  request = Req.postJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          InitiateVaultLockResponse'
            Lude.<$> (h Lude..#? "x-amz-lock-id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateVaultLock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON InitiateVaultLock where
  toJSON InitiateVaultLock' {..} =
    Lude.object (Lude.catMaybes [("policy" Lude..=) Lude.<$> policy])

instance Lude.ToPath InitiateVaultLock where
  toPath InitiateVaultLock' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/lock-policy"
      ]

instance Lude.ToQuery InitiateVaultLock where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateVaultLockResponse' smart constructor.
data InitiateVaultLockResponse = InitiateVaultLockResponse'
  { lockId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateVaultLockResponse' with the minimum fields required to make a request.
--
-- * 'lockId' - The lock ID, which is used to complete the vault locking process.
-- * 'responseStatus' - The response status code.
mkInitiateVaultLockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateVaultLockResponse
mkInitiateVaultLockResponse pResponseStatus_ =
  InitiateVaultLockResponse'
    { lockId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The lock ID, which is used to complete the vault locking process.
--
-- /Note:/ Consider using 'lockId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlrsLockId :: Lens.Lens' InitiateVaultLockResponse (Lude.Maybe Lude.Text)
ivlrsLockId = Lens.lens (lockId :: InitiateVaultLockResponse -> Lude.Maybe Lude.Text) (\s a -> s {lockId = a} :: InitiateVaultLockResponse)
{-# DEPRECATED ivlrsLockId "Use generic-lens or generic-optics with 'lockId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlrsResponseStatus :: Lens.Lens' InitiateVaultLockResponse Lude.Int
ivlrsResponseStatus = Lens.lens (responseStatus :: InitiateVaultLockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateVaultLockResponse)
{-# DEPRECATED ivlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
