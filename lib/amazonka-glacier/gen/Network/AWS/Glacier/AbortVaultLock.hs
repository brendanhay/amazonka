{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AbortVaultLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts the vault locking process if the vault lock is not in the @Locked@ state. If the vault lock is in the @Locked@ state when this operation is requested, the operation returns an @AccessDeniedException@ error. Aborting the vault locking process removes the vault lock policy from the specified vault.
--
-- A vault lock is put into the @InProgress@ state by calling 'InitiateVaultLock' . A vault lock is put into the @Locked@ state by calling 'CompleteVaultLock' . You can get the state of a vault lock by calling 'GetVaultLock' . For more information about the vault locking process, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> . For more information about vault lock policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies> .
-- This operation is idempotent. You can successfully invoke this operation multiple times, if the vault lock is in the @InProgress@ state or if there is no policy associated with the vault.
module Network.AWS.Glacier.AbortVaultLock
  ( -- * Creating a request
    AbortVaultLock (..),
    mkAbortVaultLock,

    -- ** Request lenses
    avlAccountId,
    avlVaultName,

    -- * Destructuring the response
    AbortVaultLockResponse (..),
    mkAbortVaultLockResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input values for @AbortVaultLock@ .
--
-- /See:/ 'mkAbortVaultLock' smart constructor.
data AbortVaultLock = AbortVaultLock'
  { accountId :: Lude.Text,
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

-- | Creates a value of 'AbortVaultLock' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
-- * 'vaultName' - The name of the vault.
mkAbortVaultLock ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  AbortVaultLock
mkAbortVaultLock pAccountId_ pVaultName_ =
  AbortVaultLock' {accountId = pAccountId_, vaultName = pVaultName_}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlAccountId :: Lens.Lens' AbortVaultLock Lude.Text
avlAccountId = Lens.lens (accountId :: AbortVaultLock -> Lude.Text) (\s a -> s {accountId = a} :: AbortVaultLock)
{-# DEPRECATED avlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlVaultName :: Lens.Lens' AbortVaultLock Lude.Text
avlVaultName = Lens.lens (vaultName :: AbortVaultLock -> Lude.Text) (\s a -> s {vaultName = a} :: AbortVaultLock)
{-# DEPRECATED avlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest AbortVaultLock where
  type Rs AbortVaultLock = AbortVaultLockResponse
  request = Req.delete glacierService
  response = Res.receiveNull AbortVaultLockResponse'

instance Lude.ToHeaders AbortVaultLock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AbortVaultLock where
  toPath AbortVaultLock' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/lock-policy"
      ]

instance Lude.ToQuery AbortVaultLock where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAbortVaultLockResponse' smart constructor.
data AbortVaultLockResponse = AbortVaultLockResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortVaultLockResponse' with the minimum fields required to make a request.
mkAbortVaultLockResponse ::
  AbortVaultLockResponse
mkAbortVaultLockResponse = AbortVaultLockResponse'
