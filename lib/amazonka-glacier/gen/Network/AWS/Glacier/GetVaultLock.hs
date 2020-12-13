{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the following attributes from the @lock-policy@ subresource set on the specified vault:
--
--
--     * The vault lock policy set on the vault.
--
--
--     * The state of the vault lock, which is either @InProgess@ or @Locked@ .
--
--
--     * When the lock ID expires. The lock ID is used to complete the vault locking process.
--
--
--     * When the vault lock was initiated and put into the @InProgress@ state.
--
--
-- A vault lock is put into the @InProgress@ state by calling 'InitiateVaultLock' . A vault lock is put into the @Locked@ state by calling 'CompleteVaultLock' . You can abort the vault locking process by calling 'AbortVaultLock' . For more information about the vault locking process, <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock.html Amazon Glacier Vault Lock> .
-- If there is no vault lock policy set on the vault, the operation returns a @404 Not found@ error. For more information about vault lock policies, <https://docs.aws.amazon.com/amazonglacier/latest/dev/vault-lock-policy.html Amazon Glacier Access Control with Vault Lock Policies> .
module Network.AWS.Glacier.GetVaultLock
  ( -- * Creating a request
    GetVaultLock (..),
    mkGetVaultLock,

    -- ** Request lenses
    gvlVaultName,
    gvlAccountId,

    -- * Destructuring the response
    GetVaultLockResponse (..),
    mkGetVaultLockResponse,

    -- ** Response lenses
    gvlrsState,
    gvlrsExpirationDate,
    gvlrsCreationDate,
    gvlrsPolicy,
    gvlrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input values for @GetVaultLock@ .
--
-- /See:/ 'mkGetVaultLock' smart constructor.
data GetVaultLock = GetVaultLock'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVaultLock' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkGetVaultLock ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  GetVaultLock
mkGetVaultLock pVaultName_ pAccountId_ =
  GetVaultLock' {vaultName = pVaultName_, accountId = pAccountId_}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlVaultName :: Lens.Lens' GetVaultLock Lude.Text
gvlVaultName = Lens.lens (vaultName :: GetVaultLock -> Lude.Text) (\s a -> s {vaultName = a} :: GetVaultLock)
{-# DEPRECATED gvlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlAccountId :: Lens.Lens' GetVaultLock Lude.Text
gvlAccountId = Lens.lens (accountId :: GetVaultLock -> Lude.Text) (\s a -> s {accountId = a} :: GetVaultLock)
{-# DEPRECATED gvlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest GetVaultLock where
  type Rs GetVaultLock = GetVaultLockResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetVaultLockResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ExpirationDate")
            Lude.<*> (x Lude..?> "CreationDate")
            Lude.<*> (x Lude..?> "Policy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetVaultLock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetVaultLock where
  toPath GetVaultLock' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/lock-policy"
      ]

instance Lude.ToQuery GetVaultLock where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkGetVaultLockResponse' smart constructor.
data GetVaultLockResponse = GetVaultLockResponse'
  { -- | The state of the vault lock. @InProgress@ or @Locked@ .
    state :: Lude.Maybe Lude.Text,
    -- | The UTC date and time at which the lock ID expires. This value can be @null@ if the vault lock is in a @Locked@ state.
    expirationDate :: Lude.Maybe Lude.Text,
    -- | The UTC date and time at which the vault lock was put into the @InProgress@ state.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The vault lock policy as a JSON string, which uses "\" as an escape character.
    policy :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetVaultLockResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the vault lock. @InProgress@ or @Locked@ .
-- * 'expirationDate' - The UTC date and time at which the lock ID expires. This value can be @null@ if the vault lock is in a @Locked@ state.
-- * 'creationDate' - The UTC date and time at which the vault lock was put into the @InProgress@ state.
-- * 'policy' - The vault lock policy as a JSON string, which uses "\" as an escape character.
-- * 'responseStatus' - The response status code.
mkGetVaultLockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetVaultLockResponse
mkGetVaultLockResponse pResponseStatus_ =
  GetVaultLockResponse'
    { state = Lude.Nothing,
      expirationDate = Lude.Nothing,
      creationDate = Lude.Nothing,
      policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state of the vault lock. @InProgress@ or @Locked@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsState :: Lens.Lens' GetVaultLockResponse (Lude.Maybe Lude.Text)
gvlrsState = Lens.lens (state :: GetVaultLockResponse -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: GetVaultLockResponse)
{-# DEPRECATED gvlrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The UTC date and time at which the lock ID expires. This value can be @null@ if the vault lock is in a @Locked@ state.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsExpirationDate :: Lens.Lens' GetVaultLockResponse (Lude.Maybe Lude.Text)
gvlrsExpirationDate = Lens.lens (expirationDate :: GetVaultLockResponse -> Lude.Maybe Lude.Text) (\s a -> s {expirationDate = a} :: GetVaultLockResponse)
{-# DEPRECATED gvlrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The UTC date and time at which the vault lock was put into the @InProgress@ state.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsCreationDate :: Lens.Lens' GetVaultLockResponse (Lude.Maybe Lude.Text)
gvlrsCreationDate = Lens.lens (creationDate :: GetVaultLockResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: GetVaultLockResponse)
{-# DEPRECATED gvlrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The vault lock policy as a JSON string, which uses "\" as an escape character.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsPolicy :: Lens.Lens' GetVaultLockResponse (Lude.Maybe Lude.Text)
gvlrsPolicy = Lens.lens (policy :: GetVaultLockResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetVaultLockResponse)
{-# DEPRECATED gvlrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrsResponseStatus :: Lens.Lens' GetVaultLockResponse Lude.Int
gvlrsResponseStatus = Lens.lens (responseStatus :: GetVaultLockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetVaultLockResponse)
{-# DEPRECATED gvlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
