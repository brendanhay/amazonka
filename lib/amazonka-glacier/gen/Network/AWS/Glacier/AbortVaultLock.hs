{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @AbortVaultLock@ .
--
-- /See:/ 'mkAbortVaultLock' smart constructor.
data AbortVaultLock = AbortVaultLock'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.AccountId,
    -- | The name of the vault.
    vaultName :: Types.VaultName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortVaultLock' value with any optional fields omitted.
mkAbortVaultLock ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'vaultName'
  Types.VaultName ->
  AbortVaultLock
mkAbortVaultLock accountId vaultName =
  AbortVaultLock' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlAccountId :: Lens.Lens' AbortVaultLock Types.AccountId
avlAccountId = Lens.field @"accountId"
{-# DEPRECATED avlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avlVaultName :: Lens.Lens' AbortVaultLock Types.VaultName
avlVaultName = Lens.field @"vaultName"
{-# DEPRECATED avlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.AWSRequest AbortVaultLock where
  type Rs AbortVaultLock = AbortVaultLockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/lock-policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull AbortVaultLockResponse'

-- | /See:/ 'mkAbortVaultLockResponse' smart constructor.
data AbortVaultLockResponse = AbortVaultLockResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortVaultLockResponse' value with any optional fields omitted.
mkAbortVaultLockResponse ::
  AbortVaultLockResponse
mkAbortVaultLockResponse = AbortVaultLockResponse'
