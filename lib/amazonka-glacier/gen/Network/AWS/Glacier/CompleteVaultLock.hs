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
    cvlAccountId,
    cvlVaultName,
    cvlLockId,

    -- * Destructuring the response
    CompleteVaultLockResponse (..),
    mkCompleteVaultLockResponse,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @CompleteVaultLock@ .
--
-- /See:/ 'mkCompleteVaultLock' smart constructor.
data CompleteVaultLock = CompleteVaultLock'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String,
    -- | The @lockId@ value is the lock ID obtained from a 'InitiateVaultLock' request.
    lockId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteVaultLock' value with any optional fields omitted.
mkCompleteVaultLock ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  -- | 'lockId'
  Types.String ->
  CompleteVaultLock
mkCompleteVaultLock accountId vaultName lockId =
  CompleteVaultLock' {accountId, vaultName, lockId}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlAccountId :: Lens.Lens' CompleteVaultLock Types.String
cvlAccountId = Lens.field @"accountId"
{-# DEPRECATED cvlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlVaultName :: Lens.Lens' CompleteVaultLock Types.String
cvlVaultName = Lens.field @"vaultName"
{-# DEPRECATED cvlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @lockId@ value is the lock ID obtained from a 'InitiateVaultLock' request.
--
-- /Note:/ Consider using 'lockId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvlLockId :: Lens.Lens' CompleteVaultLock Types.String
cvlLockId = Lens.field @"lockId"
{-# DEPRECATED cvlLockId "Use generic-lens or generic-optics with 'lockId' instead." #-}

instance Core.FromJSON CompleteVaultLock where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CompleteVaultLock where
  type Rs CompleteVaultLock = CompleteVaultLockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId) Core.<> ("/vaults/")
                Core.<> (Core.toText vaultName)
                Core.<> ("/lock-policy/")
                Core.<> (Core.toText lockId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CompleteVaultLockResponse'

-- | /See:/ 'mkCompleteVaultLockResponse' smart constructor.
data CompleteVaultLockResponse = CompleteVaultLockResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CompleteVaultLockResponse' value with any optional fields omitted.
mkCompleteVaultLockResponse ::
  CompleteVaultLockResponse
mkCompleteVaultLockResponse = CompleteVaultLockResponse'
