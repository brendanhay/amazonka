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
    gvlAccountId,
    gvlVaultName,

    -- * Destructuring the response
    GetVaultLockResponse (..),
    mkGetVaultLockResponse,

    -- ** Response lenses
    gvlrrsCreationDate,
    gvlrrsExpirationDate,
    gvlrrsPolicy,
    gvlrrsState,
    gvlrrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @GetVaultLock@ .
--
-- /See:/ 'mkGetVaultLock' smart constructor.
data GetVaultLock = GetVaultLock'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String,
    -- | The name of the vault.
    vaultName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVaultLock' value with any optional fields omitted.
mkGetVaultLock ::
  -- | 'accountId'
  Types.String ->
  -- | 'vaultName'
  Types.String ->
  GetVaultLock
mkGetVaultLock accountId vaultName =
  GetVaultLock' {accountId, vaultName}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlAccountId :: Lens.Lens' GetVaultLock Types.String
gvlAccountId = Lens.field @"accountId"
{-# DEPRECATED gvlAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlVaultName :: Lens.Lens' GetVaultLock Types.String
gvlVaultName = Lens.field @"vaultName"
{-# DEPRECATED gvlVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Core.AWSRequest GetVaultLock where
  type Rs GetVaultLock = GetVaultLockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVaultLockResponse'
            Core.<$> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "ExpirationDate")
            Core.<*> (x Core..:? "Policy")
            Core.<*> (x Core..:? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkGetVaultLockResponse' smart constructor.
data GetVaultLockResponse = GetVaultLockResponse'
  { -- | The UTC date and time at which the vault lock was put into the @InProgress@ state.
    creationDate :: Core.Maybe Types.String,
    -- | The UTC date and time at which the lock ID expires. This value can be @null@ if the vault lock is in a @Locked@ state.
    expirationDate :: Core.Maybe Types.String,
    -- | The vault lock policy as a JSON string, which uses "\" as an escape character.
    policy :: Core.Maybe Types.String,
    -- | The state of the vault lock. @InProgress@ or @Locked@ .
    state :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVaultLockResponse' value with any optional fields omitted.
mkGetVaultLockResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetVaultLockResponse
mkGetVaultLockResponse responseStatus =
  GetVaultLockResponse'
    { creationDate = Core.Nothing,
      expirationDate = Core.Nothing,
      policy = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The UTC date and time at which the vault lock was put into the @InProgress@ state.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsCreationDate :: Lens.Lens' GetVaultLockResponse (Core.Maybe Types.String)
gvlrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gvlrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The UTC date and time at which the lock ID expires. This value can be @null@ if the vault lock is in a @Locked@ state.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsExpirationDate :: Lens.Lens' GetVaultLockResponse (Core.Maybe Types.String)
gvlrrsExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED gvlrrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The vault lock policy as a JSON string, which uses "\" as an escape character.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsPolicy :: Lens.Lens' GetVaultLockResponse (Core.Maybe Types.String)
gvlrrsPolicy = Lens.field @"policy"
{-# DEPRECATED gvlrrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The state of the vault lock. @InProgress@ or @Locked@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsState :: Lens.Lens' GetVaultLockResponse (Core.Maybe Types.String)
gvlrrsState = Lens.field @"state"
{-# DEPRECATED gvlrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsResponseStatus :: Lens.Lens' GetVaultLockResponse Core.Int
gvlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
