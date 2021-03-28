{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      InitiateVaultLock (..)
    , mkInitiateVaultLock
    -- ** Request lenses
    , ivlAccountId
    , ivlVaultName
    , ivlPolicy

    -- * Destructuring the response
    , InitiateVaultLockResponse (..)
    , mkInitiateVaultLockResponse
    -- ** Response lenses
    , ivlrrsLockId
    , ivlrrsResponseStatus
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input values for @InitiateVaultLock@ .
--
-- /See:/ 'mkInitiateVaultLock' smart constructor.
data InitiateVaultLock = InitiateVaultLock'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
  , vaultName :: Core.Text
    -- ^ The name of the vault.
  , policy :: Core.Maybe Types.VaultLockPolicy
    -- ^ The vault lock policy as a JSON string, which uses "\" as an escape character.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateVaultLock' value with any optional fields omitted.
mkInitiateVaultLock
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'vaultName'
    -> InitiateVaultLock
mkInitiateVaultLock accountId vaultName
  = InitiateVaultLock'{accountId, vaultName, policy = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlAccountId :: Lens.Lens' InitiateVaultLock Core.Text
ivlAccountId = Lens.field @"accountId"
{-# INLINEABLE ivlAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlVaultName :: Lens.Lens' InitiateVaultLock Core.Text
ivlVaultName = Lens.field @"vaultName"
{-# INLINEABLE ivlVaultName #-}
{-# DEPRECATED vaultName "Use generic-lens or generic-optics with 'vaultName' instead"  #-}

-- | The vault lock policy as a JSON string, which uses "\" as an escape character.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlPolicy :: Lens.Lens' InitiateVaultLock (Core.Maybe Types.VaultLockPolicy)
ivlPolicy = Lens.field @"policy"
{-# INLINEABLE ivlPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.ToQuery InitiateVaultLock where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InitiateVaultLock where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON InitiateVaultLock where
        toJSON InitiateVaultLock{..}
          = Core.object (Core.catMaybes [("policy" Core..=) Core.<$> policy])

instance Core.AWSRequest InitiateVaultLock where
        type Rs InitiateVaultLock = InitiateVaultLockResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<> "/vaults/" Core.<>
                             Core.toText vaultName
                             Core.<> "/lock-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 InitiateVaultLockResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-lock-id" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateVaultLockResponse' smart constructor.
data InitiateVaultLockResponse = InitiateVaultLockResponse'
  { lockId :: Core.Maybe Core.Text
    -- ^ The lock ID, which is used to complete the vault locking process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InitiateVaultLockResponse' value with any optional fields omitted.
mkInitiateVaultLockResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InitiateVaultLockResponse
mkInitiateVaultLockResponse responseStatus
  = InitiateVaultLockResponse'{lockId = Core.Nothing, responseStatus}

-- | The lock ID, which is used to complete the vault locking process.
--
-- /Note:/ Consider using 'lockId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlrrsLockId :: Lens.Lens' InitiateVaultLockResponse (Core.Maybe Core.Text)
ivlrrsLockId = Lens.field @"lockId"
{-# INLINEABLE ivlrrsLockId #-}
{-# DEPRECATED lockId "Use generic-lens or generic-optics with 'lockId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivlrrsResponseStatus :: Lens.Lens' InitiateVaultLockResponse Core.Int
ivlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ivlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
