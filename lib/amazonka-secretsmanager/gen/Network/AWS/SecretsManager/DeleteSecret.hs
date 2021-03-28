{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DeleteSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entire secret and all of its versions. You can optionally include a recovery window during which you can restore the secret. If you don't specify a recovery window value, the operation defaults to 30 days. Secrets Manager attaches a @DeletionDate@ stamp to the secret that specifies the end of the recovery window. At the end of the recovery window, Secrets Manager deletes the secret permanently.
--
-- At any time before recovery window ends, you can use 'RestoreSecret' to remove the @DeletionDate@ and cancel the deletion of the secret.
-- You cannot access the encrypted secret information in any secret that is scheduled for deletion. If you need to access that information, you must cancel the deletion with 'RestoreSecret' and then retrieve the information.
-- __Minimum permissions__ 
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DeleteSecret
--
--
-- __Related operations__ 
--
--     * To create a secret, use 'CreateSecret' .
--
--
--     * To cancel deletion of a version of a secret before the recovery window has expired, use 'RestoreSecret' .
--
--
module Network.AWS.SecretsManager.DeleteSecret
    (
    -- * Creating a request
      DeleteSecret (..)
    , mkDeleteSecret
    -- ** Request lenses
    , dsSecretId
    , dsForceDeleteWithoutRecovery
    , dsRecoveryWindowInDays

    -- * Destructuring the response
    , DeleteSecretResponse (..)
    , mkDeleteSecretResponse
    -- ** Response lenses
    , dsrrsARN
    , dsrrsDeletionDate
    , dsrrsName
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkDeleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { secretId :: Types.SecretIdType
    -- ^ Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  , forceDeleteWithoutRecovery :: Core.Maybe Core.Bool
    -- ^ (Optional) Specifies that the secret is to be deleted without any recovery window. You can't use both this parameter and the @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so there can be a short delay before the operation completes. If you write code to delete and then immediately recreate a secret with the same name, ensure that your code includes appropriate back off and retry logic.
-- /Important:/ Use this parameter with caution. This parameter causes the operation to skip the normal waiting period before the permanent deletion that AWS would normally impose with the @RecoveryWindowInDays@ parameter. If you delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you have no opportunity to recover the secret. It is permanently lost.
  , recoveryWindowInDays :: Core.Maybe Core.Integer
    -- ^ (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API call.
--
-- This value can range from 7 to 30 days. The default value is 30.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSecret' value with any optional fields omitted.
mkDeleteSecret
    :: Types.SecretIdType -- ^ 'secretId'
    -> DeleteSecret
mkDeleteSecret secretId
  = DeleteSecret'{secretId,
                  forceDeleteWithoutRecovery = Core.Nothing,
                  recoveryWindowInDays = Core.Nothing}

-- | Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSecretId :: Lens.Lens' DeleteSecret Types.SecretIdType
dsSecretId = Lens.field @"secretId"
{-# INLINEABLE dsSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

-- | (Optional) Specifies that the secret is to be deleted without any recovery window. You can't use both this parameter and the @RecoveryWindowInDays@ parameter in the same API call.
--
-- An asynchronous background process performs the actual deletion, so there can be a short delay before the operation completes. If you write code to delete and then immediately recreate a secret with the same name, ensure that your code includes appropriate back off and retry logic.
-- /Important:/ Use this parameter with caution. This parameter causes the operation to skip the normal waiting period before the permanent deletion that AWS would normally impose with the @RecoveryWindowInDays@ parameter. If you delete a secret with the @ForceDeleteWithouRecovery@ parameter, then you have no opportunity to recover the secret. It is permanently lost.
--
-- /Note:/ Consider using 'forceDeleteWithoutRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsForceDeleteWithoutRecovery :: Lens.Lens' DeleteSecret (Core.Maybe Core.Bool)
dsForceDeleteWithoutRecovery = Lens.field @"forceDeleteWithoutRecovery"
{-# INLINEABLE dsForceDeleteWithoutRecovery #-}
{-# DEPRECATED forceDeleteWithoutRecovery "Use generic-lens or generic-optics with 'forceDeleteWithoutRecovery' instead"  #-}

-- | (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. You can't use both this parameter and the @ForceDeleteWithoutRecovery@ parameter in the same API call.
--
-- This value can range from 7 to 30 days. The default value is 30.
--
-- /Note:/ Consider using 'recoveryWindowInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRecoveryWindowInDays :: Lens.Lens' DeleteSecret (Core.Maybe Core.Integer)
dsRecoveryWindowInDays = Lens.field @"recoveryWindowInDays"
{-# INLINEABLE dsRecoveryWindowInDays #-}
{-# DEPRECATED recoveryWindowInDays "Use generic-lens or generic-optics with 'recoveryWindowInDays' instead"  #-}

instance Core.ToQuery DeleteSecret where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSecret where
        toHeaders DeleteSecret{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.DeleteSecret") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSecret where
        toJSON DeleteSecret{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SecretId" Core..= secretId),
                  ("ForceDeleteWithoutRecovery" Core..=) Core.<$>
                    forceDeleteWithoutRecovery,
                  ("RecoveryWindowInDays" Core..=) Core.<$> recoveryWindowInDays])

instance Core.AWSRequest DeleteSecret where
        type Rs DeleteSecret = DeleteSecretResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSecretResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "DeletionDate" Core.<*>
                     x Core..:? "Name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { arn :: Core.Maybe Types.ARN
    -- ^ The ARN of the secret that is now scheduled for deletion.
  , deletionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret that is now scheduled for deletion.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteSecretResponse' value with any optional fields omitted.
mkDeleteSecretResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSecretResponse
mkDeleteSecretResponse responseStatus
  = DeleteSecretResponse'{arn = Core.Nothing,
                          deletionDate = Core.Nothing, name = Core.Nothing, responseStatus}

-- | The ARN of the secret that is now scheduled for deletion.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsARN :: Lens.Lens' DeleteSecretResponse (Core.Maybe Types.ARN)
dsrrsARN = Lens.field @"arn"
{-# INLINEABLE dsrrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
--
-- /Note:/ Consider using 'deletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsDeletionDate :: Lens.Lens' DeleteSecretResponse (Core.Maybe Core.NominalDiffTime)
dsrrsDeletionDate = Lens.field @"deletionDate"
{-# INLINEABLE dsrrsDeletionDate #-}
{-# DEPRECATED deletionDate "Use generic-lens or generic-optics with 'deletionDate' instead"  #-}

-- | The friendly name of the secret that is now scheduled for deletion.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsName :: Lens.Lens' DeleteSecretResponse (Core.Maybe Types.Name)
dsrrsName = Lens.field @"name"
{-# INLINEABLE dsrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSecretResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
