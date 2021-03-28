{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.RestoreSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the scheduled deletion of a secret by removing the @DeletedDate@ time stamp. This makes the secret accessible to query once again.
--
-- __Minimum permissions__ 
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RestoreSecret
--
--
-- __Related operations__ 
--
--     * To delete a secret, use 'DeleteSecret' .
--
--
module Network.AWS.SecretsManager.RestoreSecret
    (
    -- * Creating a request
      RestoreSecret (..)
    , mkRestoreSecret
    -- ** Request lenses
    , rSecretId

    -- * Destructuring the response
    , RestoreSecretResponse (..)
    , mkRestoreSecretResponse
    -- ** Response lenses
    , rrsARN
    , rrsName
    , rrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkRestoreSecret' smart constructor.
newtype RestoreSecret = RestoreSecret'
  { secretId :: Types.SecretId
    -- ^ Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreSecret' value with any optional fields omitted.
mkRestoreSecret
    :: Types.SecretId -- ^ 'secretId'
    -> RestoreSecret
mkRestoreSecret secretId = RestoreSecret'{secretId}

-- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecretId :: Lens.Lens' RestoreSecret Types.SecretId
rSecretId = Lens.field @"secretId"
{-# INLINEABLE rSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

instance Core.ToQuery RestoreSecret where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RestoreSecret where
        toHeaders RestoreSecret{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.RestoreSecret")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RestoreSecret where
        toJSON RestoreSecret{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest RestoreSecret where
        type Rs RestoreSecret = RestoreSecretResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RestoreSecretResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "Name" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreSecretResponse' smart constructor.
data RestoreSecretResponse = RestoreSecretResponse'
  { arn :: Core.Maybe Types.SecretARNType
    -- ^ The ARN of the secret that was restored.
  , name :: Core.Maybe Types.Name
    -- ^ The friendly name of the secret that was restored.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreSecretResponse' value with any optional fields omitted.
mkRestoreSecretResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreSecretResponse
mkRestoreSecretResponse responseStatus
  = RestoreSecretResponse'{arn = Core.Nothing, name = Core.Nothing,
                           responseStatus}

-- | The ARN of the secret that was restored.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsARN :: Lens.Lens' RestoreSecretResponse (Core.Maybe Types.SecretARNType)
rrsARN = Lens.field @"arn"
{-# INLINEABLE rrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The friendly name of the secret that was restored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsName :: Lens.Lens' RestoreSecretResponse (Core.Maybe Types.Name)
rrsName = Lens.field @"name"
{-# INLINEABLE rrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreSecretResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
