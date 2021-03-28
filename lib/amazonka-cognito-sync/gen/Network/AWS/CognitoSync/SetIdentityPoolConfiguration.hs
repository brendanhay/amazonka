{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the necessary configuration for push sync.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
    (
    -- * Creating a request
      SetIdentityPoolConfiguration (..)
    , mkSetIdentityPoolConfiguration
    -- ** Request lenses
    , sipcIdentityPoolId
    , sipcCognitoStreams
    , sipcPushSync

    -- * Destructuring the response
    , SetIdentityPoolConfigurationResponse (..)
    , mkSetIdentityPoolConfigurationResponse
    -- ** Response lenses
    , sipcrrsCognitoStreams
    , sipcrrsIdentityPoolId
    , sipcrrsPushSync
    , sipcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkSetIdentityPoolConfiguration' smart constructor.
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool to modify.
  , cognitoStreams :: Core.Maybe Types.CognitoStreams
    -- ^ Options to apply to this identity pool for Amazon Cognito streams.
  , pushSync :: Core.Maybe Types.PushSync
    -- ^ Options to apply to this identity pool for push synchronization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityPoolConfiguration' value with any optional fields omitted.
mkSetIdentityPoolConfiguration
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> SetIdentityPoolConfiguration
mkSetIdentityPoolConfiguration identityPoolId
  = SetIdentityPoolConfiguration'{identityPoolId,
                                  cognitoStreams = Core.Nothing, pushSync = Core.Nothing}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool to modify.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcIdentityPoolId :: Lens.Lens' SetIdentityPoolConfiguration Types.IdentityPoolId
sipcIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE sipcIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcCognitoStreams :: Lens.Lens' SetIdentityPoolConfiguration (Core.Maybe Types.CognitoStreams)
sipcCognitoStreams = Lens.field @"cognitoStreams"
{-# INLINEABLE sipcCognitoStreams #-}
{-# DEPRECATED cognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead"  #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcPushSync :: Lens.Lens' SetIdentityPoolConfiguration (Core.Maybe Types.PushSync)
sipcPushSync = Lens.field @"pushSync"
{-# INLINEABLE sipcPushSync #-}
{-# DEPRECATED pushSync "Use generic-lens or generic-optics with 'pushSync' instead"  #-}

instance Core.ToQuery SetIdentityPoolConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetIdentityPoolConfiguration where
        toHeaders SetIdentityPoolConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetIdentityPoolConfiguration where
        toJSON SetIdentityPoolConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("CognitoStreams" Core..=) Core.<$> cognitoStreams,
                  ("PushSync" Core..=) Core.<$> pushSync])

instance Core.AWSRequest SetIdentityPoolConfiguration where
        type Rs SetIdentityPoolConfiguration =
             SetIdentityPoolConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/configuration",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SetIdentityPoolConfigurationResponse' Core.<$>
                   (x Core..:? "CognitoStreams") Core.<*> x Core..:? "IdentityPoolId"
                     Core.<*> x Core..:? "PushSync"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the SetIdentityPoolConfiguration operation
--
-- /See:/ 'mkSetIdentityPoolConfigurationResponse' smart constructor.
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'
  { cognitoStreams :: Core.Maybe Types.CognitoStreams
    -- ^ Options to apply to this identity pool for Amazon Cognito streams.
  , identityPoolId :: Core.Maybe Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
  , pushSync :: Core.Maybe Types.PushSync
    -- ^ Options to apply to this identity pool for push synchronization.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityPoolConfigurationResponse' value with any optional fields omitted.
mkSetIdentityPoolConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetIdentityPoolConfigurationResponse
mkSetIdentityPoolConfigurationResponse responseStatus
  = SetIdentityPoolConfigurationResponse'{cognitoStreams =
                                            Core.Nothing,
                                          identityPoolId = Core.Nothing, pushSync = Core.Nothing,
                                          responseStatus}

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrrsCognitoStreams :: Lens.Lens' SetIdentityPoolConfigurationResponse (Core.Maybe Types.CognitoStreams)
sipcrrsCognitoStreams = Lens.field @"cognitoStreams"
{-# INLINEABLE sipcrrsCognitoStreams #-}
{-# DEPRECATED cognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrrsIdentityPoolId :: Lens.Lens' SetIdentityPoolConfigurationResponse (Core.Maybe Types.IdentityPoolId)
sipcrrsIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE sipcrrsIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrrsPushSync :: Lens.Lens' SetIdentityPoolConfigurationResponse (Core.Maybe Types.PushSync)
sipcrrsPushSync = Lens.field @"pushSync"
{-# INLINEABLE sipcrrsPushSync #-}
{-# DEPRECATED pushSync "Use generic-lens or generic-optics with 'pushSync' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrrsResponseStatus :: Lens.Lens' SetIdentityPoolConfigurationResponse Core.Int
sipcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sipcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
