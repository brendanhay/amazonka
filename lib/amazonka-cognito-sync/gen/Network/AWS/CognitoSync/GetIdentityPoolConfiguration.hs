{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings of an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
  ( -- * Creating a request
    GetIdentityPoolConfiguration (..),
    mkGetIdentityPoolConfiguration,

    -- ** Request lenses
    gipcIdentityPoolId,

    -- * Destructuring the response
    GetIdentityPoolConfigurationResponse (..),
    mkGetIdentityPoolConfigurationResponse,

    -- ** Response lenses
    gipcrrsCognitoStreams,
    gipcrrsIdentityPoolId,
    gipcrrsPushSync,
    gipcrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkGetIdentityPoolConfiguration' smart constructor.
newtype GetIdentityPoolConfiguration = GetIdentityPoolConfiguration'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolConfiguration' value with any optional fields omitted.
mkGetIdentityPoolConfiguration ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  GetIdentityPoolConfiguration
mkGetIdentityPoolConfiguration identityPoolId =
  GetIdentityPoolConfiguration' {identityPoolId}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcIdentityPoolId :: Lens.Lens' GetIdentityPoolConfiguration Types.IdentityPoolId
gipcIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED gipcIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.AWSRequest GetIdentityPoolConfiguration where
  type
    Rs GetIdentityPoolConfiguration =
      GetIdentityPoolConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/configuration")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityPoolConfigurationResponse'
            Core.<$> (x Core..:? "CognitoStreams")
            Core.<*> (x Core..:? "IdentityPoolId")
            Core.<*> (x Core..:? "PushSync")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkGetIdentityPoolConfigurationResponse' smart constructor.
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
  { -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Core.Maybe Types.CognitoStreams,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Core.Maybe Types.PushSync,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolConfigurationResponse' value with any optional fields omitted.
mkGetIdentityPoolConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIdentityPoolConfigurationResponse
mkGetIdentityPoolConfigurationResponse responseStatus =
  GetIdentityPoolConfigurationResponse'
    { cognitoStreams =
        Core.Nothing,
      identityPoolId = Core.Nothing,
      pushSync = Core.Nothing,
      responseStatus
    }

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrrsCognitoStreams :: Lens.Lens' GetIdentityPoolConfigurationResponse (Core.Maybe Types.CognitoStreams)
gipcrrsCognitoStreams = Lens.field @"cognitoStreams"
{-# DEPRECATED gipcrrsCognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrrsIdentityPoolId :: Lens.Lens' GetIdentityPoolConfigurationResponse (Core.Maybe Types.IdentityPoolId)
gipcrrsIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED gipcrrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrrsPushSync :: Lens.Lens' GetIdentityPoolConfigurationResponse (Core.Maybe Types.PushSync)
gipcrrsPushSync = Lens.field @"pushSync"
{-# DEPRECATED gipcrrsPushSync "Use generic-lens or generic-optics with 'pushSync' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrrsResponseStatus :: Lens.Lens' GetIdentityPoolConfigurationResponse Core.Int
gipcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gipcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
