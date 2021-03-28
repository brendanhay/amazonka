{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an authorizer.
module Network.AWS.IoT.CreateAuthorizer
    (
    -- * Creating a request
      CreateAuthorizer (..)
    , mkCreateAuthorizer
    -- ** Request lenses
    , caAuthorizerName
    , caAuthorizerFunctionArn
    , caSigningDisabled
    , caStatus
    , caTags
    , caTokenKeyName
    , caTokenSigningPublicKeys

    -- * Destructuring the response
    , CreateAuthorizerResponse (..)
    , mkCreateAuthorizerResponse
    -- ** Response lenses
    , carrsAuthorizerArn
    , carrsAuthorizerName
    , carrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { authorizerName :: Types.AuthorizerName
    -- ^ The authorizer name.
  , authorizerFunctionArn :: Types.AuthorizerFunctionArn
    -- ^ The ARN of the authorizer's Lambda function.
  , signingDisabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether AWS IoT validates the token signature in an authorization request.
  , status :: Core.Maybe Types.AuthorizerStatus
    -- ^ The status of the create authorizer request.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the custom authorizer.
  , tokenKeyName :: Core.Maybe Types.TokenKeyName
    -- ^ The name of the token key used to extract the token from the HTTP headers.
  , tokenSigningPublicKeys :: Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue)
    -- ^ The public keys used to verify the digital signature returned by your custom authentication service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAuthorizer' value with any optional fields omitted.
mkCreateAuthorizer
    :: Types.AuthorizerName -- ^ 'authorizerName'
    -> Types.AuthorizerFunctionArn -- ^ 'authorizerFunctionArn'
    -> CreateAuthorizer
mkCreateAuthorizer authorizerName authorizerFunctionArn
  = CreateAuthorizer'{authorizerName, authorizerFunctionArn,
                      signingDisabled = Core.Nothing, status = Core.Nothing,
                      tags = Core.Nothing, tokenKeyName = Core.Nothing,
                      tokenSigningPublicKeys = Core.Nothing}

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerName :: Lens.Lens' CreateAuthorizer Types.AuthorizerName
caAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE caAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

-- | The ARN of the authorizer's Lambda function.
--
-- /Note:/ Consider using 'authorizerFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerFunctionArn :: Lens.Lens' CreateAuthorizer Types.AuthorizerFunctionArn
caAuthorizerFunctionArn = Lens.field @"authorizerFunctionArn"
{-# INLINEABLE caAuthorizerFunctionArn #-}
{-# DEPRECATED authorizerFunctionArn "Use generic-lens or generic-optics with 'authorizerFunctionArn' instead"  #-}

-- | Specifies whether AWS IoT validates the token signature in an authorization request.
--
-- /Note:/ Consider using 'signingDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSigningDisabled :: Lens.Lens' CreateAuthorizer (Core.Maybe Core.Bool)
caSigningDisabled = Lens.field @"signingDisabled"
{-# INLINEABLE caSigningDisabled #-}
{-# DEPRECATED signingDisabled "Use generic-lens or generic-optics with 'signingDisabled' instead"  #-}

-- | The status of the create authorizer request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caStatus :: Lens.Lens' CreateAuthorizer (Core.Maybe Types.AuthorizerStatus)
caStatus = Lens.field @"status"
{-# INLINEABLE caStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Metadata which can be used to manage the custom authorizer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAuthorizer (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The name of the token key used to extract the token from the HTTP headers.
--
-- /Note:/ Consider using 'tokenKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTokenKeyName :: Lens.Lens' CreateAuthorizer (Core.Maybe Types.TokenKeyName)
caTokenKeyName = Lens.field @"tokenKeyName"
{-# INLINEABLE caTokenKeyName #-}
{-# DEPRECATED tokenKeyName "Use generic-lens or generic-optics with 'tokenKeyName' instead"  #-}

-- | The public keys used to verify the digital signature returned by your custom authentication service.
--
-- /Note:/ Consider using 'tokenSigningPublicKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTokenSigningPublicKeys :: Lens.Lens' CreateAuthorizer (Core.Maybe (Core.HashMap Types.KeyName Types.KeyValue))
caTokenSigningPublicKeys = Lens.field @"tokenSigningPublicKeys"
{-# INLINEABLE caTokenSigningPublicKeys #-}
{-# DEPRECATED tokenSigningPublicKeys "Use generic-lens or generic-optics with 'tokenSigningPublicKeys' instead"  #-}

instance Core.ToQuery CreateAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAuthorizer where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateAuthorizer where
        toJSON CreateAuthorizer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("authorizerFunctionArn" Core..= authorizerFunctionArn),
                  ("signingDisabled" Core..=) Core.<$> signingDisabled,
                  ("status" Core..=) Core.<$> status, ("tags" Core..=) Core.<$> tags,
                  ("tokenKeyName" Core..=) Core.<$> tokenKeyName,
                  ("tokenSigningPublicKeys" Core..=) Core.<$>
                    tokenSigningPublicKeys])

instance Core.AWSRequest CreateAuthorizer where
        type Rs CreateAuthorizer = CreateAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/authorizer/" Core.<> Core.toText authorizerName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAuthorizerResponse' Core.<$>
                   (x Core..:? "authorizerArn") Core.<*> x Core..:? "authorizerName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
  { authorizerArn :: Core.Maybe Types.AuthorizerArn
    -- ^ The authorizer ARN.
  , authorizerName :: Core.Maybe Types.AuthorizerName
    -- ^ The authorizer's name.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAuthorizerResponse' value with any optional fields omitted.
mkCreateAuthorizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAuthorizerResponse
mkCreateAuthorizerResponse responseStatus
  = CreateAuthorizerResponse'{authorizerArn = Core.Nothing,
                              authorizerName = Core.Nothing, responseStatus}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAuthorizerArn :: Lens.Lens' CreateAuthorizerResponse (Core.Maybe Types.AuthorizerArn)
carrsAuthorizerArn = Lens.field @"authorizerArn"
{-# INLINEABLE carrsAuthorizerArn #-}
{-# DEPRECATED authorizerArn "Use generic-lens or generic-optics with 'authorizerArn' instead"  #-}

-- | The authorizer's name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAuthorizerName :: Lens.Lens' CreateAuthorizerResponse (Core.Maybe Types.AuthorizerName)
carrsAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE carrsAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAuthorizerResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
