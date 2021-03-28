{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeResourceServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource server.
module Network.AWS.CognitoIdentityProvider.DescribeResourceServer
    (
    -- * Creating a request
      DescribeResourceServer (..)
    , mkDescribeResourceServer
    -- ** Request lenses
    , drsfUserPoolId
    , drsfIdentifier

    -- * Destructuring the response
    , DescribeResourceServerResponse (..)
    , mkDescribeResourceServerResponse
    -- ** Response lenses
    , drsrrsResourceServer
    , drsrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool that hosts the resource server.
  , identifier :: Types.Identifier
    -- ^ The identifier for the resource server
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourceServer' value with any optional fields omitted.
mkDescribeResourceServer
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.Identifier -- ^ 'identifier'
    -> DescribeResourceServer
mkDescribeResourceServer userPoolId identifier
  = DescribeResourceServer'{userPoolId, identifier}

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsfUserPoolId :: Lens.Lens' DescribeResourceServer Types.UserPoolId
drsfUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE drsfUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The identifier for the resource server
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsfIdentifier :: Lens.Lens' DescribeResourceServer Types.Identifier
drsfIdentifier = Lens.field @"identifier"
{-# INLINEABLE drsfIdentifier #-}
{-# DEPRECATED identifier "Use generic-lens or generic-optics with 'identifier' instead"  #-}

instance Core.ToQuery DescribeResourceServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeResourceServer where
        toHeaders DescribeResourceServer{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DescribeResourceServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeResourceServer where
        toJSON DescribeResourceServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("Identifier" Core..= identifier)])

instance Core.AWSRequest DescribeResourceServer where
        type Rs DescribeResourceServer = DescribeResourceServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeResourceServerResponse' Core.<$>
                   (x Core..: "ResourceServer") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeResourceServerResponse' smart constructor.
data DescribeResourceServerResponse = DescribeResourceServerResponse'
  { resourceServer :: Types.ResourceServerType
    -- ^ The resource server.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourceServerResponse' value with any optional fields omitted.
mkDescribeResourceServerResponse
    :: Types.ResourceServerType -- ^ 'resourceServer'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeResourceServerResponse
mkDescribeResourceServerResponse resourceServer responseStatus
  = DescribeResourceServerResponse'{resourceServer, responseStatus}

-- | The resource server.
--
-- /Note:/ Consider using 'resourceServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrrsResourceServer :: Lens.Lens' DescribeResourceServerResponse Types.ResourceServerType
drsrrsResourceServer = Lens.field @"resourceServer"
{-# INLINEABLE drsrrsResourceServer #-}
{-# DEPRECATED resourceServer "Use generic-lens or generic-optics with 'resourceServer' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsrrsResponseStatus :: Lens.Lens' DescribeResourceServerResponse Core.Int
drsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
