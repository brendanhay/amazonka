{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an authorizer.
module Network.AWS.IoT.DeleteAuthorizer
    (
    -- * Creating a request
      DeleteAuthorizer (..)
    , mkDeleteAuthorizer
    -- ** Request lenses
    , dAuthorizerName

    -- * Destructuring the response
    , DeleteAuthorizerResponse (..)
    , mkDeleteAuthorizerResponse
    -- ** Response lenses
    , darfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAuthorizer' smart constructor.
newtype DeleteAuthorizer = DeleteAuthorizer'
  { authorizerName :: Types.AuthorizerName
    -- ^ The name of the authorizer to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuthorizer' value with any optional fields omitted.
mkDeleteAuthorizer
    :: Types.AuthorizerName -- ^ 'authorizerName'
    -> DeleteAuthorizer
mkDeleteAuthorizer authorizerName
  = DeleteAuthorizer'{authorizerName}

-- | The name of the authorizer to delete.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAuthorizerName :: Lens.Lens' DeleteAuthorizer Types.AuthorizerName
dAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE dAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

instance Core.ToQuery DeleteAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAuthorizer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteAuthorizer where
        type Rs DeleteAuthorizer = DeleteAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/authorizer/" Core.<> Core.toText authorizerName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteAuthorizerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAuthorizerResponse' smart constructor.
newtype DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuthorizerResponse' value with any optional fields omitted.
mkDeleteAuthorizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAuthorizerResponse
mkDeleteAuthorizerResponse responseStatus
  = DeleteAuthorizerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsResponseStatus :: Lens.Lens' DeleteAuthorizerResponse Core.Int
darfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
