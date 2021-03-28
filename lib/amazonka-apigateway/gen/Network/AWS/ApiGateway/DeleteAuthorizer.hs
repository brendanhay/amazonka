{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing 'Authorizer' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/delete-authorizer.html AWS CLI> 
module Network.AWS.ApiGateway.DeleteAuthorizer
    (
    -- * Creating a request
      DeleteAuthorizer (..)
    , mkDeleteAuthorizer
    -- ** Request lenses
    , daRestApiId
    , daAuthorizerId

    -- * Destructuring the response
    , DeleteAuthorizerResponse (..)
    , mkDeleteAuthorizerResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing 'Authorizer' resource.
--
-- /See:/ 'mkDeleteAuthorizer' smart constructor.
data DeleteAuthorizer = DeleteAuthorizer'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , authorizerId :: Core.Text
    -- ^ [Required] The identifier of the 'Authorizer' resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuthorizer' value with any optional fields omitted.
mkDeleteAuthorizer
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'authorizerId'
    -> DeleteAuthorizer
mkDeleteAuthorizer restApiId authorizerId
  = DeleteAuthorizer'{restApiId, authorizerId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daRestApiId :: Lens.Lens' DeleteAuthorizer Core.Text
daRestApiId = Lens.field @"restApiId"
{-# INLINEABLE daRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'Authorizer' resource.
--
-- /Note:/ Consider using 'authorizerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthorizerId :: Lens.Lens' DeleteAuthorizer Core.Text
daAuthorizerId = Lens.field @"authorizerId"
{-# INLINEABLE daAuthorizerId #-}
{-# DEPRECATED authorizerId "Use generic-lens or generic-optics with 'authorizerId' instead"  #-}

instance Core.ToQuery DeleteAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAuthorizer where
        toHeaders DeleteAuthorizer{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteAuthorizer where
        type Rs DeleteAuthorizer = DeleteAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/authorizers/"
                             Core.<> Core.toText authorizerId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAuthorizerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAuthorizerResponse' smart constructor.
data DeleteAuthorizerResponse = DeleteAuthorizerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAuthorizerResponse' value with any optional fields omitted.
mkDeleteAuthorizerResponse
    :: DeleteAuthorizerResponse
mkDeleteAuthorizerResponse = DeleteAuthorizerResponse'
