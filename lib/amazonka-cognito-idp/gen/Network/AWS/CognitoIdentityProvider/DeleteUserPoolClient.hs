{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to delete the user pool client.
module Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
    (
    -- * Creating a request
      DeleteUserPoolClient (..)
    , mkDeleteUserPoolClient
    -- ** Request lenses
    , dupcUserPoolId
    , dupcClientId

    -- * Destructuring the response
    , DeleteUserPoolClientResponse (..)
    , mkDeleteUserPoolClientResponse
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user pool client.
--
-- /See:/ 'mkDeleteUserPoolClient' smart constructor.
data DeleteUserPoolClient = DeleteUserPoolClient'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to delete the client.
  , clientId :: Types.ClientIdType
    -- ^ The app client ID of the app associated with the user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPoolClient' value with any optional fields omitted.
mkDeleteUserPoolClient
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ClientIdType -- ^ 'clientId'
    -> DeleteUserPoolClient
mkDeleteUserPoolClient userPoolId clientId
  = DeleteUserPoolClient'{userPoolId, clientId}

-- | The user pool ID for the user pool where you want to delete the client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcUserPoolId :: Lens.Lens' DeleteUserPoolClient Types.UserPoolId
dupcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE dupcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The app client ID of the app associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupcClientId :: Lens.Lens' DeleteUserPoolClient Types.ClientIdType
dupcClientId = Lens.field @"clientId"
{-# INLINEABLE dupcClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

instance Core.ToQuery DeleteUserPoolClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUserPoolClient where
        toHeaders DeleteUserPoolClient{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.DeleteUserPoolClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteUserPoolClient where
        toJSON DeleteUserPoolClient{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ClientId" Core..= clientId)])

instance Core.AWSRequest DeleteUserPoolClient where
        type Rs DeleteUserPoolClient = DeleteUserPoolClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUserPoolClientResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserPoolClientResponse' smart constructor.
data DeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPoolClientResponse' value with any optional fields omitted.
mkDeleteUserPoolClientResponse
    :: DeleteUserPoolClientResponse
mkDeleteUserPoolClientResponse = DeleteUserPoolClientResponse'
