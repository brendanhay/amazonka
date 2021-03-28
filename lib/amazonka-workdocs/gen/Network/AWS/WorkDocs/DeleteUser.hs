{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified user from a Simple AD or Microsoft AD directory.
module Network.AWS.WorkDocs.DeleteUser
    (
    -- * Creating a request
      DeleteUser (..)
    , mkDeleteUser
    -- ** Request lenses
    , dufUserId
    , dufAuthenticationToken

    -- * Destructuring the response
    , DeleteUserResponse (..)
    , mkDeleteUserResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { userId :: Types.IdType
    -- ^ The ID of the user.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser
    :: Types.IdType -- ^ 'userId'
    -> DeleteUser
mkDeleteUser userId
  = DeleteUser'{userId, authenticationToken = Core.Nothing}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufUserId :: Lens.Lens' DeleteUser Types.IdType
dufUserId = Lens.field @"userId"
{-# INLINEABLE dufUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufAuthenticationToken :: Lens.Lens' DeleteUser (Core.Maybe Types.AuthenticationHeaderType)
dufAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dufAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

instance Core.ToQuery DeleteUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteUser where
        toHeaders DeleteUser{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteUser where
        type Rs DeleteUser = DeleteUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/api/v1/users/" Core.<> Core.toText userId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteUserResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
data DeleteUserResponse = DeleteUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserResponse' value with any optional fields omitted.
mkDeleteUserResponse
    :: DeleteUserResponse
mkDeleteUserResponse = DeleteUserResponse'
