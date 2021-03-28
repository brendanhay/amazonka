{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DeleteServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the server and the underlying AWS CloudFormation stacks (including the server's EC2 instance). When you run this command, the server state is updated to @DELETING@ . After the server is deleted, it is no longer returned by @DescribeServer@ requests. If the AWS CloudFormation stack cannot be deleted, the server cannot be deleted. 
--
-- This operation is asynchronous. 
-- An @InvalidStateException@ is thrown when a server deletion is already in progress. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid. 
--
module Network.AWS.OpsWorksCM.DeleteServer
    (
    -- * Creating a request
      DeleteServer (..)
    , mkDeleteServer
    -- ** Request lenses
    , dsServerName

    -- * Destructuring the response
    , DeleteServerResponse (..)
    , mkDeleteServerResponse
    -- ** Response lenses
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteServer' smart constructor.
newtype DeleteServer = DeleteServer'
  { serverName :: Types.ServerName
    -- ^ The ID of the server to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServer' value with any optional fields omitted.
mkDeleteServer
    :: Types.ServerName -- ^ 'serverName'
    -> DeleteServer
mkDeleteServer serverName = DeleteServer'{serverName}

-- | The ID of the server to delete.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServerName :: Lens.Lens' DeleteServer Types.ServerName
dsServerName = Lens.field @"serverName"
{-# INLINEABLE dsServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

instance Core.ToQuery DeleteServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteServer where
        toHeaders DeleteServer{..}
          = Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.DeleteServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteServer where
        toJSON DeleteServer{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ServerName" Core..= serverName)])

instance Core.AWSRequest DeleteServer where
        type Rs DeleteServer = DeleteServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteServerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteServerResponse' smart constructor.
newtype DeleteServerResponse = DeleteServerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServerResponse' value with any optional fields omitted.
mkDeleteServerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteServerResponse
mkDeleteServerResponse responseStatus
  = DeleteServerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteServerResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
