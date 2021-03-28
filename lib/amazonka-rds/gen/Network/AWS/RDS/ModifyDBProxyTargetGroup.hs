{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of a @DBProxyTargetGroup@ .
module Network.AWS.RDS.ModifyDBProxyTargetGroup
    (
    -- * Creating a request
      ModifyDBProxyTargetGroup (..)
    , mkModifyDBProxyTargetGroup
    -- ** Request lenses
    , mdbptgTargetGroupName
    , mdbptgDBProxyName
    , mdbptgConnectionPoolConfig
    , mdbptgNewName

    -- * Destructuring the response
    , ModifyDBProxyTargetGroupResponse (..)
    , mkModifyDBProxyTargetGroupResponse
    -- ** Response lenses
    , mdbptgrrsDBProxyTargetGroup
    , mdbptgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyDBProxyTargetGroup' smart constructor.
data ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroup'
  { targetGroupName :: Core.Text
    -- ^ The name of the new target group to assign to the proxy.
  , dBProxyName :: Core.Text
    -- ^ The name of the new proxy to which to assign the target group.
  , connectionPoolConfig :: Core.Maybe Types.ConnectionPoolConfiguration
    -- ^ The settings that determine the size and behavior of the connection pool for the target group.
  , newName :: Core.Maybe Core.Text
    -- ^ The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBProxyTargetGroup' value with any optional fields omitted.
mkModifyDBProxyTargetGroup
    :: Core.Text -- ^ 'targetGroupName'
    -> Core.Text -- ^ 'dBProxyName'
    -> ModifyDBProxyTargetGroup
mkModifyDBProxyTargetGroup targetGroupName dBProxyName
  = ModifyDBProxyTargetGroup'{targetGroupName, dBProxyName,
                              connectionPoolConfig = Core.Nothing, newName = Core.Nothing}

-- | The name of the new target group to assign to the proxy.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgTargetGroupName :: Lens.Lens' ModifyDBProxyTargetGroup Core.Text
mdbptgTargetGroupName = Lens.field @"targetGroupName"
{-# INLINEABLE mdbptgTargetGroupName #-}
{-# DEPRECATED targetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead"  #-}

-- | The name of the new proxy to which to assign the target group.
--
-- /Note:/ Consider using 'dBProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgDBProxyName :: Lens.Lens' ModifyDBProxyTargetGroup Core.Text
mdbptgDBProxyName = Lens.field @"dBProxyName"
{-# INLINEABLE mdbptgDBProxyName #-}
{-# DEPRECATED dBProxyName "Use generic-lens or generic-optics with 'dBProxyName' instead"  #-}

-- | The settings that determine the size and behavior of the connection pool for the target group.
--
-- /Note:/ Consider using 'connectionPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgConnectionPoolConfig :: Lens.Lens' ModifyDBProxyTargetGroup (Core.Maybe Types.ConnectionPoolConfiguration)
mdbptgConnectionPoolConfig = Lens.field @"connectionPoolConfig"
{-# INLINEABLE mdbptgConnectionPoolConfig #-}
{-# DEPRECATED connectionPoolConfig "Use generic-lens or generic-optics with 'connectionPoolConfig' instead"  #-}

-- | The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'newName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgNewName :: Lens.Lens' ModifyDBProxyTargetGroup (Core.Maybe Core.Text)
mdbptgNewName = Lens.field @"newName"
{-# INLINEABLE mdbptgNewName #-}
{-# DEPRECATED newName "Use generic-lens or generic-optics with 'newName' instead"  #-}

instance Core.ToQuery ModifyDBProxyTargetGroup where
        toQuery ModifyDBProxyTargetGroup{..}
          = Core.toQueryPair "Action"
              ("ModifyDBProxyTargetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupName" targetGroupName
              Core.<> Core.toQueryPair "DBProxyName" dBProxyName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ConnectionPoolConfig")
                connectionPoolConfig
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "NewName") newName

instance Core.ToHeaders ModifyDBProxyTargetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBProxyTargetGroup where
        type Rs ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyDBProxyTargetGroupResult"
              (\ s h x ->
                 ModifyDBProxyTargetGroupResponse' Core.<$>
                   (x Core..@? "DBProxyTargetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBProxyTargetGroupResponse' smart constructor.
data ModifyDBProxyTargetGroupResponse = ModifyDBProxyTargetGroupResponse'
  { dBProxyTargetGroup :: Core.Maybe Types.DBProxyTargetGroup
    -- ^ The settings of the modified @DBProxyTarget@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyDBProxyTargetGroupResponse' value with any optional fields omitted.
mkModifyDBProxyTargetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBProxyTargetGroupResponse
mkModifyDBProxyTargetGroupResponse responseStatus
  = ModifyDBProxyTargetGroupResponse'{dBProxyTargetGroup =
                                        Core.Nothing,
                                      responseStatus}

-- | The settings of the modified @DBProxyTarget@ .
--
-- /Note:/ Consider using 'dBProxyTargetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgrrsDBProxyTargetGroup :: Lens.Lens' ModifyDBProxyTargetGroupResponse (Core.Maybe Types.DBProxyTargetGroup)
mdbptgrrsDBProxyTargetGroup = Lens.field @"dBProxyTargetGroup"
{-# INLINEABLE mdbptgrrsDBProxyTargetGroup #-}
{-# DEPRECATED dBProxyTargetGroup "Use generic-lens or generic-optics with 'dBProxyTargetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbptgrrsResponseStatus :: Lens.Lens' ModifyDBProxyTargetGroupResponse Core.Int
mdbptgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbptgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
