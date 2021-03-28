{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon RDS DB instance. When you stop a DB instance, Amazon RDS retains the DB instance's metadata, including its endpoint, DB parameter group, and option group membership. Amazon RDS also retains the transaction logs so you can do a point-in-time restore if necessary. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_StopInstance.html Stopping an Amazon RDS DB Instance Temporarily> in the /Amazon RDS User Guide./ 
module Network.AWS.RDS.StopDBInstance
    (
    -- * Creating a request
      StopDBInstance (..)
    , mkStopDBInstance
    -- ** Request lenses
    , sdbiDBInstanceIdentifier
    , sdbiDBSnapshotIdentifier

    -- * Destructuring the response
    , StopDBInstanceResponse (..)
    , mkStopDBInstanceResponse
    -- ** Response lenses
    , sdbirrsDBInstance
    , sdbirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopDBInstance' smart constructor.
data StopDBInstance = StopDBInstance'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The user-supplied instance identifier. 
  , dBSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopDBInstance' value with any optional fields omitted.
mkStopDBInstance
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> StopDBInstance
mkStopDBInstance dBInstanceIdentifier
  = StopDBInstance'{dBInstanceIdentifier,
                    dBSnapshotIdentifier = Core.Nothing}

-- | The user-supplied instance identifier. 
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbiDBInstanceIdentifier :: Lens.Lens' StopDBInstance Core.Text
sdbiDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE sdbiDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The user-supplied instance identifier of the DB Snapshot created immediately before the DB instance is stopped. 
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbiDBSnapshotIdentifier :: Lens.Lens' StopDBInstance (Core.Maybe Core.Text)
sdbiDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE sdbiDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

instance Core.ToQuery StopDBInstance where
        toQuery StopDBInstance{..}
          = Core.toQueryPair "Action" ("StopDBInstance" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSnapshotIdentifier")
                dBSnapshotIdentifier

instance Core.ToHeaders StopDBInstance where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest StopDBInstance where
        type Rs StopDBInstance = StopDBInstanceResponse
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
          = Response.receiveXMLWrapper "StopDBInstanceResult"
              (\ s h x ->
                 StopDBInstanceResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopDBInstanceResponse' smart constructor.
data StopDBInstanceResponse = StopDBInstanceResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopDBInstanceResponse' value with any optional fields omitted.
mkStopDBInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopDBInstanceResponse
mkStopDBInstanceResponse responseStatus
  = StopDBInstanceResponse'{dBInstance = Core.Nothing,
                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirrsDBInstance :: Lens.Lens' StopDBInstanceResponse (Core.Maybe Types.DBInstance)
sdbirrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE sdbirrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdbirrsResponseStatus :: Lens.Lens' StopDBInstanceResponse Core.Int
sdbirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdbirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
