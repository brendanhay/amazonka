{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DB subnet group. DB subnet groups must contain at least one subnet in at least two AZs in the AWS Region.
module Network.AWS.RDS.ModifyDBSubnetGroup
    (
    -- * Creating a request
      ModifyDBSubnetGroup (..)
    , mkModifyDBSubnetGroup
    -- ** Request lenses
    , mdbsgDBSubnetGroupName
    , mdbsgSubnetIds
    , mdbsgDBSubnetGroupDescription

    -- * Destructuring the response
    , ModifyDBSubnetGroupResponse (..)
    , mkModifyDBSubnetGroupResponse
    -- ** Response lenses
    , mdbsgrrsDBSubnetGroup
    , mdbsgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyDBSubnetGroup' smart constructor.
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
  { dBSubnetGroupName :: Core.Text
    -- ^ The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group. 
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@ 
  , subnetIds :: [Core.Text]
    -- ^ The EC2 subnet IDs for the DB subnet group.
  , dBSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ The description for the DB subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSubnetGroup' value with any optional fields omitted.
mkModifyDBSubnetGroup
    :: Core.Text -- ^ 'dBSubnetGroupName'
    -> ModifyDBSubnetGroup
mkModifyDBSubnetGroup dBSubnetGroupName
  = ModifyDBSubnetGroup'{dBSubnetGroupName, subnetIds = Core.mempty,
                         dBSubnetGroupDescription = Core.Nothing}

-- | The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group. 
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgDBSubnetGroupName :: Lens.Lens' ModifyDBSubnetGroup Core.Text
mdbsgDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE mdbsgDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | The EC2 subnet IDs for the DB subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgSubnetIds :: Lens.Lens' ModifyDBSubnetGroup [Core.Text]
mdbsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE mdbsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The description for the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgDBSubnetGroupDescription :: Lens.Lens' ModifyDBSubnetGroup (Core.Maybe Core.Text)
mdbsgDBSubnetGroupDescription = Lens.field @"dBSubnetGroupDescription"
{-# INLINEABLE mdbsgDBSubnetGroupDescription #-}
{-# DEPRECATED dBSubnetGroupDescription "Use generic-lens or generic-optics with 'dBSubnetGroupDescription' instead"  #-}

instance Core.ToQuery ModifyDBSubnetGroup where
        toQuery ModifyDBSubnetGroup{..}
          = Core.toQueryPair "Action" ("ModifyDBSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "DBSubnetGroupName" dBSubnetGroupName
              Core.<>
              Core.toQueryPair "SubnetIds"
                (Core.toQueryList "SubnetIdentifier" subnetIds)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBSubnetGroupDescription")
                dBSubnetGroupDescription

instance Core.ToHeaders ModifyDBSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBSubnetGroup where
        type Rs ModifyDBSubnetGroup = ModifyDBSubnetGroupResponse
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
          = Response.receiveXMLWrapper "ModifyDBSubnetGroupResult"
              (\ s h x ->
                 ModifyDBSubnetGroupResponse' Core.<$>
                   (x Core..@? "DBSubnetGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBSubnetGroupResponse' smart constructor.
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
  { dBSubnetGroup :: Core.Maybe Types.DBSubnetGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSubnetGroupResponse' value with any optional fields omitted.
mkModifyDBSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBSubnetGroupResponse
mkModifyDBSubnetGroupResponse responseStatus
  = ModifyDBSubnetGroupResponse'{dBSubnetGroup = Core.Nothing,
                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgrrsDBSubnetGroup :: Lens.Lens' ModifyDBSubnetGroupResponse (Core.Maybe Types.DBSubnetGroup)
mdbsgrrsDBSubnetGroup = Lens.field @"dBSubnetGroup"
{-# INLINEABLE mdbsgrrsDBSubnetGroup #-}
{-# DEPRECATED dBSubnetGroup "Use generic-lens or generic-optics with 'dBSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgrrsResponseStatus :: Lens.Lens' ModifyDBSubnetGroupResponse Core.Int
mdbsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
