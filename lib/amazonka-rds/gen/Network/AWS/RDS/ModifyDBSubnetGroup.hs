{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyDBSubnetGroup (..),
    mkModifyDBSubnetGroup,

    -- ** Request lenses
    mdbsgDBSubnetGroupName,
    mdbsgSubnetIds,
    mdbsgDBSubnetGroupDescription,

    -- * Destructuring the response
    ModifyDBSubnetGroupResponse (..),
    mkModifyDBSubnetGroupResponse,

    -- ** Response lenses
    mdbsgrrsDBSubnetGroup,
    mdbsgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyDBSubnetGroup' smart constructor.
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
  { -- | The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Types.String,
    -- | The EC2 subnet IDs for the DB subnet group.
    subnetIds :: [Types.String],
    -- | The description for the DB subnet group.
    dBSubnetGroupDescription :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSubnetGroup' value with any optional fields omitted.
mkModifyDBSubnetGroup ::
  -- | 'dBSubnetGroupName'
  Types.String ->
  ModifyDBSubnetGroup
mkModifyDBSubnetGroup dBSubnetGroupName =
  ModifyDBSubnetGroup'
    { dBSubnetGroupName,
      subnetIds = Core.mempty,
      dBSubnetGroupDescription = Core.Nothing
    }

-- | The name for the DB subnet group. This value is stored as a lowercase string. You can't modify the default subnet group.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgDBSubnetGroupName :: Lens.Lens' ModifyDBSubnetGroup Types.String
mdbsgDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED mdbsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | The EC2 subnet IDs for the DB subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgSubnetIds :: Lens.Lens' ModifyDBSubnetGroup [Types.String]
mdbsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED mdbsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The description for the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgDBSubnetGroupDescription :: Lens.Lens' ModifyDBSubnetGroup (Core.Maybe Types.String)
mdbsgDBSubnetGroupDescription = Lens.field @"dBSubnetGroupDescription"
{-# DEPRECATED mdbsgDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dBSubnetGroupDescription' instead." #-}

instance Core.AWSRequest ModifyDBSubnetGroup where
  type Rs ModifyDBSubnetGroup = ModifyDBSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyDBSubnetGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSubnetGroupName" dBSubnetGroupName)
                Core.<> ( Core.toQueryValue
                            "SubnetIds"
                            (Core.toQueryList "SubnetIdentifier" subnetIds)
                        )
                Core.<> ( Core.toQueryValue "DBSubnetGroupDescription"
                            Core.<$> dBSubnetGroupDescription
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBSubnetGroupResult"
      ( \s h x ->
          ModifyDBSubnetGroupResponse'
            Core.<$> (x Core..@? "DBSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBSubnetGroupResponse' smart constructor.
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
  { dBSubnetGroup :: Core.Maybe Types.DBSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSubnetGroupResponse' value with any optional fields omitted.
mkModifyDBSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBSubnetGroupResponse
mkModifyDBSubnetGroupResponse responseStatus =
  ModifyDBSubnetGroupResponse'
    { dBSubnetGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgrrsDBSubnetGroup :: Lens.Lens' ModifyDBSubnetGroupResponse (Core.Maybe Types.DBSubnetGroup)
mdbsgrrsDBSubnetGroup = Lens.field @"dBSubnetGroup"
{-# DEPRECATED mdbsgrrsDBSubnetGroup "Use generic-lens or generic-optics with 'dBSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsgrrsResponseStatus :: Lens.Lens' ModifyDBSubnetGroupResponse Core.Int
mdbsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
