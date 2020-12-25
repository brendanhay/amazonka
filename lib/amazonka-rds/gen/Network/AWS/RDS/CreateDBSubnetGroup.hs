{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least one subnet in at least two AZs in the AWS Region.
module Network.AWS.RDS.CreateDBSubnetGroup
  ( -- * Creating a request
    CreateDBSubnetGroup (..),
    mkCreateDBSubnetGroup,

    -- ** Request lenses
    cdbsgfDBSubnetGroupName,
    cdbsgfDBSubnetGroupDescription,
    cdbsgfSubnetIds,
    cdbsgfTags,

    -- * Destructuring the response
    CreateDBSubnetGroupResponse (..),
    mkCreateDBSubnetGroupResponse,

    -- ** Response lenses
    cdbsgrfrsDBSubnetGroup,
    cdbsgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBSubnetGroup' smart constructor.
data CreateDBSubnetGroup = CreateDBSubnetGroup'
  { -- | The name for the DB subnet group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default.
    -- Example: @mySubnetgroup@
    dBSubnetGroupName :: Types.String,
    -- | The description for the DB subnet group.
    dBSubnetGroupDescription :: Types.String,
    -- | The EC2 Subnet IDs for the DB subnet group.
    subnetIds :: [Types.String],
    -- | Tags to assign to the DB subnet group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSubnetGroup' value with any optional fields omitted.
mkCreateDBSubnetGroup ::
  -- | 'dBSubnetGroupName'
  Types.String ->
  -- | 'dBSubnetGroupDescription'
  Types.String ->
  CreateDBSubnetGroup
mkCreateDBSubnetGroup dBSubnetGroupName dBSubnetGroupDescription =
  CreateDBSubnetGroup'
    { dBSubnetGroupName,
      dBSubnetGroupDescription,
      subnetIds = Core.mempty,
      tags = Core.Nothing
    }

-- | The name for the DB subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 letters, numbers, periods, underscores, spaces, or hyphens. Must not be default.
-- Example: @mySubnetgroup@
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgfDBSubnetGroupName :: Lens.Lens' CreateDBSubnetGroup Types.String
cdbsgfDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED cdbsgfDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | The description for the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgfDBSubnetGroupDescription :: Lens.Lens' CreateDBSubnetGroup Types.String
cdbsgfDBSubnetGroupDescription = Lens.field @"dBSubnetGroupDescription"
{-# DEPRECATED cdbsgfDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dBSubnetGroupDescription' instead." #-}

-- | The EC2 Subnet IDs for the DB subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgfSubnetIds :: Lens.Lens' CreateDBSubnetGroup [Types.String]
cdbsgfSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED cdbsgfSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | Tags to assign to the DB subnet group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgfTags :: Lens.Lens' CreateDBSubnetGroup (Core.Maybe [Types.Tag])
cdbsgfTags = Lens.field @"tags"
{-# DEPRECATED cdbsgfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateDBSubnetGroup where
  type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse
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
            ( Core.pure ("Action", "CreateDBSubnetGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSubnetGroupName" dBSubnetGroupName)
                Core.<> ( Core.toQueryValue
                            "DBSubnetGroupDescription"
                            dBSubnetGroupDescription
                        )
                Core.<> ( Core.toQueryValue
                            "SubnetIds"
                            (Core.toQueryList "SubnetIdentifier" subnetIds)
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBSubnetGroupResult"
      ( \s h x ->
          CreateDBSubnetGroupResponse'
            Core.<$> (x Core..@? "DBSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBSubnetGroupResponse' smart constructor.
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
  { dBSubnetGroup :: Core.Maybe Types.DBSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSubnetGroupResponse' value with any optional fields omitted.
mkCreateDBSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBSubnetGroupResponse
mkCreateDBSubnetGroupResponse responseStatus =
  CreateDBSubnetGroupResponse'
    { dBSubnetGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrfrsDBSubnetGroup :: Lens.Lens' CreateDBSubnetGroupResponse (Core.Maybe Types.DBSubnetGroup)
cdbsgrfrsDBSubnetGroup = Lens.field @"dBSubnetGroup"
{-# DEPRECATED cdbsgrfrsDBSubnetGroup "Use generic-lens or generic-optics with 'dBSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrfrsResponseStatus :: Lens.Lens' CreateDBSubnetGroupResponse Core.Int
cdbsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbsgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
