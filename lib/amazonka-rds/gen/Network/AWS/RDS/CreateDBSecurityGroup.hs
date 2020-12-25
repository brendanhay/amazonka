{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB security group. DB security groups control access to a DB instance.
module Network.AWS.RDS.CreateDBSecurityGroup
  ( -- * Creating a request
    CreateDBSecurityGroup (..),
    mkCreateDBSecurityGroup,

    -- ** Request lenses
    cdbsgDBSecurityGroupName,
    cdbsgDBSecurityGroupDescription,
    cdbsgTags,

    -- * Destructuring the response
    CreateDBSecurityGroupResponse (..),
    mkCreateDBSecurityGroupResponse,

    -- ** Response lenses
    cdbsgrrsDBSecurityGroup,
    cdbsgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBSecurityGroup' smart constructor.
data CreateDBSecurityGroup = CreateDBSecurityGroup'
  { -- | The name for the DB security group. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must be 1 to 255 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    --     * Must not be "Default"
    --
    --
    -- Example: @mysecuritygroup@
    dBSecurityGroupName :: Types.String,
    -- | The description for the DB security group.
    dBSecurityGroupDescription :: Types.String,
    -- | Tags to assign to the DB security group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSecurityGroup' value with any optional fields omitted.
mkCreateDBSecurityGroup ::
  -- | 'dBSecurityGroupName'
  Types.String ->
  -- | 'dBSecurityGroupDescription'
  Types.String ->
  CreateDBSecurityGroup
mkCreateDBSecurityGroup
  dBSecurityGroupName
  dBSecurityGroupDescription =
    CreateDBSecurityGroup'
      { dBSecurityGroupName,
        dBSecurityGroupDescription,
        tags = Core.Nothing
      }

-- | The name for the DB security group. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--     * Must not be "Default"
--
--
-- Example: @mysecuritygroup@
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupName :: Lens.Lens' CreateDBSecurityGroup Types.String
cdbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# DEPRECATED cdbsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead." #-}

-- | The description for the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgDBSecurityGroupDescription :: Lens.Lens' CreateDBSecurityGroup Types.String
cdbsgDBSecurityGroupDescription = Lens.field @"dBSecurityGroupDescription"
{-# DEPRECATED cdbsgDBSecurityGroupDescription "Use generic-lens or generic-optics with 'dBSecurityGroupDescription' instead." #-}

-- | Tags to assign to the DB security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgTags :: Lens.Lens' CreateDBSecurityGroup (Core.Maybe [Types.Tag])
cdbsgTags = Lens.field @"tags"
{-# DEPRECATED cdbsgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateDBSecurityGroup where
  type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse
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
            ( Core.pure ("Action", "CreateDBSecurityGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSecurityGroupName" dBSecurityGroupName)
                Core.<> ( Core.toQueryValue
                            "DBSecurityGroupDescription"
                            dBSecurityGroupDescription
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBSecurityGroupResult"
      ( \s h x ->
          CreateDBSecurityGroupResponse'
            Core.<$> (x Core..@? "DBSecurityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBSecurityGroupResponse' smart constructor.
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
  { dBSecurityGroup :: Core.Maybe Types.DBSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBSecurityGroupResponse' value with any optional fields omitted.
mkCreateDBSecurityGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBSecurityGroupResponse
mkCreateDBSecurityGroupResponse responseStatus =
  CreateDBSecurityGroupResponse'
    { dBSecurityGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrrsDBSecurityGroup :: Lens.Lens' CreateDBSecurityGroupResponse (Core.Maybe Types.DBSecurityGroup)
cdbsgrrsDBSecurityGroup = Lens.field @"dBSecurityGroup"
{-# DEPRECATED cdbsgrrsDBSecurityGroup "Use generic-lens or generic-optics with 'dBSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbsgrrsResponseStatus :: Lens.Lens' CreateDBSecurityGroupResponse Core.Int
cdbsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
