{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DescribeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the group.
module Network.AWS.WorkMail.DescribeGroup
  ( -- * Creating a request
    DescribeGroup (..),
    mkDescribeGroup,

    -- ** Request lenses
    dgOrganizationId,
    dgGroupId,

    -- * Destructuring the response
    DescribeGroupResponse (..),
    mkDescribeGroupResponse,

    -- ** Response lenses
    dgrfrsDisabledDate,
    dgrfrsEmail,
    dgrfrsEnabledDate,
    dgrfrsGroupId,
    dgrfrsName,
    dgrfrsState,
    dgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDescribeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { -- | The identifier for the organization under which the group exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the group to be described.
    groupId :: Types.WorkMailIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGroup' value with any optional fields omitted.
mkDescribeGroup ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'groupId'
  Types.WorkMailIdentifier ->
  DescribeGroup
mkDescribeGroup organizationId groupId =
  DescribeGroup' {organizationId, groupId}

-- | The identifier for the organization under which the group exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgOrganizationId :: Lens.Lens' DescribeGroup Types.OrganizationId
dgOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dgOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the group to be described.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupId :: Lens.Lens' DescribeGroup Types.WorkMailIdentifier
dgGroupId = Lens.field @"groupId"
{-# DEPRECATED dgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Core.FromJSON DescribeGroup where
  toJSON DescribeGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("GroupId" Core..= groupId)
          ]
      )

instance Core.AWSRequest DescribeGroup where
  type Rs DescribeGroup = DescribeGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.DescribeGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupResponse'
            Core.<$> (x Core..:? "DisabledDate")
            Core.<*> (x Core..:? "Email")
            Core.<*> (x Core..:? "EnabledDate")
            Core.<*> (x Core..:? "GroupId")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { -- | The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
    disabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The email of the described group.
    email :: Core.Maybe Types.Email,
    -- | The date and time when a user was registered to WorkMail, in UNIX epoch time format.
    enabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the described group.
    groupId :: Core.Maybe Types.WorkMailIdentifier,
    -- | The name of the described group.
    name :: Core.Maybe Types.Name,
    -- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
    state :: Core.Maybe Types.EntityState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeGroupResponse' value with any optional fields omitted.
mkDescribeGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGroupResponse
mkDescribeGroupResponse responseStatus =
  DescribeGroupResponse'
    { disabledDate = Core.Nothing,
      email = Core.Nothing,
      enabledDate = Core.Nothing,
      groupId = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The date and time when a user was deregistered from WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsDisabledDate :: Lens.Lens' DescribeGroupResponse (Core.Maybe Core.NominalDiffTime)
dgrfrsDisabledDate = Lens.field @"disabledDate"
{-# DEPRECATED dgrfrsDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The email of the described group.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsEmail :: Lens.Lens' DescribeGroupResponse (Core.Maybe Types.Email)
dgrfrsEmail = Lens.field @"email"
{-# DEPRECATED dgrfrsEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The date and time when a user was registered to WorkMail, in UNIX epoch time format.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsEnabledDate :: Lens.Lens' DescribeGroupResponse (Core.Maybe Core.NominalDiffTime)
dgrfrsEnabledDate = Lens.field @"enabledDate"
{-# DEPRECATED dgrfrsEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The identifier of the described group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsGroupId :: Lens.Lens' DescribeGroupResponse (Core.Maybe Types.WorkMailIdentifier)
dgrfrsGroupId = Lens.field @"groupId"
{-# DEPRECATED dgrfrsGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the described group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsName :: Lens.Lens' DescribeGroupResponse (Core.Maybe Types.Name)
dgrfrsName = Lens.field @"name"
{-# DEPRECATED dgrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to WorkMail).
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsState :: Lens.Lens' DescribeGroupResponse (Core.Maybe Types.EntityState)
dgrfrsState = Lens.field @"state"
{-# DEPRECATED dgrfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrfrsResponseStatus :: Lens.Lens' DescribeGroupResponse Core.Int
dgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
