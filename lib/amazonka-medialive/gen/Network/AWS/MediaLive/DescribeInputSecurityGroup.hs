{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a summary of an Input Security Group
module Network.AWS.MediaLive.DescribeInputSecurityGroup
  ( -- * Creating a request
    DescribeInputSecurityGroup (..),
    mkDescribeInputSecurityGroup,

    -- ** Request lenses
    disgInputSecurityGroupId,

    -- * Destructuring the response
    DescribeInputSecurityGroupResponse (..),
    mkDescribeInputSecurityGroupResponse,

    -- ** Response lenses
    disgrfrsArn,
    disgrfrsId,
    disgrfrsInputs,
    disgrfrsState,
    disgrfrsTags,
    disgrfrsWhitelistRules,
    disgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputSecurityGroupRequest
--
-- /See:/ 'mkDescribeInputSecurityGroup' smart constructor.
newtype DescribeInputSecurityGroup = DescribeInputSecurityGroup'
  { -- | The id of the Input Security Group to describe
    inputSecurityGroupId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputSecurityGroup' value with any optional fields omitted.
mkDescribeInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Core.Text ->
  DescribeInputSecurityGroup
mkDescribeInputSecurityGroup inputSecurityGroupId =
  DescribeInputSecurityGroup' {inputSecurityGroupId}

-- | The id of the Input Security Group to describe
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgInputSecurityGroupId :: Lens.Lens' DescribeInputSecurityGroup Core.Text
disgInputSecurityGroupId = Lens.field @"inputSecurityGroupId"
{-# DEPRECATED disgInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

instance Core.AWSRequest DescribeInputSecurityGroup where
  type
    Rs DescribeInputSecurityGroup =
      DescribeInputSecurityGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputSecurityGroups/"
                Core.<> (Core.toText inputSecurityGroupId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputSecurityGroupResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "inputs")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "whitelistRules")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DescribeInputSecurityGroupResponse
--
-- /See:/ 'mkDescribeInputSecurityGroupResponse' smart constructor.
data DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse'
  { -- | Unique ARN of Input Security Group
    arn :: Core.Maybe Core.Text,
    -- | The Id of the Input Security Group
    id :: Core.Maybe Core.Text,
    -- | The list of inputs currently using this Input Security Group.
    inputs :: Core.Maybe [Core.Text],
    -- | The current state of the Input Security Group.
    state :: Core.Maybe Types.InputSecurityGroupState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Whitelist rules and their sync status
    whitelistRules :: Core.Maybe [Types.InputWhitelistRule],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputSecurityGroupResponse' value with any optional fields omitted.
mkDescribeInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInputSecurityGroupResponse
mkDescribeInputSecurityGroupResponse responseStatus =
  DescribeInputSecurityGroupResponse'
    { arn = Core.Nothing,
      id = Core.Nothing,
      inputs = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      whitelistRules = Core.Nothing,
      responseStatus
    }

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsArn :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Core.Text)
disgrfrsArn = Lens.field @"arn"
{-# DEPRECATED disgrfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsId :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Core.Text)
disgrfrsId = Lens.field @"id"
{-# DEPRECATED disgrfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsInputs :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe [Core.Text])
disgrfrsInputs = Lens.field @"inputs"
{-# DEPRECATED disgrfrsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The current state of the Input Security Group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsState :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe Types.InputSecurityGroupState)
disgrfrsState = Lens.field @"state"
{-# DEPRECATED disgrfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsTags :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
disgrfrsTags = Lens.field @"tags"
{-# DEPRECATED disgrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsWhitelistRules :: Lens.Lens' DescribeInputSecurityGroupResponse (Core.Maybe [Types.InputWhitelistRule])
disgrfrsWhitelistRules = Lens.field @"whitelistRules"
{-# DEPRECATED disgrfrsWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disgrfrsResponseStatus :: Lens.Lens' DescribeInputSecurityGroupResponse Core.Int
disgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED disgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
