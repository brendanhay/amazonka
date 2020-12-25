{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specification for the specified protection group.
module Network.AWS.Shield.DescribeProtectionGroup
  ( -- * Creating a request
    DescribeProtectionGroup (..),
    mkDescribeProtectionGroup,

    -- ** Request lenses
    dProtectionGroupId,

    -- * Destructuring the response
    DescribeProtectionGroupResponse (..),
    mkDescribeProtectionGroupResponse,

    -- ** Response lenses
    drsProtectionGroup,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeProtectionGroup' smart constructor.
newtype DescribeProtectionGroup = DescribeProtectionGroup'
  { -- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
    protectionGroupId :: Types.ProtectionGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProtectionGroup' value with any optional fields omitted.
mkDescribeProtectionGroup ::
  -- | 'protectionGroupId'
  Types.ProtectionGroupId ->
  DescribeProtectionGroup
mkDescribeProtectionGroup protectionGroupId =
  DescribeProtectionGroup' {protectionGroupId}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProtectionGroupId :: Lens.Lens' DescribeProtectionGroup Types.ProtectionGroupId
dProtectionGroupId = Lens.field @"protectionGroupId"
{-# DEPRECATED dProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

instance Core.FromJSON DescribeProtectionGroup where
  toJSON DescribeProtectionGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ProtectionGroupId" Core..= protectionGroupId)]
      )

instance Core.AWSRequest DescribeProtectionGroup where
  type Rs DescribeProtectionGroup = DescribeProtectionGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.DescribeProtectionGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectionGroupResponse'
            Core.<$> (x Core..: "ProtectionGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeProtectionGroupResponse' smart constructor.
data DescribeProtectionGroupResponse = DescribeProtectionGroupResponse'
  { -- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
    protectionGroup :: Types.ProtectionGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeProtectionGroupResponse' value with any optional fields omitted.
mkDescribeProtectionGroupResponse ::
  -- | 'protectionGroup'
  Types.ProtectionGroup ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeProtectionGroupResponse
mkDescribeProtectionGroupResponse protectionGroup responseStatus =
  DescribeProtectionGroupResponse' {protectionGroup, responseStatus}

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
--
-- /Note:/ Consider using 'protectionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProtectionGroup :: Lens.Lens' DescribeProtectionGroupResponse Types.ProtectionGroup
drsProtectionGroup = Lens.field @"protectionGroup"
{-# DEPRECATED drsProtectionGroup "Use generic-lens or generic-optics with 'protectionGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProtectionGroupResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
