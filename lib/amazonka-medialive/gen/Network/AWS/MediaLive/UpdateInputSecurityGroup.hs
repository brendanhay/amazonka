{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an Input Security Group's Whilelists.
module Network.AWS.MediaLive.UpdateInputSecurityGroup
  ( -- * Creating a request
    UpdateInputSecurityGroup (..),
    mkUpdateInputSecurityGroup,

    -- ** Request lenses
    uisgInputSecurityGroupId,
    uisgTags,
    uisgWhitelistRules,

    -- * Destructuring the response
    UpdateInputSecurityGroupResponse (..),
    mkUpdateInputSecurityGroupResponse,

    -- ** Response lenses
    uisgrrsSecurityGroup,
    uisgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to update some combination of the Input Security Group name and the IPv4 CIDRs the Input Security Group should allow.
--
-- /See:/ 'mkUpdateInputSecurityGroup' smart constructor.
data UpdateInputSecurityGroup = UpdateInputSecurityGroup'
  { -- | The id of the Input Security Group to update.
    inputSecurityGroupId :: Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | List of IPv4 CIDR addresses to whitelist
    whitelistRules :: Core.Maybe [Types.InputWhitelistRuleCidr]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputSecurityGroup' value with any optional fields omitted.
mkUpdateInputSecurityGroup ::
  -- | 'inputSecurityGroupId'
  Core.Text ->
  UpdateInputSecurityGroup
mkUpdateInputSecurityGroup inputSecurityGroupId =
  UpdateInputSecurityGroup'
    { inputSecurityGroupId,
      tags = Core.Nothing,
      whitelistRules = Core.Nothing
    }

-- | The id of the Input Security Group to update.
--
-- /Note:/ Consider using 'inputSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgInputSecurityGroupId :: Lens.Lens' UpdateInputSecurityGroup Core.Text
uisgInputSecurityGroupId = Lens.field @"inputSecurityGroupId"
{-# DEPRECATED uisgInputSecurityGroupId "Use generic-lens or generic-optics with 'inputSecurityGroupId' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgTags :: Lens.Lens' UpdateInputSecurityGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
uisgTags = Lens.field @"tags"
{-# DEPRECATED uisgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | List of IPv4 CIDR addresses to whitelist
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgWhitelistRules :: Lens.Lens' UpdateInputSecurityGroup (Core.Maybe [Types.InputWhitelistRuleCidr])
uisgWhitelistRules = Lens.field @"whitelistRules"
{-# DEPRECATED uisgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

instance Core.FromJSON UpdateInputSecurityGroup where
  toJSON UpdateInputSecurityGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("whitelistRules" Core..=) Core.<$> whitelistRules
          ]
      )

instance Core.AWSRequest UpdateInputSecurityGroup where
  type Rs UpdateInputSecurityGroup = UpdateInputSecurityGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputSecurityGroups/"
                Core.<> (Core.toText inputSecurityGroupId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputSecurityGroupResponse'
            Core.<$> (x Core..:? "securityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateInputSecurityGroupResponse
--
-- /See:/ 'mkUpdateInputSecurityGroupResponse' smart constructor.
data UpdateInputSecurityGroupResponse = UpdateInputSecurityGroupResponse'
  { securityGroup :: Core.Maybe Types.InputSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputSecurityGroupResponse' value with any optional fields omitted.
mkUpdateInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateInputSecurityGroupResponse
mkUpdateInputSecurityGroupResponse responseStatus =
  UpdateInputSecurityGroupResponse'
    { securityGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgrrsSecurityGroup :: Lens.Lens' UpdateInputSecurityGroupResponse (Core.Maybe Types.InputSecurityGroup)
uisgrrsSecurityGroup = Lens.field @"securityGroup"
{-# DEPRECATED uisgrrsSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisgrrsResponseStatus :: Lens.Lens' UpdateInputSecurityGroupResponse Core.Int
uisgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uisgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
