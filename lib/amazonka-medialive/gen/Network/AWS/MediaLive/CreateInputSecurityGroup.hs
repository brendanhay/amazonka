{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateInputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Input Security Group
module Network.AWS.MediaLive.CreateInputSecurityGroup
  ( -- * Creating a request
    CreateInputSecurityGroup (..),
    mkCreateInputSecurityGroup,

    -- ** Request lenses
    cisgTags,
    cisgWhitelistRules,

    -- * Destructuring the response
    CreateInputSecurityGroupResponse (..),
    mkCreateInputSecurityGroupResponse,

    -- ** Response lenses
    cisgrrsSecurityGroup,
    cisgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The IPv4 CIDRs to whitelist for this Input Security Group
--
-- /See:/ 'mkCreateInputSecurityGroup' smart constructor.
data CreateInputSecurityGroup = CreateInputSecurityGroup'
  { -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | List of IPv4 CIDR addresses to whitelist
    whitelistRules :: Core.Maybe [Types.InputWhitelistRuleCidr]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInputSecurityGroup' value with any optional fields omitted.
mkCreateInputSecurityGroup ::
  CreateInputSecurityGroup
mkCreateInputSecurityGroup =
  CreateInputSecurityGroup'
    { tags = Core.Nothing,
      whitelistRules = Core.Nothing
    }

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgTags :: Lens.Lens' CreateInputSecurityGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
cisgTags = Lens.field @"tags"
{-# DEPRECATED cisgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | List of IPv4 CIDR addresses to whitelist
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgWhitelistRules :: Lens.Lens' CreateInputSecurityGroup (Core.Maybe [Types.InputWhitelistRuleCidr])
cisgWhitelistRules = Lens.field @"whitelistRules"
{-# DEPRECATED cisgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

instance Core.FromJSON CreateInputSecurityGroup where
  toJSON CreateInputSecurityGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("whitelistRules" Core..=) Core.<$> whitelistRules
          ]
      )

instance Core.AWSRequest CreateInputSecurityGroup where
  type Rs CreateInputSecurityGroup = CreateInputSecurityGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/prod/inputSecurityGroups",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInputSecurityGroupResponse'
            Core.<$> (x Core..:? "securityGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for CreateInputSecurityGroupResponse
--
-- /See:/ 'mkCreateInputSecurityGroupResponse' smart constructor.
data CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse'
  { securityGroup :: Core.Maybe Types.InputSecurityGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInputSecurityGroupResponse' value with any optional fields omitted.
mkCreateInputSecurityGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInputSecurityGroupResponse
mkCreateInputSecurityGroupResponse responseStatus =
  CreateInputSecurityGroupResponse'
    { securityGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrrsSecurityGroup :: Lens.Lens' CreateInputSecurityGroupResponse (Core.Maybe Types.InputSecurityGroup)
cisgrrsSecurityGroup = Lens.field @"securityGroup"
{-# DEPRECATED cisgrrsSecurityGroup "Use generic-lens or generic-optics with 'securityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrrsResponseStatus :: Lens.Lens' CreateInputSecurityGroupResponse Core.Int
cisgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cisgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
