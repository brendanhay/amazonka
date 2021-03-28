{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateInputSecurityGroup (..)
    , mkCreateInputSecurityGroup
    -- ** Request lenses
    , cisgTags
    , cisgWhitelistRules

    -- * Destructuring the response
    , CreateInputSecurityGroupResponse (..)
    , mkCreateInputSecurityGroupResponse
    -- ** Response lenses
    , cisgrrsSecurityGroup
    , cisgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The IPv4 CIDRs to whitelist for this Input Security Group
--
-- /See:/ 'mkCreateInputSecurityGroup' smart constructor.
data CreateInputSecurityGroup = CreateInputSecurityGroup'
  { tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs.
  , whitelistRules :: Core.Maybe [Types.InputWhitelistRuleCidr]
    -- ^ List of IPv4 CIDR addresses to whitelist
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInputSecurityGroup' value with any optional fields omitted.
mkCreateInputSecurityGroup
    :: CreateInputSecurityGroup
mkCreateInputSecurityGroup
  = CreateInputSecurityGroup'{tags = Core.Nothing,
                              whitelistRules = Core.Nothing}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgTags :: Lens.Lens' CreateInputSecurityGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
cisgTags = Lens.field @"tags"
{-# INLINEABLE cisgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | List of IPv4 CIDR addresses to whitelist
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgWhitelistRules :: Lens.Lens' CreateInputSecurityGroup (Core.Maybe [Types.InputWhitelistRuleCidr])
cisgWhitelistRules = Lens.field @"whitelistRules"
{-# INLINEABLE cisgWhitelistRules #-}
{-# DEPRECATED whitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead"  #-}

instance Core.ToQuery CreateInputSecurityGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateInputSecurityGroup where
        toHeaders CreateInputSecurityGroup{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateInputSecurityGroup where
        toJSON CreateInputSecurityGroup{..}
          = Core.object
              (Core.catMaybes
                 [("tags" Core..=) Core.<$> tags,
                  ("whitelistRules" Core..=) Core.<$> whitelistRules])

instance Core.AWSRequest CreateInputSecurityGroup where
        type Rs CreateInputSecurityGroup = CreateInputSecurityGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/prod/inputSecurityGroups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateInputSecurityGroupResponse' Core.<$>
                   (x Core..:? "securityGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for CreateInputSecurityGroupResponse
--
-- /See:/ 'mkCreateInputSecurityGroupResponse' smart constructor.
data CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse'
  { securityGroup :: Core.Maybe Types.InputSecurityGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInputSecurityGroupResponse' value with any optional fields omitted.
mkCreateInputSecurityGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInputSecurityGroupResponse
mkCreateInputSecurityGroupResponse responseStatus
  = CreateInputSecurityGroupResponse'{securityGroup = Core.Nothing,
                                      responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'securityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrrsSecurityGroup :: Lens.Lens' CreateInputSecurityGroupResponse (Core.Maybe Types.InputSecurityGroup)
cisgrrsSecurityGroup = Lens.field @"securityGroup"
{-# INLINEABLE cisgrrsSecurityGroup #-}
{-# DEPRECATED securityGroup "Use generic-lens or generic-optics with 'securityGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cisgrrsResponseStatus :: Lens.Lens' CreateInputSecurityGroupResponse Core.Int
cisgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cisgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
