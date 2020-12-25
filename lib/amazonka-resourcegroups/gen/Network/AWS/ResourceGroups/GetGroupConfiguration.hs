{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetGroupConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the service configuration associated with the specified resource group. AWS Resource Groups supports configurations for the following resource group types:
--
--
--     * @AWS::EC2::CapacityReservationPool@ - Amazon EC2 capacity reservation pools. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/capacity-reservations-using.html#create-cr-group Working with capacity reservation groups> in the /EC2 Users Guide/ .
module Network.AWS.ResourceGroups.GetGroupConfiguration
  ( -- * Creating a request
    GetGroupConfiguration (..),
    mkGetGroupConfiguration,

    -- ** Request lenses
    ggcGroup,

    -- * Destructuring the response
    GetGroupConfigurationResponse (..),
    mkGetGroupConfigurationResponse,

    -- ** Response lenses
    ggcrrsGroupConfiguration,
    ggcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupConfiguration' smart constructor.
newtype GetGroupConfiguration = GetGroupConfiguration'
  { -- | The name or the ARN of the resource group.
    group :: Core.Maybe Types.GroupString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupConfiguration' value with any optional fields omitted.
mkGetGroupConfiguration ::
  GetGroupConfiguration
mkGetGroupConfiguration =
  GetGroupConfiguration' {group = Core.Nothing}

-- | The name or the ARN of the resource group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcGroup :: Lens.Lens' GetGroupConfiguration (Core.Maybe Types.GroupString)
ggcGroup = Lens.field @"group"
{-# DEPRECATED ggcGroup "Use generic-lens or generic-optics with 'group' instead." #-}

instance Core.FromJSON GetGroupConfiguration where
  toJSON GetGroupConfiguration {..} =
    Core.object (Core.catMaybes [("Group" Core..=) Core.<$> group])

instance Core.AWSRequest GetGroupConfiguration where
  type Rs GetGroupConfiguration = GetGroupConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/get-group-configuration",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupConfigurationResponse'
            Core.<$> (x Core..:? "GroupConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGroupConfigurationResponse' smart constructor.
data GetGroupConfigurationResponse = GetGroupConfigurationResponse'
  { -- | The configuration associated with the specified group.
    groupConfiguration :: Core.Maybe Types.GroupConfiguration,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupConfigurationResponse' value with any optional fields omitted.
mkGetGroupConfigurationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGroupConfigurationResponse
mkGetGroupConfigurationResponse responseStatus =
  GetGroupConfigurationResponse'
    { groupConfiguration = Core.Nothing,
      responseStatus
    }

-- | The configuration associated with the specified group.
--
-- /Note:/ Consider using 'groupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrrsGroupConfiguration :: Lens.Lens' GetGroupConfigurationResponse (Core.Maybe Types.GroupConfiguration)
ggcrrsGroupConfiguration = Lens.field @"groupConfiguration"
{-# DEPRECATED ggcrrsGroupConfiguration "Use generic-lens or generic-optics with 'groupConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggcrrsResponseStatus :: Lens.Lens' GetGroupConfigurationResponse Core.Int
ggcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
