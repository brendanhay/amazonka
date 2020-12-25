{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an input.
module Network.AWS.MediaLive.UpdateInput
  ( -- * Creating a request
    UpdateInput (..),
    mkUpdateInput,

    -- ** Request lenses
    uiInputId,
    uiDestinations,
    uiInputDevices,
    uiInputSecurityGroups,
    uiMediaConnectFlows,
    uiName,
    uiRoleArn,
    uiSources,

    -- * Destructuring the response
    UpdateInputResponse (..),
    mkUpdateInputResponse,

    -- ** Response lenses
    uirrsInput,
    uirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update an input.
--
-- /See:/ 'mkUpdateInput' smart constructor.
data UpdateInput = UpdateInput'
  { -- | Unique ID of the input.
    inputId :: Core.Text,
    -- | Destination settings for PUSH type inputs.
    destinations :: Core.Maybe [Types.InputDestinationRequest],
    -- | Settings for the devices.
    inputDevices :: Core.Maybe [Types.InputDeviceRequest],
    -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Core.Maybe [Core.Text],
    -- | A list of the MediaConnect Flow ARNs that you want to use as the source of the input. You can specify as few as one
    --
    -- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
    -- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
    mediaConnectFlows :: Core.Maybe [Types.MediaConnectFlowRequest],
    -- | Name of the input.
    name :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
    roleArn :: Core.Maybe Core.Text,
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    --
    -- exactly two source URLs for redundancy.
    -- Only specify sources for PULL type Inputs. Leave Destinations empty.
    sources :: Core.Maybe [Types.InputSourceRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInput' value with any optional fields omitted.
mkUpdateInput ::
  -- | 'inputId'
  Core.Text ->
  UpdateInput
mkUpdateInput inputId =
  UpdateInput'
    { inputId,
      destinations = Core.Nothing,
      inputDevices = Core.Nothing,
      inputSecurityGroups = Core.Nothing,
      mediaConnectFlows = Core.Nothing,
      name = Core.Nothing,
      roleArn = Core.Nothing,
      sources = Core.Nothing
    }

-- | Unique ID of the input.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputId :: Lens.Lens' UpdateInput Core.Text
uiInputId = Lens.field @"inputId"
{-# DEPRECATED uiInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

-- | Destination settings for PUSH type inputs.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDestinations :: Lens.Lens' UpdateInput (Core.Maybe [Types.InputDestinationRequest])
uiDestinations = Lens.field @"destinations"
{-# DEPRECATED uiDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Settings for the devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputDevices :: Lens.Lens' UpdateInput (Core.Maybe [Types.InputDeviceRequest])
uiInputDevices = Lens.field @"inputDevices"
{-# DEPRECATED uiInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A list of security groups referenced by IDs to attach to the input.
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputSecurityGroups :: Lens.Lens' UpdateInput (Core.Maybe [Core.Text])
uiInputSecurityGroups = Lens.field @"inputSecurityGroups"
{-# DEPRECATED uiInputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead." #-}

-- | A list of the MediaConnect Flow ARNs that you want to use as the source of the input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiMediaConnectFlows :: Lens.Lens' UpdateInput (Core.Maybe [Types.MediaConnectFlowRequest])
uiMediaConnectFlows = Lens.field @"mediaConnectFlows"
{-# DEPRECATED uiMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | Name of the input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiName :: Lens.Lens' UpdateInput (Core.Maybe Core.Text)
uiName = Lens.field @"name"
{-# DEPRECATED uiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiRoleArn :: Lens.Lens' UpdateInput (Core.Maybe Core.Text)
uiRoleArn = Lens.field @"roleArn"
{-# DEPRECATED uiRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiSources :: Lens.Lens' UpdateInput (Core.Maybe [Types.InputSourceRequest])
uiSources = Lens.field @"sources"
{-# DEPRECATED uiSources "Use generic-lens or generic-optics with 'sources' instead." #-}

instance Core.FromJSON UpdateInput where
  toJSON UpdateInput {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinations" Core..=) Core.<$> destinations,
            ("inputDevices" Core..=) Core.<$> inputDevices,
            ("inputSecurityGroups" Core..=) Core.<$> inputSecurityGroups,
            ("mediaConnectFlows" Core..=) Core.<$> mediaConnectFlows,
            ("name" Core..=) Core.<$> name,
            ("roleArn" Core..=) Core.<$> roleArn,
            ("sources" Core..=) Core.<$> sources
          ]
      )

instance Core.AWSRequest UpdateInput where
  type Rs UpdateInput = UpdateInputResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/prod/inputs/" Core.<> (Core.toText inputId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInputResponse'
            Core.<$> (x Core..:? "input") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for UpdateInputResponse
--
-- /See:/ 'mkUpdateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
  { input :: Core.Maybe Types.Input,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInputResponse' value with any optional fields omitted.
mkUpdateInputResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateInputResponse
mkUpdateInputResponse responseStatus =
  UpdateInputResponse' {input = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsInput :: Lens.Lens' UpdateInputResponse (Core.Maybe Types.Input)
uirrsInput = Lens.field @"input"
{-# DEPRECATED uirrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirrsResponseStatus :: Lens.Lens' UpdateInputResponse Core.Int
uirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
