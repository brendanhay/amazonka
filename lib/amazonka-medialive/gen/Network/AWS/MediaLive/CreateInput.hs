{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an input
module Network.AWS.MediaLive.CreateInput
  ( -- * Creating a request
    CreateInput (..),
    mkCreateInput,

    -- ** Request lenses
    ciDestinations,
    ciInputDevices,
    ciInputSecurityGroups,
    ciMediaConnectFlows,
    ciName,
    ciRequestId,
    ciRoleArn,
    ciSources,
    ciTags,
    ciType,
    ciVpc,

    -- * Destructuring the response
    CreateInputResponse (..),
    mkCreateInputResponse,

    -- ** Response lenses
    cirrsInput,
    cirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The name of the input
--
-- /See:/ 'mkCreateInput' smart constructor.
data CreateInput = CreateInput'
  { -- | Destination settings for PUSH type inputs.
    destinations :: Core.Maybe [Types.InputDestinationRequest],
    -- | Settings for the devices.
    inputDevices :: Core.Maybe [Types.InputDeviceSettings],
    -- | A list of security groups referenced by IDs to attach to the input.
    inputSecurityGroups :: Core.Maybe [Core.Text],
    -- | A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one
    --
    -- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
    -- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
    mediaConnectFlows :: Core.Maybe [Types.MediaConnectFlowRequest],
    -- | Name of the input.
    name :: Core.Maybe Core.Text,
    -- | Unique identifier of the request to ensure the request is handled
    --
    -- exactly once in case of retries.
    requestId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
    roleArn :: Core.Maybe Core.Text,
    -- | The source URLs for a PULL-type input. Every PULL type input needs
    --
    -- exactly two source URLs for redundancy.
    -- Only specify sources for PULL type Inputs. Leave Destinations empty.
    sources :: Core.Maybe [Types.InputSourceRequest],
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    type' :: Core.Maybe Types.InputType,
    vpc :: Core.Maybe Types.InputVpcRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInput' value with any optional fields omitted.
mkCreateInput ::
  CreateInput
mkCreateInput =
  CreateInput'
    { destinations = Core.Nothing,
      inputDevices = Core.Nothing,
      inputSecurityGroups = Core.Nothing,
      mediaConnectFlows = Core.Nothing,
      name = Core.Nothing,
      requestId = Core.Nothing,
      roleArn = Core.Nothing,
      sources = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      vpc = Core.Nothing
    }

-- | Destination settings for PUSH type inputs.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinations :: Lens.Lens' CreateInput (Core.Maybe [Types.InputDestinationRequest])
ciDestinations = Lens.field @"destinations"
{-# DEPRECATED ciDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Settings for the devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInputDevices :: Lens.Lens' CreateInput (Core.Maybe [Types.InputDeviceSettings])
ciInputDevices = Lens.field @"inputDevices"
{-# DEPRECATED ciInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A list of security groups referenced by IDs to attach to the input.
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInputSecurityGroups :: Lens.Lens' CreateInput (Core.Maybe [Core.Text])
ciInputSecurityGroups = Lens.field @"inputSecurityGroups"
{-# DEPRECATED ciInputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead." #-}

-- | A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMediaConnectFlows :: Lens.Lens' CreateInput (Core.Maybe [Types.MediaConnectFlowRequest])
ciMediaConnectFlows = Lens.field @"mediaConnectFlows"
{-# DEPRECATED ciMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | Name of the input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' CreateInput (Core.Maybe Core.Text)
ciName = Lens.field @"name"
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique identifier of the request to ensure the request is handled
--
-- exactly once in case of retries.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRequestId :: Lens.Lens' CreateInput (Core.Maybe Core.Text)
ciRequestId = Lens.field @"requestId"
{-# DEPRECATED ciRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRoleArn :: Lens.Lens' CreateInput (Core.Maybe Core.Text)
ciRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ciRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSources :: Lens.Lens' CreateInput (Core.Maybe [Types.InputSourceRequest])
ciSources = Lens.field @"sources"
{-# DEPRECATED ciSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInput (Core.Maybe (Core.HashMap Core.Text Core.Text))
ciTags = Lens.field @"tags"
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciType :: Lens.Lens' CreateInput (Core.Maybe Types.InputType)
ciType = Lens.field @"type'"
{-# DEPRECATED ciType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVpc :: Lens.Lens' CreateInput (Core.Maybe Types.InputVpcRequest)
ciVpc = Lens.field @"vpc"
{-# DEPRECATED ciVpc "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Core.FromJSON CreateInput where
  toJSON CreateInput {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinations" Core..=) Core.<$> destinations,
            ("inputDevices" Core..=) Core.<$> inputDevices,
            ("inputSecurityGroups" Core..=) Core.<$> inputSecurityGroups,
            ("mediaConnectFlows" Core..=) Core.<$> mediaConnectFlows,
            ("name" Core..=) Core.<$> name,
            ("requestId" Core..=) Core.<$> requestId,
            ("roleArn" Core..=) Core.<$> roleArn,
            ("sources" Core..=) Core.<$> sources,
            ("tags" Core..=) Core.<$> tags,
            ("type" Core..=) Core.<$> type',
            ("vpc" Core..=) Core.<$> vpc
          ]
      )

instance Core.AWSRequest CreateInput where
  type Rs CreateInput = CreateInputResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/prod/inputs",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInputResponse'
            Core.<$> (x Core..:? "input") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for CreateInputResponse
--
-- /See:/ 'mkCreateInputResponse' smart constructor.
data CreateInputResponse = CreateInputResponse'
  { input :: Core.Maybe Types.Input,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInputResponse' value with any optional fields omitted.
mkCreateInputResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInputResponse
mkCreateInputResponse responseStatus =
  CreateInputResponse' {input = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsInput :: Lens.Lens' CreateInputResponse (Core.Maybe Types.Input)
cirrsInput = Lens.field @"input"
{-# DEPRECATED cirrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateInputResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
