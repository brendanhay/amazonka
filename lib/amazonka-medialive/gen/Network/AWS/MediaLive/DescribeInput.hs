{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces details about an input
module Network.AWS.MediaLive.DescribeInput
  ( -- * Creating a request
    DescribeInput (..),
    mkDescribeInput,

    -- ** Request lenses
    dInputId,

    -- * Destructuring the response
    DescribeInputResponse (..),
    mkDescribeInputResponse,

    -- ** Response lenses
    dirfrsArn,
    dirfrsAttachedChannels,
    dirfrsDestinations,
    dirfrsId,
    dirfrsInputClass,
    dirfrsInputDevices,
    dirfrsInputSourceType,
    dirfrsMediaConnectFlows,
    dirfrsName,
    dirfrsRoleArn,
    dirfrsSecurityGroups,
    dirfrsSources,
    dirfrsState,
    dirfrsTags,
    dirfrsType,
    dirfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'mkDescribeInput' smart constructor.
newtype DescribeInput = DescribeInput'
  { -- | Unique ID of the input
    inputId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInput' value with any optional fields omitted.
mkDescribeInput ::
  -- | 'inputId'
  Core.Text ->
  DescribeInput
mkDescribeInput inputId = DescribeInput' {inputId}

-- | Unique ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInputId :: Lens.Lens' DescribeInput Core.Text
dInputId = Lens.field @"inputId"
{-# DEPRECATED dInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance Core.AWSRequest DescribeInput where
  type Rs DescribeInput = DescribeInputResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/prod/inputs/" Core.<> (Core.toText inputId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "attachedChannels")
            Core.<*> (x Core..:? "destinations")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "inputClass")
            Core.<*> (x Core..:? "inputDevices")
            Core.<*> (x Core..:? "inputSourceType")
            Core.<*> (x Core..:? "mediaConnectFlows")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "securityGroups")
            Core.<*> (x Core..:? "sources")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'mkDescribeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
  { -- | The Unique ARN of the input (generated, immutable).
    arn :: Core.Maybe Core.Text,
    -- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
    attachedChannels :: Core.Maybe [Core.Text],
    -- | A list of the destinations of the input (PUSH-type).
    destinations :: Core.Maybe [Types.InputDestination],
    -- | The generated ID of the input (unique for user account, immutable).
    id :: Core.Maybe Core.Text,
    -- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
    --
    -- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
    inputClass :: Core.Maybe Types.InputClass,
    -- | Settings for the input devices.
    inputDevices :: Core.Maybe [Types.InputDeviceSettings],
    -- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
    --
    -- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
    inputSourceType :: Core.Maybe Types.InputSourceType,
    -- | A list of MediaConnect Flows for this input.
    mediaConnectFlows :: Core.Maybe [Types.MediaConnectFlow],
    -- | The user-assigned name (This is a mutable value).
    name :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
    roleArn :: Core.Maybe Core.Text,
    -- | A list of IDs for all the Input Security Groups attached to the input.
    securityGroups :: Core.Maybe [Core.Text],
    -- | A list of the sources of the input (PULL-type).
    sources :: Core.Maybe [Types.InputSource],
    state :: Core.Maybe Types.InputState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    type' :: Core.Maybe Types.InputType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputResponse' value with any optional fields omitted.
mkDescribeInputResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInputResponse
mkDescribeInputResponse responseStatus =
  DescribeInputResponse'
    { arn = Core.Nothing,
      attachedChannels = Core.Nothing,
      destinations = Core.Nothing,
      id = Core.Nothing,
      inputClass = Core.Nothing,
      inputDevices = Core.Nothing,
      inputSourceType = Core.Nothing,
      mediaConnectFlows = Core.Nothing,
      name = Core.Nothing,
      roleArn = Core.Nothing,
      securityGroups = Core.Nothing,
      sources = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      responseStatus
    }

-- | The Unique ARN of the input (generated, immutable).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsArn :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
dirfrsArn = Lens.field @"arn"
{-# DEPRECATED dirfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- /Note:/ Consider using 'attachedChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsAttachedChannels :: Lens.Lens' DescribeInputResponse (Core.Maybe [Core.Text])
dirfrsAttachedChannels = Lens.field @"attachedChannels"
{-# DEPRECATED dirfrsAttachedChannels "Use generic-lens or generic-optics with 'attachedChannels' instead." #-}

-- | A list of the destinations of the input (PUSH-type).
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsDestinations :: Lens.Lens' DescribeInputResponse (Core.Maybe [Types.InputDestination])
dirfrsDestinations = Lens.field @"destinations"
{-# DEPRECATED dirfrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The generated ID of the input (unique for user account, immutable).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsId :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
dirfrsId = Lens.field @"id"
{-# DEPRECATED dirfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- /Note:/ Consider using 'inputClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsInputClass :: Lens.Lens' DescribeInputResponse (Core.Maybe Types.InputClass)
dirfrsInputClass = Lens.field @"inputClass"
{-# DEPRECATED dirfrsInputClass "Use generic-lens or generic-optics with 'inputClass' instead." #-}

-- | Settings for the input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsInputDevices :: Lens.Lens' DescribeInputResponse (Core.Maybe [Types.InputDeviceSettings])
dirfrsInputDevices = Lens.field @"inputDevices"
{-# DEPRECATED dirfrsInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- /Note:/ Consider using 'inputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsInputSourceType :: Lens.Lens' DescribeInputResponse (Core.Maybe Types.InputSourceType)
dirfrsInputSourceType = Lens.field @"inputSourceType"
{-# DEPRECATED dirfrsInputSourceType "Use generic-lens or generic-optics with 'inputSourceType' instead." #-}

-- | A list of MediaConnect Flows for this input.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsMediaConnectFlows :: Lens.Lens' DescribeInputResponse (Core.Maybe [Types.MediaConnectFlow])
dirfrsMediaConnectFlows = Lens.field @"mediaConnectFlows"
{-# DEPRECATED dirfrsMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | The user-assigned name (This is a mutable value).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsName :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
dirfrsName = Lens.field @"name"
{-# DEPRECATED dirfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsRoleArn :: Lens.Lens' DescribeInputResponse (Core.Maybe Core.Text)
dirfrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dirfrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | A list of IDs for all the Input Security Groups attached to the input.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsSecurityGroups :: Lens.Lens' DescribeInputResponse (Core.Maybe [Core.Text])
dirfrsSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED dirfrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | A list of the sources of the input (PULL-type).
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsSources :: Lens.Lens' DescribeInputResponse (Core.Maybe [Types.InputSource])
dirfrsSources = Lens.field @"sources"
{-# DEPRECATED dirfrsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsState :: Lens.Lens' DescribeInputResponse (Core.Maybe Types.InputState)
dirfrsState = Lens.field @"state"
{-# DEPRECATED dirfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsTags :: Lens.Lens' DescribeInputResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dirfrsTags = Lens.field @"tags"
{-# DEPRECATED dirfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsType :: Lens.Lens' DescribeInputResponse (Core.Maybe Types.InputType)
dirfrsType = Lens.field @"type'"
{-# DEPRECATED dirfrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DescribeInputResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
