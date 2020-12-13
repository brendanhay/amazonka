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
    difrsState,
    difrsSecurityGroups,
    difrsARN,
    difrsInputDevices,
    difrsSources,
    difrsDestinations,
    difrsName,
    difrsAttachedChannels,
    difrsId,
    difrsInputClass,
    difrsType,
    difrsMediaConnectFlows,
    difrsInputSourceType,
    difrsTags,
    difrsRoleARN,
    difrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'mkDescribeInput' smart constructor.
newtype DescribeInput = DescribeInput'
  { -- | Unique ID of the input
    inputId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInput' with the minimum fields required to make a request.
--
-- * 'inputId' - Unique ID of the input
mkDescribeInput ::
  -- | 'inputId'
  Lude.Text ->
  DescribeInput
mkDescribeInput pInputId_ = DescribeInput' {inputId = pInputId_}

-- | Unique ID of the input
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dInputId :: Lens.Lens' DescribeInput Lude.Text
dInputId = Lens.lens (inputId :: DescribeInput -> Lude.Text) (\s a -> s {inputId = a} :: DescribeInput)
{-# DEPRECATED dInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance Lude.AWSRequest DescribeInput where
  type Rs DescribeInput = DescribeInputResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInputResponse'
            Lude.<$> (x Lude..?> "state")
            Lude.<*> (x Lude..?> "securityGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "inputDevices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "sources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "destinations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "attachedChannels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "inputClass")
            Lude.<*> (x Lude..?> "type")
            Lude.<*> (x Lude..?> "mediaConnectFlows" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "inputSourceType")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeInput where
  toPath DescribeInput' {..} =
    Lude.mconcat ["/prod/inputs/", Lude.toBS inputId]

instance Lude.ToQuery DescribeInput where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'mkDescribeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'
  { state :: Lude.Maybe InputState,
    -- | A list of IDs for all the Input Security Groups attached to the input.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The Unique ARN of the input (generated, immutable).
    arn :: Lude.Maybe Lude.Text,
    -- | Settings for the input devices.
    inputDevices :: Lude.Maybe [InputDeviceSettings],
    -- | A list of the sources of the input (PULL-type).
    sources :: Lude.Maybe [InputSource],
    -- | A list of the destinations of the input (PUSH-type).
    destinations :: Lude.Maybe [InputDestination],
    -- | The user-assigned name (This is a mutable value).
    name :: Lude.Maybe Lude.Text,
    -- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
    attachedChannels :: Lude.Maybe [Lude.Text],
    -- | The generated ID of the input (unique for user account, immutable).
    id :: Lude.Maybe Lude.Text,
    -- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
    --
    -- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
    inputClass :: Lude.Maybe InputClass,
    type' :: Lude.Maybe InputType,
    -- | A list of MediaConnect Flows for this input.
    mediaConnectFlows :: Lude.Maybe [MediaConnectFlow],
    -- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
    --
    -- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
    inputSourceType :: Lude.Maybe InputSourceType,
    -- | A collection of key-value pairs.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputResponse' with the minimum fields required to make a request.
--
-- * 'state' -
-- * 'securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
-- * 'arn' - The Unique ARN of the input (generated, immutable).
-- * 'inputDevices' - Settings for the input devices.
-- * 'sources' - A list of the sources of the input (PULL-type).
-- * 'destinations' - A list of the destinations of the input (PUSH-type).
-- * 'name' - The user-assigned name (This is a mutable value).
-- * 'attachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
-- * 'id' - The generated ID of the input (unique for user account, immutable).
-- * 'inputClass' - STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
-- * 'type'' -
-- * 'mediaConnectFlows' - A list of MediaConnect Flows for this input.
-- * 'inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
-- * 'tags' - A collection of key-value pairs.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
-- * 'responseStatus' - The response status code.
mkDescribeInputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInputResponse
mkDescribeInputResponse pResponseStatus_ =
  DescribeInputResponse'
    { state = Lude.Nothing,
      securityGroups = Lude.Nothing,
      arn = Lude.Nothing,
      inputDevices = Lude.Nothing,
      sources = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      attachedChannels = Lude.Nothing,
      id = Lude.Nothing,
      inputClass = Lude.Nothing,
      type' = Lude.Nothing,
      mediaConnectFlows = Lude.Nothing,
      inputSourceType = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsState :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputState)
difrsState = Lens.lens (state :: DescribeInputResponse -> Lude.Maybe InputState) (\s a -> s {state = a} :: DescribeInputResponse)
{-# DEPRECATED difrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A list of IDs for all the Input Security Groups attached to the input.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsSecurityGroups :: Lens.Lens' DescribeInputResponse (Lude.Maybe [Lude.Text])
difrsSecurityGroups = Lens.lens (securityGroups :: DescribeInputResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: DescribeInputResponse)
{-# DEPRECATED difrsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The Unique ARN of the input (generated, immutable).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsARN :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
difrsARN = Lens.lens (arn :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeInputResponse)
{-# DEPRECATED difrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Settings for the input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsInputDevices :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputDeviceSettings])
difrsInputDevices = Lens.lens (inputDevices :: DescribeInputResponse -> Lude.Maybe [InputDeviceSettings]) (\s a -> s {inputDevices = a} :: DescribeInputResponse)
{-# DEPRECATED difrsInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A list of the sources of the input (PULL-type).
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsSources :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputSource])
difrsSources = Lens.lens (sources :: DescribeInputResponse -> Lude.Maybe [InputSource]) (\s a -> s {sources = a} :: DescribeInputResponse)
{-# DEPRECATED difrsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A list of the destinations of the input (PUSH-type).
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsDestinations :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputDestination])
difrsDestinations = Lens.lens (destinations :: DescribeInputResponse -> Lude.Maybe [InputDestination]) (\s a -> s {destinations = a} :: DescribeInputResponse)
{-# DEPRECATED difrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The user-assigned name (This is a mutable value).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsName :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
difrsName = Lens.lens (name :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeInputResponse)
{-# DEPRECATED difrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- /Note:/ Consider using 'attachedChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsAttachedChannels :: Lens.Lens' DescribeInputResponse (Lude.Maybe [Lude.Text])
difrsAttachedChannels = Lens.lens (attachedChannels :: DescribeInputResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {attachedChannels = a} :: DescribeInputResponse)
{-# DEPRECATED difrsAttachedChannels "Use generic-lens or generic-optics with 'attachedChannels' instead." #-}

-- | The generated ID of the input (unique for user account, immutable).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsId :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
difrsId = Lens.lens (id :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeInputResponse)
{-# DEPRECATED difrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- /Note:/ Consider using 'inputClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsInputClass :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputClass)
difrsInputClass = Lens.lens (inputClass :: DescribeInputResponse -> Lude.Maybe InputClass) (\s a -> s {inputClass = a} :: DescribeInputResponse)
{-# DEPRECATED difrsInputClass "Use generic-lens or generic-optics with 'inputClass' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsType :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputType)
difrsType = Lens.lens (type' :: DescribeInputResponse -> Lude.Maybe InputType) (\s a -> s {type' = a} :: DescribeInputResponse)
{-# DEPRECATED difrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A list of MediaConnect Flows for this input.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsMediaConnectFlows :: Lens.Lens' DescribeInputResponse (Lude.Maybe [MediaConnectFlow])
difrsMediaConnectFlows = Lens.lens (mediaConnectFlows :: DescribeInputResponse -> Lude.Maybe [MediaConnectFlow]) (\s a -> s {mediaConnectFlows = a} :: DescribeInputResponse)
{-# DEPRECATED difrsMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- /Note:/ Consider using 'inputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsInputSourceType :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputSourceType)
difrsInputSourceType = Lens.lens (inputSourceType :: DescribeInputResponse -> Lude.Maybe InputSourceType) (\s a -> s {inputSourceType = a} :: DescribeInputResponse)
{-# DEPRECATED difrsInputSourceType "Use generic-lens or generic-optics with 'inputSourceType' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsTags :: Lens.Lens' DescribeInputResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
difrsTags = Lens.lens (tags :: DescribeInputResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeInputResponse)
{-# DEPRECATED difrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsRoleARN :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
difrsRoleARN = Lens.lens (roleARN :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeInputResponse)
{-# DEPRECATED difrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
difrsResponseStatus :: Lens.Lens' DescribeInputResponse Lude.Int
difrsResponseStatus = Lens.lens (responseStatus :: DescribeInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputResponse)
{-# DEPRECATED difrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
