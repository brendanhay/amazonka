{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    diirsState,
    diirsSecurityGroups,
    diirsARN,
    diirsInputDevices,
    diirsSources,
    diirsDestinations,
    diirsName,
    diirsAttachedChannels,
    diirsId,
    diirsInputClass,
    diirsType,
    diirsMediaConnectFlows,
    diirsInputSourceType,
    diirsTags,
    diirsRoleARN,
    diirsResponseStatus,
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
newtype DescribeInput = DescribeInput' {inputId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { state ::
      Lude.Maybe InputState,
    securityGroups :: Lude.Maybe [Lude.Text],
    arn :: Lude.Maybe Lude.Text,
    inputDevices ::
      Lude.Maybe [InputDeviceSettings],
    sources :: Lude.Maybe [InputSource],
    destinations :: Lude.Maybe [InputDestination],
    name :: Lude.Maybe Lude.Text,
    attachedChannels :: Lude.Maybe [Lude.Text],
    id :: Lude.Maybe Lude.Text,
    inputClass :: Lude.Maybe InputClass,
    type' :: Lude.Maybe InputType,
    mediaConnectFlows ::
      Lude.Maybe [MediaConnectFlow],
    inputSourceType :: Lude.Maybe InputSourceType,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    roleARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Unique ARN of the input (generated, immutable).
-- * 'attachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
-- * 'destinations' - A list of the destinations of the input (PUSH-type).
-- * 'id' - The generated ID of the input (unique for user account, immutable).
-- * 'inputClass' - STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
-- * 'inputDevices' - Settings for the input devices.
-- * 'inputSourceType' - Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
-- * 'mediaConnectFlows' - A list of MediaConnect Flows for this input.
-- * 'name' - The user-assigned name (This is a mutable value).
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
-- * 'securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
-- * 'sources' - A list of the sources of the input (PULL-type).
-- * 'state' - Undocumented field.
-- * 'tags' - A collection of key-value pairs.
-- * 'type'' - Undocumented field.
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
diirsState :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputState)
diirsState = Lens.lens (state :: DescribeInputResponse -> Lude.Maybe InputState) (\s a -> s {state = a} :: DescribeInputResponse)
{-# DEPRECATED diirsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A list of IDs for all the Input Security Groups attached to the input.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsSecurityGroups :: Lens.Lens' DescribeInputResponse (Lude.Maybe [Lude.Text])
diirsSecurityGroups = Lens.lens (securityGroups :: DescribeInputResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: DescribeInputResponse)
{-# DEPRECATED diirsSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The Unique ARN of the input (generated, immutable).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsARN :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
diirsARN = Lens.lens (arn :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeInputResponse)
{-# DEPRECATED diirsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Settings for the input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsInputDevices :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputDeviceSettings])
diirsInputDevices = Lens.lens (inputDevices :: DescribeInputResponse -> Lude.Maybe [InputDeviceSettings]) (\s a -> s {inputDevices = a} :: DescribeInputResponse)
{-# DEPRECATED diirsInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A list of the sources of the input (PULL-type).
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsSources :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputSource])
diirsSources = Lens.lens (sources :: DescribeInputResponse -> Lude.Maybe [InputSource]) (\s a -> s {sources = a} :: DescribeInputResponse)
{-# DEPRECATED diirsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A list of the destinations of the input (PUSH-type).
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsDestinations :: Lens.Lens' DescribeInputResponse (Lude.Maybe [InputDestination])
diirsDestinations = Lens.lens (destinations :: DescribeInputResponse -> Lude.Maybe [InputDestination]) (\s a -> s {destinations = a} :: DescribeInputResponse)
{-# DEPRECATED diirsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The user-assigned name (This is a mutable value).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsName :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
diirsName = Lens.lens (name :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeInputResponse)
{-# DEPRECATED diirsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- /Note:/ Consider using 'attachedChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsAttachedChannels :: Lens.Lens' DescribeInputResponse (Lude.Maybe [Lude.Text])
diirsAttachedChannels = Lens.lens (attachedChannels :: DescribeInputResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {attachedChannels = a} :: DescribeInputResponse)
{-# DEPRECATED diirsAttachedChannels "Use generic-lens or generic-optics with 'attachedChannels' instead." #-}

-- | The generated ID of the input (unique for user account, immutable).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsId :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
diirsId = Lens.lens (id :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeInputResponse)
{-# DEPRECATED diirsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- /Note:/ Consider using 'inputClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsInputClass :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputClass)
diirsInputClass = Lens.lens (inputClass :: DescribeInputResponse -> Lude.Maybe InputClass) (\s a -> s {inputClass = a} :: DescribeInputResponse)
{-# DEPRECATED diirsInputClass "Use generic-lens or generic-optics with 'inputClass' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsType :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputType)
diirsType = Lens.lens (type' :: DescribeInputResponse -> Lude.Maybe InputType) (\s a -> s {type' = a} :: DescribeInputResponse)
{-# DEPRECATED diirsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A list of MediaConnect Flows for this input.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsMediaConnectFlows :: Lens.Lens' DescribeInputResponse (Lude.Maybe [MediaConnectFlow])
diirsMediaConnectFlows = Lens.lens (mediaConnectFlows :: DescribeInputResponse -> Lude.Maybe [MediaConnectFlow]) (\s a -> s {mediaConnectFlows = a} :: DescribeInputResponse)
{-# DEPRECATED diirsMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- /Note:/ Consider using 'inputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsInputSourceType :: Lens.Lens' DescribeInputResponse (Lude.Maybe InputSourceType)
diirsInputSourceType = Lens.lens (inputSourceType :: DescribeInputResponse -> Lude.Maybe InputSourceType) (\s a -> s {inputSourceType = a} :: DescribeInputResponse)
{-# DEPRECATED diirsInputSourceType "Use generic-lens or generic-optics with 'inputSourceType' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsTags :: Lens.Lens' DescribeInputResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
diirsTags = Lens.lens (tags :: DescribeInputResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeInputResponse)
{-# DEPRECATED diirsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsRoleARN :: Lens.Lens' DescribeInputResponse (Lude.Maybe Lude.Text)
diirsRoleARN = Lens.lens (roleARN :: DescribeInputResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeInputResponse)
{-# DEPRECATED diirsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsResponseStatus :: Lens.Lens' DescribeInputResponse Lude.Int
diirsResponseStatus = Lens.lens (responseStatus :: DescribeInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputResponse)
{-# DEPRECATED diirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
