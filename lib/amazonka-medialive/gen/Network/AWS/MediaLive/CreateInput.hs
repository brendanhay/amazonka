{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ciRequestId,
    ciInputDevices,
    ciSources,
    ciInputSecurityGroups,
    ciDestinations,
    ciName,
    ciVPC,
    ciType,
    ciMediaConnectFlows,
    ciTags,
    ciRoleARN,

    -- * Destructuring the response
    CreateInputResponse (..),
    mkCreateInputResponse,

    -- ** Response lenses
    cirsInput,
    cirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The name of the input
--
-- /See:/ 'mkCreateInput' smart constructor.
data CreateInput = CreateInput'
  { requestId :: Lude.Maybe Lude.Text,
    inputDevices :: Lude.Maybe [InputDeviceSettings],
    sources :: Lude.Maybe [InputSourceRequest],
    inputSecurityGroups :: Lude.Maybe [Lude.Text],
    destinations :: Lude.Maybe [InputDestinationRequest],
    name :: Lude.Maybe Lude.Text,
    vpc :: Lude.Maybe InputVPCRequest,
    type' :: Lude.Maybe InputType,
    mediaConnectFlows :: Lude.Maybe [MediaConnectFlowRequest],
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInput' with the minimum fields required to make a request.
--
-- * 'destinations' - Destination settings for PUSH type inputs.
-- * 'inputDevices' - Settings for the devices.
-- * 'inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
-- * 'mediaConnectFlows' - A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
-- * 'name' - Name of the input.
-- * 'requestId' - Unique identifier of the request to ensure the request is handled
--
-- exactly once in case of retries.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
-- * 'sources' - The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
-- * 'tags' - A collection of key-value pairs.
-- * 'type'' - Undocumented field.
-- * 'vpc' - Undocumented field.
mkCreateInput ::
  CreateInput
mkCreateInput =
  CreateInput'
    { requestId = Lude.Nothing,
      inputDevices = Lude.Nothing,
      sources = Lude.Nothing,
      inputSecurityGroups = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      vpc = Lude.Nothing,
      type' = Lude.Nothing,
      mediaConnectFlows = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Unique identifier of the request to ensure the request is handled
--
-- exactly once in case of retries.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRequestId :: Lens.Lens' CreateInput (Lude.Maybe Lude.Text)
ciRequestId = Lens.lens (requestId :: CreateInput -> Lude.Maybe Lude.Text) (\s a -> s {requestId = a} :: CreateInput)
{-# DEPRECATED ciRequestId "Use generic-lens or generic-optics with 'requestId' instead." #-}

-- | Settings for the devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInputDevices :: Lens.Lens' CreateInput (Lude.Maybe [InputDeviceSettings])
ciInputDevices = Lens.lens (inputDevices :: CreateInput -> Lude.Maybe [InputDeviceSettings]) (\s a -> s {inputDevices = a} :: CreateInput)
{-# DEPRECATED ciInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSources :: Lens.Lens' CreateInput (Lude.Maybe [InputSourceRequest])
ciSources = Lens.lens (sources :: CreateInput -> Lude.Maybe [InputSourceRequest]) (\s a -> s {sources = a} :: CreateInput)
{-# DEPRECATED ciSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A list of security groups referenced by IDs to attach to the input.
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInputSecurityGroups :: Lens.Lens' CreateInput (Lude.Maybe [Lude.Text])
ciInputSecurityGroups = Lens.lens (inputSecurityGroups :: CreateInput -> Lude.Maybe [Lude.Text]) (\s a -> s {inputSecurityGroups = a} :: CreateInput)
{-# DEPRECATED ciInputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead." #-}

-- | Destination settings for PUSH type inputs.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDestinations :: Lens.Lens' CreateInput (Lude.Maybe [InputDestinationRequest])
ciDestinations = Lens.lens (destinations :: CreateInput -> Lude.Maybe [InputDestinationRequest]) (\s a -> s {destinations = a} :: CreateInput)
{-# DEPRECATED ciDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Name of the input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' CreateInput (Lude.Maybe Lude.Text)
ciName = Lens.lens (name :: CreateInput -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateInput)
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVPC :: Lens.Lens' CreateInput (Lude.Maybe InputVPCRequest)
ciVPC = Lens.lens (vpc :: CreateInput -> Lude.Maybe InputVPCRequest) (\s a -> s {vpc = a} :: CreateInput)
{-# DEPRECATED ciVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciType :: Lens.Lens' CreateInput (Lude.Maybe InputType)
ciType = Lens.lens (type' :: CreateInput -> Lude.Maybe InputType) (\s a -> s {type' = a} :: CreateInput)
{-# DEPRECATED ciType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A list of the MediaConnect Flows that you want to use in this input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMediaConnectFlows :: Lens.Lens' CreateInput (Lude.Maybe [MediaConnectFlowRequest])
ciMediaConnectFlows = Lens.lens (mediaConnectFlows :: CreateInput -> Lude.Maybe [MediaConnectFlowRequest]) (\s a -> s {mediaConnectFlows = a} :: CreateInput)
{-# DEPRECATED ciMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ciTags = Lens.lens (tags :: CreateInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateInput)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRoleARN :: Lens.Lens' CreateInput (Lude.Maybe Lude.Text)
ciRoleARN = Lens.lens (roleARN :: CreateInput -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateInput)
{-# DEPRECATED ciRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateInput where
  type Rs CreateInput = CreateInputResponse
  request = Req.postJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInputResponse'
            Lude.<$> (x Lude..?> "input") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInput where
  toJSON CreateInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("requestId" Lude..=) Lude.<$> requestId,
            ("inputDevices" Lude..=) Lude.<$> inputDevices,
            ("sources" Lude..=) Lude.<$> sources,
            ("inputSecurityGroups" Lude..=) Lude.<$> inputSecurityGroups,
            ("destinations" Lude..=) Lude.<$> destinations,
            ("name" Lude..=) Lude.<$> name,
            ("vpc" Lude..=) Lude.<$> vpc,
            ("type" Lude..=) Lude.<$> type',
            ("mediaConnectFlows" Lude..=) Lude.<$> mediaConnectFlows,
            ("tags" Lude..=) Lude.<$> tags,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath CreateInput where
  toPath = Lude.const "/prod/inputs"

instance Lude.ToQuery CreateInput where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for CreateInputResponse
--
-- /See:/ 'mkCreateInputResponse' smart constructor.
data CreateInputResponse = CreateInputResponse'
  { input ::
      Lude.Maybe Input,
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

-- | Creates a value of 'CreateInputResponse' with the minimum fields required to make a request.
--
-- * 'input' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateInputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInputResponse
mkCreateInputResponse pResponseStatus_ =
  CreateInputResponse'
    { input = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsInput :: Lens.Lens' CreateInputResponse (Lude.Maybe Input)
cirsInput = Lens.lens (input :: CreateInputResponse -> Lude.Maybe Input) (\s a -> s {input = a} :: CreateInputResponse)
{-# DEPRECATED cirsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateInputResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInputResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
