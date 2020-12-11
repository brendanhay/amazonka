{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    uiInputDevices,
    uiSources,
    uiInputSecurityGroups,
    uiDestinations,
    uiName,
    uiMediaConnectFlows,
    uiRoleARN,
    uiInputId,

    -- * Destructuring the response
    UpdateInputResponse (..),
    mkUpdateInputResponse,

    -- ** Response lenses
    uirsInput,
    uirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to update an input.
--
-- /See:/ 'mkUpdateInput' smart constructor.
data UpdateInput = UpdateInput'
  { inputDevices ::
      Lude.Maybe [InputDeviceRequest],
    sources :: Lude.Maybe [InputSourceRequest],
    inputSecurityGroups :: Lude.Maybe [Lude.Text],
    destinations :: Lude.Maybe [InputDestinationRequest],
    name :: Lude.Maybe Lude.Text,
    mediaConnectFlows :: Lude.Maybe [MediaConnectFlowRequest],
    roleARN :: Lude.Maybe Lude.Text,
    inputId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInput' with the minimum fields required to make a request.
--
-- * 'destinations' - Destination settings for PUSH type inputs.
-- * 'inputDevices' - Settings for the devices.
-- * 'inputId' - Unique ID of the input.
-- * 'inputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
-- * 'mediaConnectFlows' - A list of the MediaConnect Flow ARNs that you want to use as the source of the input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
-- * 'name' - Name of the input.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
-- * 'sources' - The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
mkUpdateInput ::
  -- | 'inputId'
  Lude.Text ->
  UpdateInput
mkUpdateInput pInputId_ =
  UpdateInput'
    { inputDevices = Lude.Nothing,
      sources = Lude.Nothing,
      inputSecurityGroups = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing,
      mediaConnectFlows = Lude.Nothing,
      roleARN = Lude.Nothing,
      inputId = pInputId_
    }

-- | Settings for the devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputDevices :: Lens.Lens' UpdateInput (Lude.Maybe [InputDeviceRequest])
uiInputDevices = Lens.lens (inputDevices :: UpdateInput -> Lude.Maybe [InputDeviceRequest]) (\s a -> s {inputDevices = a} :: UpdateInput)
{-# DEPRECATED uiInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | The source URLs for a PULL-type input. Every PULL type input needs
--
-- exactly two source URLs for redundancy.
-- Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiSources :: Lens.Lens' UpdateInput (Lude.Maybe [InputSourceRequest])
uiSources = Lens.lens (sources :: UpdateInput -> Lude.Maybe [InputSourceRequest]) (\s a -> s {sources = a} :: UpdateInput)
{-# DEPRECATED uiSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A list of security groups referenced by IDs to attach to the input.
--
-- /Note:/ Consider using 'inputSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputSecurityGroups :: Lens.Lens' UpdateInput (Lude.Maybe [Lude.Text])
uiInputSecurityGroups = Lens.lens (inputSecurityGroups :: UpdateInput -> Lude.Maybe [Lude.Text]) (\s a -> s {inputSecurityGroups = a} :: UpdateInput)
{-# DEPRECATED uiInputSecurityGroups "Use generic-lens or generic-optics with 'inputSecurityGroups' instead." #-}

-- | Destination settings for PUSH type inputs.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiDestinations :: Lens.Lens' UpdateInput (Lude.Maybe [InputDestinationRequest])
uiDestinations = Lens.lens (destinations :: UpdateInput -> Lude.Maybe [InputDestinationRequest]) (\s a -> s {destinations = a} :: UpdateInput)
{-# DEPRECATED uiDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Name of the input.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiName :: Lens.Lens' UpdateInput (Lude.Maybe Lude.Text)
uiName = Lens.lens (name :: UpdateInput -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateInput)
{-# DEPRECATED uiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of the MediaConnect Flow ARNs that you want to use as the source of the input. You can specify as few as one
--
-- Flow and presently, as many as two. The only requirement is when you have more than one is that each Flow is in a
-- separate Availability Zone as this ensures your EML input is redundant to AZ issues.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiMediaConnectFlows :: Lens.Lens' UpdateInput (Lude.Maybe [MediaConnectFlowRequest])
uiMediaConnectFlows = Lens.lens (mediaConnectFlows :: UpdateInput -> Lude.Maybe [MediaConnectFlowRequest]) (\s a -> s {mediaConnectFlows = a} :: UpdateInput)
{-# DEPRECATED uiMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiRoleARN :: Lens.Lens' UpdateInput (Lude.Maybe Lude.Text)
uiRoleARN = Lens.lens (roleARN :: UpdateInput -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateInput)
{-# DEPRECATED uiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Unique ID of the input.
--
-- /Note:/ Consider using 'inputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiInputId :: Lens.Lens' UpdateInput Lude.Text
uiInputId = Lens.lens (inputId :: UpdateInput -> Lude.Text) (\s a -> s {inputId = a} :: UpdateInput)
{-# DEPRECATED uiInputId "Use generic-lens or generic-optics with 'inputId' instead." #-}

instance Lude.AWSRequest UpdateInput where
  type Rs UpdateInput = UpdateInputResponse
  request = Req.putJSON mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateInputResponse'
            Lude.<$> (x Lude..?> "input") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateInput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateInput where
  toJSON UpdateInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputDevices" Lude..=) Lude.<$> inputDevices,
            ("sources" Lude..=) Lude.<$> sources,
            ("inputSecurityGroups" Lude..=) Lude.<$> inputSecurityGroups,
            ("destinations" Lude..=) Lude.<$> destinations,
            ("name" Lude..=) Lude.<$> name,
            ("mediaConnectFlows" Lude..=) Lude.<$> mediaConnectFlows,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateInput where
  toPath UpdateInput' {..} =
    Lude.mconcat ["/prod/inputs/", Lude.toBS inputId]

instance Lude.ToQuery UpdateInput where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for UpdateInputResponse
--
-- /See:/ 'mkUpdateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
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

-- | Creates a value of 'UpdateInputResponse' with the minimum fields required to make a request.
--
-- * 'input' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateInputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateInputResponse
mkUpdateInputResponse pResponseStatus_ =
  UpdateInputResponse'
    { input = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsInput :: Lens.Lens' UpdateInputResponse (Lude.Maybe Input)
uirsInput = Lens.lens (input :: UpdateInputResponse -> Lude.Maybe Input) (\s a -> s {input = a} :: UpdateInputResponse)
{-# DEPRECATED uirsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uirsResponseStatus :: Lens.Lens' UpdateInputResponse Lude.Int
uirsResponseStatus = Lens.lens (responseStatus :: UpdateInputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateInputResponse)
{-# DEPRECATED uirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
