-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Input
  ( Input (..),

    -- * Smart constructor
    mkInput,

    -- * Lenses
    iState,
    iSecurityGroups,
    iARN,
    iInputDevices,
    iSources,
    iDestinations,
    iName,
    iAttachedChannels,
    iId,
    iInputClass,
    iType,
    iMediaConnectFlows,
    iInputSourceType,
    iTags,
    iRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClass
import Network.AWS.MediaLive.Types.InputDestination
import Network.AWS.MediaLive.Types.InputDeviceSettings
import Network.AWS.MediaLive.Types.InputSource
import Network.AWS.MediaLive.Types.InputSourceType
import Network.AWS.MediaLive.Types.InputState
import Network.AWS.MediaLive.Types.InputType
import Network.AWS.MediaLive.Types.MediaConnectFlow
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for Input
--
-- /See:/ 'mkInput' smart constructor.
data Input = Input'
  { state :: Lude.Maybe InputState,
    securityGroups :: Lude.Maybe [Lude.Text],
    arn :: Lude.Maybe Lude.Text,
    inputDevices :: Lude.Maybe [InputDeviceSettings],
    sources :: Lude.Maybe [InputSource],
    destinations :: Lude.Maybe [InputDestination],
    name :: Lude.Maybe Lude.Text,
    attachedChannels :: Lude.Maybe [Lude.Text],
    id :: Lude.Maybe Lude.Text,
    inputClass :: Lude.Maybe InputClass,
    type' :: Lude.Maybe InputType,
    mediaConnectFlows :: Lude.Maybe [MediaConnectFlow],
    inputSourceType :: Lude.Maybe InputSourceType,
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

-- | Creates a value of 'Input' with the minimum fields required to make a request.
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
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
-- * 'securityGroups' - A list of IDs for all the Input Security Groups attached to the input.
-- * 'sources' - A list of the sources of the input (PULL-type).
-- * 'state' - Undocumented field.
-- * 'tags' - A collection of key-value pairs.
-- * 'type'' - Undocumented field.
mkInput ::
  Input
mkInput =
  Input'
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
      roleARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Input (Lude.Maybe InputState)
iState = Lens.lens (state :: Input -> Lude.Maybe InputState) (\s a -> s {state = a} :: Input)
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A list of IDs for all the Input Security Groups attached to the input.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSecurityGroups :: Lens.Lens' Input (Lude.Maybe [Lude.Text])
iSecurityGroups = Lens.lens (securityGroups :: Input -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: Input)
{-# DEPRECATED iSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The Unique ARN of the input (generated, immutable).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iARN :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iARN = Lens.lens (arn :: Input -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Input)
{-# DEPRECATED iARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Settings for the input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputDevices :: Lens.Lens' Input (Lude.Maybe [InputDeviceSettings])
iInputDevices = Lens.lens (inputDevices :: Input -> Lude.Maybe [InputDeviceSettings]) (\s a -> s {inputDevices = a} :: Input)
{-# DEPRECATED iInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A list of the sources of the input (PULL-type).
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSources :: Lens.Lens' Input (Lude.Maybe [InputSource])
iSources = Lens.lens (sources :: Input -> Lude.Maybe [InputSource]) (\s a -> s {sources = a} :: Input)
{-# DEPRECATED iSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | A list of the destinations of the input (PUSH-type).
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iDestinations :: Lens.Lens' Input (Lude.Maybe [InputDestination])
iDestinations = Lens.lens (destinations :: Input -> Lude.Maybe [InputDestination]) (\s a -> s {destinations = a} :: Input)
{-# DEPRECATED iDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The user-assigned name (This is a mutable value).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iName = Lens.lens (name :: Input -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Input)
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- /Note:/ Consider using 'attachedChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAttachedChannels :: Lens.Lens' Input (Lude.Maybe [Lude.Text])
iAttachedChannels = Lens.lens (attachedChannels :: Input -> Lude.Maybe [Lude.Text]) (\s a -> s {attachedChannels = a} :: Input)
{-# DEPRECATED iAttachedChannels "Use generic-lens or generic-optics with 'attachedChannels' instead." #-}

-- | The generated ID of the input (unique for user account, immutable).
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iId = Lens.lens (id :: Input -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Input)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails.
--
-- SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- /Note:/ Consider using 'inputClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputClass :: Lens.Lens' Input (Lude.Maybe InputClass)
iInputClass = Lens.lens (inputClass :: Input -> Lude.Maybe InputClass) (\s a -> s {inputClass = a} :: Input)
{-# DEPRECATED iInputClass "Use generic-lens or generic-optics with 'inputClass' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Input (Lude.Maybe InputType)
iType = Lens.lens (type' :: Input -> Lude.Maybe InputType) (\s a -> s {type' = a} :: Input)
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A list of MediaConnect Flows for this input.
--
-- /Note:/ Consider using 'mediaConnectFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMediaConnectFlows :: Lens.Lens' Input (Lude.Maybe [MediaConnectFlow])
iMediaConnectFlows = Lens.lens (mediaConnectFlows :: Input -> Lude.Maybe [MediaConnectFlow]) (\s a -> s {mediaConnectFlows = a} :: Input)
{-# DEPRECATED iMediaConnectFlows "Use generic-lens or generic-optics with 'mediaConnectFlows' instead." #-}

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes
--
-- during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- /Note:/ Consider using 'inputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInputSourceType :: Lens.Lens' Input (Lude.Maybe InputSourceType)
iInputSourceType = Lens.lens (inputSourceType :: Input -> Lude.Maybe InputSourceType) (\s a -> s {inputSourceType = a} :: Input)
{-# DEPRECATED iInputSourceType "Use generic-lens or generic-optics with 'inputSourceType' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Input (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
iTags = Lens.lens (tags :: Input -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Input)
{-# DEPRECATED iTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRoleARN :: Lens.Lens' Input (Lude.Maybe Lude.Text)
iRoleARN = Lens.lens (roleARN :: Input -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Input)
{-# DEPRECATED iRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON Input where
  parseJSON =
    Lude.withObject
      "Input"
      ( \x ->
          Input'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "securityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "inputDevices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "attachedChannels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "inputClass")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "mediaConnectFlows" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inputSourceType")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
      )
