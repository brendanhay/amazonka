{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AddressConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AddressConfiguration
  ( AddressConfiguration (..),

    -- * Smart constructor
    mkAddressConfiguration,

    -- * Lenses
    acSubstitutions,
    acTitleOverride,
    acContext,
    acRawContent,
    acBodyOverride,
    acChannelType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import qualified Network.AWS.Prelude as Lude

-- | Specifies address-based configuration settings for a message that's sent directly to an endpoint.
--
-- /See:/ 'mkAddressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    titleOverride :: Lude.Maybe Lude.Text,
    context ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    rawContent :: Lude.Maybe Lude.Text,
    bodyOverride :: Lude.Maybe Lude.Text,
    channelType :: Lude.Maybe ChannelType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddressConfiguration' with the minimum fields required to make a request.
--
-- * 'bodyOverride' - The message body to use instead of the default message body. This value overrides the default message body.
-- * 'channelType' - The channel to use when sending the message.
-- * 'context' - An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
-- * 'substitutions' - A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
-- * 'titleOverride' - The message title to use instead of the default message title. This value overrides the default message title.
mkAddressConfiguration ::
  AddressConfiguration
mkAddressConfiguration =
  AddressConfiguration'
    { substitutions = Lude.Nothing,
      titleOverride = Lude.Nothing,
      context = Lude.Nothing,
      rawContent = Lude.Nothing,
      bodyOverride = Lude.Nothing,
      channelType = Lude.Nothing
    }

-- | A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSubstitutions :: Lens.Lens' AddressConfiguration (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
acSubstitutions = Lens.lens (substitutions :: AddressConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: AddressConfiguration)
{-# DEPRECATED acSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The message title to use instead of the default message title. This value overrides the default message title.
--
-- /Note:/ Consider using 'titleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTitleOverride :: Lens.Lens' AddressConfiguration (Lude.Maybe Lude.Text)
acTitleOverride = Lens.lens (titleOverride :: AddressConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {titleOverride = a} :: AddressConfiguration)
{-# DEPRECATED acTitleOverride "Use generic-lens or generic-optics with 'titleOverride' instead." #-}

-- | An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acContext :: Lens.Lens' AddressConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
acContext = Lens.lens (context :: AddressConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {context = a} :: AddressConfiguration)
{-# DEPRECATED acContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRawContent :: Lens.Lens' AddressConfiguration (Lude.Maybe Lude.Text)
acRawContent = Lens.lens (rawContent :: AddressConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: AddressConfiguration)
{-# DEPRECATED acRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The message body to use instead of the default message body. This value overrides the default message body.
--
-- /Note:/ Consider using 'bodyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acBodyOverride :: Lens.Lens' AddressConfiguration (Lude.Maybe Lude.Text)
acBodyOverride = Lens.lens (bodyOverride :: AddressConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {bodyOverride = a} :: AddressConfiguration)
{-# DEPRECATED acBodyOverride "Use generic-lens or generic-optics with 'bodyOverride' instead." #-}

-- | The channel to use when sending the message.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acChannelType :: Lens.Lens' AddressConfiguration (Lude.Maybe ChannelType)
acChannelType = Lens.lens (channelType :: AddressConfiguration -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: AddressConfiguration)
{-# DEPRECATED acChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.ToJSON AddressConfiguration where
  toJSON AddressConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("TitleOverride" Lude..=) Lude.<$> titleOverride,
            ("Context" Lude..=) Lude.<$> context,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("BodyOverride" Lude..=) Lude.<$> bodyOverride,
            ("ChannelType" Lude..=) Lude.<$> channelType
          ]
      )
