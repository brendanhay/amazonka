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
    acBodyOverride,
    acChannelType,
    acContext,
    acRawContent,
    acSubstitutions,
    acTitleOverride,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.ChannelType as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies address-based configuration settings for a message that's sent directly to an endpoint.
--
-- /See:/ 'mkAddressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { -- | The message body to use instead of the default message body. This value overrides the default message body.
    bodyOverride :: Core.Maybe Core.Text,
    -- | The channel to use when sending the message.
    channelType :: Core.Maybe Types.ChannelType,
    -- | An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive.
    --
    -- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
    context :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
    rawContent :: Core.Maybe Core.Text,
    -- | A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The message title to use instead of the default message title. This value overrides the default message title.
    titleOverride :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddressConfiguration' value with any optional fields omitted.
mkAddressConfiguration ::
  AddressConfiguration
mkAddressConfiguration =
  AddressConfiguration'
    { bodyOverride = Core.Nothing,
      channelType = Core.Nothing,
      context = Core.Nothing,
      rawContent = Core.Nothing,
      substitutions = Core.Nothing,
      titleOverride = Core.Nothing
    }

-- | The message body to use instead of the default message body. This value overrides the default message body.
--
-- /Note:/ Consider using 'bodyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acBodyOverride :: Lens.Lens' AddressConfiguration (Core.Maybe Core.Text)
acBodyOverride = Lens.field @"bodyOverride"
{-# DEPRECATED acBodyOverride "Use generic-lens or generic-optics with 'bodyOverride' instead." #-}

-- | The channel to use when sending the message.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acChannelType :: Lens.Lens' AddressConfiguration (Core.Maybe Types.ChannelType)
acChannelType = Lens.field @"channelType"
{-# DEPRECATED acChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

-- | An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acContext :: Lens.Lens' AddressConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
acContext = Lens.field @"context"
{-# DEPRECATED acContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRawContent :: Lens.Lens' AddressConfiguration (Core.Maybe Core.Text)
acRawContent = Lens.field @"rawContent"
{-# DEPRECATED acRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSubstitutions :: Lens.Lens' AddressConfiguration (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
acSubstitutions = Lens.field @"substitutions"
{-# DEPRECATED acSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The message title to use instead of the default message title. This value overrides the default message title.
--
-- /Note:/ Consider using 'titleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTitleOverride :: Lens.Lens' AddressConfiguration (Core.Maybe Core.Text)
acTitleOverride = Lens.field @"titleOverride"
{-# DEPRECATED acTitleOverride "Use generic-lens or generic-optics with 'titleOverride' instead." #-}

instance Core.FromJSON AddressConfiguration where
  toJSON AddressConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("BodyOverride" Core..=) Core.<$> bodyOverride,
            ("ChannelType" Core..=) Core.<$> channelType,
            ("Context" Core..=) Core.<$> context,
            ("RawContent" Core..=) Core.<$> rawContent,
            ("Substitutions" Core..=) Core.<$> substitutions,
            ("TitleOverride" Core..=) Core.<$> titleOverride
          ]
      )
