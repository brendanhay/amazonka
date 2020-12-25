{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointSendConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointSendConfiguration
  ( EndpointSendConfiguration (..),

    -- * Smart constructor
    mkEndpointSendConfiguration,

    -- * Lenses
    escBodyOverride,
    escContext,
    escRawContent,
    escSubstitutions,
    escTitleOverride,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the content, including message variables and attributes, to use in a message that's sent directly to an endpoint.
--
-- /See:/ 'mkEndpointSendConfiguration' smart constructor.
data EndpointSendConfiguration = EndpointSendConfiguration'
  { -- | The body of the message. If specified, this value overrides the default message body.
    bodyOverride :: Core.Maybe Core.Text,
    -- | A map of custom attributes to attach to the message for the address. Attribute names are case sensitive.
    --
    -- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
    context :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
    rawContent :: Core.Maybe Core.Text,
    -- | A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The title or subject line of the message. If specified, this value overrides the default message title or subject line.
    titleOverride :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointSendConfiguration' value with any optional fields omitted.
mkEndpointSendConfiguration ::
  EndpointSendConfiguration
mkEndpointSendConfiguration =
  EndpointSendConfiguration'
    { bodyOverride = Core.Nothing,
      context = Core.Nothing,
      rawContent = Core.Nothing,
      substitutions = Core.Nothing,
      titleOverride = Core.Nothing
    }

-- | The body of the message. If specified, this value overrides the default message body.
--
-- /Note:/ Consider using 'bodyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escBodyOverride :: Lens.Lens' EndpointSendConfiguration (Core.Maybe Core.Text)
escBodyOverride = Lens.field @"bodyOverride"
{-# DEPRECATED escBodyOverride "Use generic-lens or generic-optics with 'bodyOverride' instead." #-}

-- | A map of custom attributes to attach to the message for the address. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escContext :: Lens.Lens' EndpointSendConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
escContext = Lens.field @"context"
{-# DEPRECATED escContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escRawContent :: Lens.Lens' EndpointSendConfiguration (Core.Maybe Core.Text)
escRawContent = Lens.field @"rawContent"
{-# DEPRECATED escRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSubstitutions :: Lens.Lens' EndpointSendConfiguration (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
escSubstitutions = Lens.field @"substitutions"
{-# DEPRECATED escSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The title or subject line of the message. If specified, this value overrides the default message title or subject line.
--
-- /Note:/ Consider using 'titleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escTitleOverride :: Lens.Lens' EndpointSendConfiguration (Core.Maybe Core.Text)
escTitleOverride = Lens.field @"titleOverride"
{-# DEPRECATED escTitleOverride "Use generic-lens or generic-optics with 'titleOverride' instead." #-}

instance Core.FromJSON EndpointSendConfiguration where
  toJSON EndpointSendConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("BodyOverride" Core..=) Core.<$> bodyOverride,
            ("Context" Core..=) Core.<$> context,
            ("RawContent" Core..=) Core.<$> rawContent,
            ("Substitutions" Core..=) Core.<$> substitutions,
            ("TitleOverride" Core..=) Core.<$> titleOverride
          ]
      )
