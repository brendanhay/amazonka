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
    escSubstitutions,
    escTitleOverride,
    escContext,
    escRawContent,
    escBodyOverride,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the content, including message variables and attributes, to use in a message that's sent directly to an endpoint.
--
-- /See:/ 'mkEndpointSendConfiguration' smart constructor.
data EndpointSendConfiguration = EndpointSendConfiguration'
  { substitutions ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            ([Lude.Text])
        ),
    titleOverride :: Lude.Maybe Lude.Text,
    context ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    rawContent :: Lude.Maybe Lude.Text,
    bodyOverride :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointSendConfiguration' with the minimum fields required to make a request.
--
-- * 'bodyOverride' - The body of the message. If specified, this value overrides the default message body.
-- * 'context' - A map of custom attributes to attach to the message for the address. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
-- * 'rawContent' - The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
-- * 'substitutions' - A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
-- * 'titleOverride' - The title or subject line of the message. If specified, this value overrides the default message title or subject line.
mkEndpointSendConfiguration ::
  EndpointSendConfiguration
mkEndpointSendConfiguration =
  EndpointSendConfiguration'
    { substitutions = Lude.Nothing,
      titleOverride = Lude.Nothing,
      context = Lude.Nothing,
      rawContent = Lude.Nothing,
      bodyOverride = Lude.Nothing
    }

-- | A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSubstitutions :: Lens.Lens' EndpointSendConfiguration (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
escSubstitutions = Lens.lens (substitutions :: EndpointSendConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: EndpointSendConfiguration)
{-# DEPRECATED escSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The title or subject line of the message. If specified, this value overrides the default message title or subject line.
--
-- /Note:/ Consider using 'titleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escTitleOverride :: Lens.Lens' EndpointSendConfiguration (Lude.Maybe Lude.Text)
escTitleOverride = Lens.lens (titleOverride :: EndpointSendConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {titleOverride = a} :: EndpointSendConfiguration)
{-# DEPRECATED escTitleOverride "Use generic-lens or generic-optics with 'titleOverride' instead." #-}

-- | A map of custom attributes to attach to the message for the address. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escContext :: Lens.Lens' EndpointSendConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
escContext = Lens.lens (context :: EndpointSendConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {context = a} :: EndpointSendConfiguration)
{-# DEPRECATED escContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- /Note:/ Consider using 'rawContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escRawContent :: Lens.Lens' EndpointSendConfiguration (Lude.Maybe Lude.Text)
escRawContent = Lens.lens (rawContent :: EndpointSendConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {rawContent = a} :: EndpointSendConfiguration)
{-# DEPRECATED escRawContent "Use generic-lens or generic-optics with 'rawContent' instead." #-}

-- | The body of the message. If specified, this value overrides the default message body.
--
-- /Note:/ Consider using 'bodyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escBodyOverride :: Lens.Lens' EndpointSendConfiguration (Lude.Maybe Lude.Text)
escBodyOverride = Lens.lens (bodyOverride :: EndpointSendConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {bodyOverride = a} :: EndpointSendConfiguration)
{-# DEPRECATED escBodyOverride "Use generic-lens or generic-optics with 'bodyOverride' instead." #-}

instance Lude.ToJSON EndpointSendConfiguration where
  toJSON EndpointSendConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("TitleOverride" Lude..=) Lude.<$> titleOverride,
            ("Context" Lude..=) Lude.<$> context,
            ("RawContent" Lude..=) Lude.<$> rawContent,
            ("BodyOverride" Lude..=) Lude.<$> bodyOverride
          ]
      )
