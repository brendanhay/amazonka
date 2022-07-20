{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.EndpointSendConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointSendConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the content, including message variables and attributes, to
-- use in a message that\'s sent directly to an endpoint.
--
-- /See:/ 'newEndpointSendConfiguration' smart constructor.
data EndpointSendConfiguration = EndpointSendConfiguration'
  { -- | A map of custom attributes to attach to the message for the address.
    -- Attribute names are case sensitive.
    --
    -- For a push notification, this payload is added to the data.pinpoint
    -- object. For an email or text message, this payload is added to
    -- email\/SMS delivery receipt event attributes.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of the message variables to merge with the variables specified for
    -- the default message (DefaultMessage.Substitutions). The variables
    -- specified in this map take precedence over all other variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The title or subject line of the message. If specified, this value
    -- overrides the default message title or subject line.
    titleOverride :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the message. If
    -- specified, this value overrides all other values for the message.
    rawContent :: Prelude.Maybe Prelude.Text,
    -- | The body of the message. If specified, this value overrides the default
    -- message body.
    bodyOverride :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointSendConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'endpointSendConfiguration_context' - A map of custom attributes to attach to the message for the address.
-- Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint
-- object. For an email or text message, this payload is added to
-- email\/SMS delivery receipt event attributes.
--
-- 'substitutions', 'endpointSendConfiguration_substitutions' - A map of the message variables to merge with the variables specified for
-- the default message (DefaultMessage.Substitutions). The variables
-- specified in this map take precedence over all other variables.
--
-- 'titleOverride', 'endpointSendConfiguration_titleOverride' - The title or subject line of the message. If specified, this value
-- overrides the default message title or subject line.
--
-- 'rawContent', 'endpointSendConfiguration_rawContent' - The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
--
-- 'bodyOverride', 'endpointSendConfiguration_bodyOverride' - The body of the message. If specified, this value overrides the default
-- message body.
newEndpointSendConfiguration ::
  EndpointSendConfiguration
newEndpointSendConfiguration =
  EndpointSendConfiguration'
    { context =
        Prelude.Nothing,
      substitutions = Prelude.Nothing,
      titleOverride = Prelude.Nothing,
      rawContent = Prelude.Nothing,
      bodyOverride = Prelude.Nothing
    }

-- | A map of custom attributes to attach to the message for the address.
-- Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint
-- object. For an email or text message, this payload is added to
-- email\/SMS delivery receipt event attributes.
endpointSendConfiguration_context :: Lens.Lens' EndpointSendConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
endpointSendConfiguration_context = Lens.lens (\EndpointSendConfiguration' {context} -> context) (\s@EndpointSendConfiguration' {} a -> s {context = a} :: EndpointSendConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A map of the message variables to merge with the variables specified for
-- the default message (DefaultMessage.Substitutions). The variables
-- specified in this map take precedence over all other variables.
endpointSendConfiguration_substitutions :: Lens.Lens' EndpointSendConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
endpointSendConfiguration_substitutions = Lens.lens (\EndpointSendConfiguration' {substitutions} -> substitutions) (\s@EndpointSendConfiguration' {} a -> s {substitutions = a} :: EndpointSendConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The title or subject line of the message. If specified, this value
-- overrides the default message title or subject line.
endpointSendConfiguration_titleOverride :: Lens.Lens' EndpointSendConfiguration (Prelude.Maybe Prelude.Text)
endpointSendConfiguration_titleOverride = Lens.lens (\EndpointSendConfiguration' {titleOverride} -> titleOverride) (\s@EndpointSendConfiguration' {} a -> s {titleOverride = a} :: EndpointSendConfiguration)

-- | The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
endpointSendConfiguration_rawContent :: Lens.Lens' EndpointSendConfiguration (Prelude.Maybe Prelude.Text)
endpointSendConfiguration_rawContent = Lens.lens (\EndpointSendConfiguration' {rawContent} -> rawContent) (\s@EndpointSendConfiguration' {} a -> s {rawContent = a} :: EndpointSendConfiguration)

-- | The body of the message. If specified, this value overrides the default
-- message body.
endpointSendConfiguration_bodyOverride :: Lens.Lens' EndpointSendConfiguration (Prelude.Maybe Prelude.Text)
endpointSendConfiguration_bodyOverride = Lens.lens (\EndpointSendConfiguration' {bodyOverride} -> bodyOverride) (\s@EndpointSendConfiguration' {} a -> s {bodyOverride = a} :: EndpointSendConfiguration)

instance Prelude.Hashable EndpointSendConfiguration where
  hashWithSalt _salt EndpointSendConfiguration' {..} =
    _salt `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` substitutions
      `Prelude.hashWithSalt` titleOverride
      `Prelude.hashWithSalt` rawContent
      `Prelude.hashWithSalt` bodyOverride

instance Prelude.NFData EndpointSendConfiguration where
  rnf EndpointSendConfiguration' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf substitutions
      `Prelude.seq` Prelude.rnf titleOverride
      `Prelude.seq` Prelude.rnf rawContent
      `Prelude.seq` Prelude.rnf bodyOverride

instance Core.ToJSON EndpointSendConfiguration where
  toJSON EndpointSendConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Context" Core..=) Prelude.<$> context,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("TitleOverride" Core..=) Prelude.<$> titleOverride,
            ("RawContent" Core..=) Prelude.<$> rawContent,
            ("BodyOverride" Core..=) Prelude.<$> bodyOverride
          ]
      )
