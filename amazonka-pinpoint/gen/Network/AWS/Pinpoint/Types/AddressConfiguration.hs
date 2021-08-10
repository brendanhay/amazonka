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
-- Module      : Network.AWS.Pinpoint.Types.AddressConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AddressConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ChannelType
import qualified Network.AWS.Prelude as Prelude

-- | Specifies address-based configuration settings for a message that\'s
-- sent directly to an endpoint.
--
-- /See:/ 'newAddressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { -- | The channel to use when sending the message.
    channelType :: Prelude.Maybe ChannelType,
    -- | An object that maps custom attributes to attributes for the address and
    -- is attached to the message. Attribute names are case sensitive.
    --
    -- For a push notification, this payload is added to the data.pinpoint
    -- object. For an email or text message, this payload is added to
    -- email\/SMS delivery receipt event attributes.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of the message variables to merge with the variables specified by
    -- properties of the DefaultMessage object. The variables specified in this
    -- map take precedence over all other variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The message title to use instead of the default message title. This
    -- value overrides the default message title.
    titleOverride :: Prelude.Maybe Prelude.Text,
    -- | The message body to use instead of the default message body. This value
    -- overrides the default message body.
    bodyOverride :: Prelude.Maybe Prelude.Text,
    -- | The raw, JSON-formatted string to use as the payload for the message. If
    -- specified, this value overrides all other values for the message.
    rawContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelType', 'addressConfiguration_channelType' - The channel to use when sending the message.
--
-- 'context', 'addressConfiguration_context' - An object that maps custom attributes to attributes for the address and
-- is attached to the message. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint
-- object. For an email or text message, this payload is added to
-- email\/SMS delivery receipt event attributes.
--
-- 'substitutions', 'addressConfiguration_substitutions' - A map of the message variables to merge with the variables specified by
-- properties of the DefaultMessage object. The variables specified in this
-- map take precedence over all other variables.
--
-- 'titleOverride', 'addressConfiguration_titleOverride' - The message title to use instead of the default message title. This
-- value overrides the default message title.
--
-- 'bodyOverride', 'addressConfiguration_bodyOverride' - The message body to use instead of the default message body. This value
-- overrides the default message body.
--
-- 'rawContent', 'addressConfiguration_rawContent' - The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
newAddressConfiguration ::
  AddressConfiguration
newAddressConfiguration =
  AddressConfiguration'
    { channelType =
        Prelude.Nothing,
      context = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      titleOverride = Prelude.Nothing,
      bodyOverride = Prelude.Nothing,
      rawContent = Prelude.Nothing
    }

-- | The channel to use when sending the message.
addressConfiguration_channelType :: Lens.Lens' AddressConfiguration (Prelude.Maybe ChannelType)
addressConfiguration_channelType = Lens.lens (\AddressConfiguration' {channelType} -> channelType) (\s@AddressConfiguration' {} a -> s {channelType = a} :: AddressConfiguration)

-- | An object that maps custom attributes to attributes for the address and
-- is attached to the message. Attribute names are case sensitive.
--
-- For a push notification, this payload is added to the data.pinpoint
-- object. For an email or text message, this payload is added to
-- email\/SMS delivery receipt event attributes.
addressConfiguration_context :: Lens.Lens' AddressConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
addressConfiguration_context = Lens.lens (\AddressConfiguration' {context} -> context) (\s@AddressConfiguration' {} a -> s {context = a} :: AddressConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | A map of the message variables to merge with the variables specified by
-- properties of the DefaultMessage object. The variables specified in this
-- map take precedence over all other variables.
addressConfiguration_substitutions :: Lens.Lens' AddressConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
addressConfiguration_substitutions = Lens.lens (\AddressConfiguration' {substitutions} -> substitutions) (\s@AddressConfiguration' {} a -> s {substitutions = a} :: AddressConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | The message title to use instead of the default message title. This
-- value overrides the default message title.
addressConfiguration_titleOverride :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_titleOverride = Lens.lens (\AddressConfiguration' {titleOverride} -> titleOverride) (\s@AddressConfiguration' {} a -> s {titleOverride = a} :: AddressConfiguration)

-- | The message body to use instead of the default message body. This value
-- overrides the default message body.
addressConfiguration_bodyOverride :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_bodyOverride = Lens.lens (\AddressConfiguration' {bodyOverride} -> bodyOverride) (\s@AddressConfiguration' {} a -> s {bodyOverride = a} :: AddressConfiguration)

-- | The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
addressConfiguration_rawContent :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_rawContent = Lens.lens (\AddressConfiguration' {rawContent} -> rawContent) (\s@AddressConfiguration' {} a -> s {rawContent = a} :: AddressConfiguration)

instance Prelude.Hashable AddressConfiguration

instance Prelude.NFData AddressConfiguration

instance Core.ToJSON AddressConfiguration where
  toJSON AddressConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ChannelType" Core..=) Prelude.<$> channelType,
            ("Context" Core..=) Prelude.<$> context,
            ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("TitleOverride" Core..=) Prelude.<$> titleOverride,
            ("BodyOverride" Core..=) Prelude.<$> bodyOverride,
            ("RawContent" Core..=) Prelude.<$> rawContent
          ]
      )
