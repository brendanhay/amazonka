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
-- Module      : Amazonka.Pinpoint.Types.AddressConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.AddressConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ChannelType
import qualified Amazonka.Prelude as Prelude

-- | Specifies address-based configuration settings for a message that\'s
-- sent directly to an endpoint.
--
-- /See:/ 'newAddressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { -- | The message body to use instead of the default message body. This value
    -- overrides the default message body.
    bodyOverride :: Prelude.Maybe Prelude.Text,
    -- | The channel to use when sending the message.
    channelType :: Prelude.Maybe ChannelType,
    -- | An object that maps custom attributes to attributes for the address and
    -- is attached to the message. Attribute names are case sensitive.
    --
    -- For a push notification, this payload is added to the data.pinpoint
    -- object. For an email or text message, this payload is added to
    -- email\/SMS delivery receipt event attributes.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The raw, JSON-formatted string to use as the payload for the message. If
    -- specified, this value overrides all other values for the message.
    rawContent :: Prelude.Maybe Prelude.Text,
    -- | A map of the message variables to merge with the variables specified by
    -- properties of the DefaultMessage object. The variables specified in this
    -- map take precedence over all other variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The message title to use instead of the default message title. This
    -- value overrides the default message title.
    titleOverride :: Prelude.Maybe Prelude.Text
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
-- 'bodyOverride', 'addressConfiguration_bodyOverride' - The message body to use instead of the default message body. This value
-- overrides the default message body.
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
-- 'rawContent', 'addressConfiguration_rawContent' - The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
--
-- 'substitutions', 'addressConfiguration_substitutions' - A map of the message variables to merge with the variables specified by
-- properties of the DefaultMessage object. The variables specified in this
-- map take precedence over all other variables.
--
-- 'titleOverride', 'addressConfiguration_titleOverride' - The message title to use instead of the default message title. This
-- value overrides the default message title.
newAddressConfiguration ::
  AddressConfiguration
newAddressConfiguration =
  AddressConfiguration'
    { bodyOverride =
        Prelude.Nothing,
      channelType = Prelude.Nothing,
      context = Prelude.Nothing,
      rawContent = Prelude.Nothing,
      substitutions = Prelude.Nothing,
      titleOverride = Prelude.Nothing
    }

-- | The message body to use instead of the default message body. This value
-- overrides the default message body.
addressConfiguration_bodyOverride :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_bodyOverride = Lens.lens (\AddressConfiguration' {bodyOverride} -> bodyOverride) (\s@AddressConfiguration' {} a -> s {bodyOverride = a} :: AddressConfiguration)

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
addressConfiguration_context = Lens.lens (\AddressConfiguration' {context} -> context) (\s@AddressConfiguration' {} a -> s {context = a} :: AddressConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The raw, JSON-formatted string to use as the payload for the message. If
-- specified, this value overrides all other values for the message.
addressConfiguration_rawContent :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_rawContent = Lens.lens (\AddressConfiguration' {rawContent} -> rawContent) (\s@AddressConfiguration' {} a -> s {rawContent = a} :: AddressConfiguration)

-- | A map of the message variables to merge with the variables specified by
-- properties of the DefaultMessage object. The variables specified in this
-- map take precedence over all other variables.
addressConfiguration_substitutions :: Lens.Lens' AddressConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
addressConfiguration_substitutions = Lens.lens (\AddressConfiguration' {substitutions} -> substitutions) (\s@AddressConfiguration' {} a -> s {substitutions = a} :: AddressConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The message title to use instead of the default message title. This
-- value overrides the default message title.
addressConfiguration_titleOverride :: Lens.Lens' AddressConfiguration (Prelude.Maybe Prelude.Text)
addressConfiguration_titleOverride = Lens.lens (\AddressConfiguration' {titleOverride} -> titleOverride) (\s@AddressConfiguration' {} a -> s {titleOverride = a} :: AddressConfiguration)

instance Prelude.Hashable AddressConfiguration where
  hashWithSalt _salt AddressConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` bodyOverride
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` rawContent
      `Prelude.hashWithSalt` substitutions
      `Prelude.hashWithSalt` titleOverride

instance Prelude.NFData AddressConfiguration where
  rnf AddressConfiguration' {..} =
    Prelude.rnf bodyOverride `Prelude.seq`
      Prelude.rnf channelType `Prelude.seq`
        Prelude.rnf context `Prelude.seq`
          Prelude.rnf rawContent `Prelude.seq`
            Prelude.rnf substitutions `Prelude.seq`
              Prelude.rnf titleOverride

instance Data.ToJSON AddressConfiguration where
  toJSON AddressConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BodyOverride" Data..=) Prelude.<$> bodyOverride,
            ("ChannelType" Data..=) Prelude.<$> channelType,
            ("Context" Data..=) Prelude.<$> context,
            ("RawContent" Data..=) Prelude.<$> rawContent,
            ("Substitutions" Data..=) Prelude.<$> substitutions,
            ("TitleOverride" Data..=) Prelude.<$> titleOverride
          ]
      )
