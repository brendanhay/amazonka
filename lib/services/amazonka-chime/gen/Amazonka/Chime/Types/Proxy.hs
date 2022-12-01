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
-- Module      : Amazonka.Chime.Types.Proxy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Proxy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The proxy configuration for an Amazon Chime Voice Connector.
--
-- /See:/ 'newProxy' smart constructor.
data Proxy = Proxy'
  { -- | The countries for proxy phone numbers to be selected from.
    phoneNumberCountries :: Prelude.Maybe [Prelude.Text],
    -- | The default number of minutes allowed for proxy sessions.
    defaultSessionExpiryMinutes :: Prelude.Maybe Prelude.Int,
    -- | The phone number to route calls to after a proxy session expires.
    fallBackPhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | When true, stops proxy sessions from being created on the specified
    -- Amazon Chime Voice Connector.
    disabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Proxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberCountries', 'proxy_phoneNumberCountries' - The countries for proxy phone numbers to be selected from.
--
-- 'defaultSessionExpiryMinutes', 'proxy_defaultSessionExpiryMinutes' - The default number of minutes allowed for proxy sessions.
--
-- 'fallBackPhoneNumber', 'proxy_fallBackPhoneNumber' - The phone number to route calls to after a proxy session expires.
--
-- 'disabled', 'proxy_disabled' - When true, stops proxy sessions from being created on the specified
-- Amazon Chime Voice Connector.
newProxy ::
  Proxy
newProxy =
  Proxy'
    { phoneNumberCountries = Prelude.Nothing,
      defaultSessionExpiryMinutes = Prelude.Nothing,
      fallBackPhoneNumber = Prelude.Nothing,
      disabled = Prelude.Nothing
    }

-- | The countries for proxy phone numbers to be selected from.
proxy_phoneNumberCountries :: Lens.Lens' Proxy (Prelude.Maybe [Prelude.Text])
proxy_phoneNumberCountries = Lens.lens (\Proxy' {phoneNumberCountries} -> phoneNumberCountries) (\s@Proxy' {} a -> s {phoneNumberCountries = a} :: Proxy) Prelude.. Lens.mapping Lens.coerced

-- | The default number of minutes allowed for proxy sessions.
proxy_defaultSessionExpiryMinutes :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Int)
proxy_defaultSessionExpiryMinutes = Lens.lens (\Proxy' {defaultSessionExpiryMinutes} -> defaultSessionExpiryMinutes) (\s@Proxy' {} a -> s {defaultSessionExpiryMinutes = a} :: Proxy)

-- | The phone number to route calls to after a proxy session expires.
proxy_fallBackPhoneNumber :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Text)
proxy_fallBackPhoneNumber = Lens.lens (\Proxy' {fallBackPhoneNumber} -> fallBackPhoneNumber) (\s@Proxy' {} a -> s {fallBackPhoneNumber = a} :: Proxy) Prelude.. Lens.mapping Core._Sensitive

-- | When true, stops proxy sessions from being created on the specified
-- Amazon Chime Voice Connector.
proxy_disabled :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Bool)
proxy_disabled = Lens.lens (\Proxy' {disabled} -> disabled) (\s@Proxy' {} a -> s {disabled = a} :: Proxy)

instance Core.FromJSON Proxy where
  parseJSON =
    Core.withObject
      "Proxy"
      ( \x ->
          Proxy'
            Prelude.<$> ( x Core..:? "PhoneNumberCountries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DefaultSessionExpiryMinutes")
            Prelude.<*> (x Core..:? "FallBackPhoneNumber")
            Prelude.<*> (x Core..:? "Disabled")
      )

instance Prelude.Hashable Proxy where
  hashWithSalt _salt Proxy' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberCountries
      `Prelude.hashWithSalt` defaultSessionExpiryMinutes
      `Prelude.hashWithSalt` fallBackPhoneNumber
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData Proxy where
  rnf Proxy' {..} =
    Prelude.rnf phoneNumberCountries
      `Prelude.seq` Prelude.rnf defaultSessionExpiryMinutes
      `Prelude.seq` Prelude.rnf fallBackPhoneNumber
      `Prelude.seq` Prelude.rnf disabled
