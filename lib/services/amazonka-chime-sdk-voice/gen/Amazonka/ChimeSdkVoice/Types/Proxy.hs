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
-- Module      : Amazonka.ChimeSdkVoice.Types.Proxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Proxy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newProxy' smart constructor.
data Proxy = Proxy'
  { defaultSessionExpiryMinutes :: Prelude.Maybe Prelude.Int,
    disabled :: Prelude.Maybe Prelude.Bool,
    fallBackPhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    phoneNumberCountries :: Prelude.Maybe [Prelude.Text]
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
-- 'defaultSessionExpiryMinutes', 'proxy_defaultSessionExpiryMinutes' - Undocumented member.
--
-- 'disabled', 'proxy_disabled' - Undocumented member.
--
-- 'fallBackPhoneNumber', 'proxy_fallBackPhoneNumber' - Undocumented member.
--
-- 'phoneNumberCountries', 'proxy_phoneNumberCountries' - Undocumented member.
newProxy ::
  Proxy
newProxy =
  Proxy'
    { defaultSessionExpiryMinutes =
        Prelude.Nothing,
      disabled = Prelude.Nothing,
      fallBackPhoneNumber = Prelude.Nothing,
      phoneNumberCountries = Prelude.Nothing
    }

-- | Undocumented member.
proxy_defaultSessionExpiryMinutes :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Int)
proxy_defaultSessionExpiryMinutes = Lens.lens (\Proxy' {defaultSessionExpiryMinutes} -> defaultSessionExpiryMinutes) (\s@Proxy' {} a -> s {defaultSessionExpiryMinutes = a} :: Proxy)

-- | Undocumented member.
proxy_disabled :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Bool)
proxy_disabled = Lens.lens (\Proxy' {disabled} -> disabled) (\s@Proxy' {} a -> s {disabled = a} :: Proxy)

-- | Undocumented member.
proxy_fallBackPhoneNumber :: Lens.Lens' Proxy (Prelude.Maybe Prelude.Text)
proxy_fallBackPhoneNumber = Lens.lens (\Proxy' {fallBackPhoneNumber} -> fallBackPhoneNumber) (\s@Proxy' {} a -> s {fallBackPhoneNumber = a} :: Proxy) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
proxy_phoneNumberCountries :: Lens.Lens' Proxy (Prelude.Maybe [Prelude.Text])
proxy_phoneNumberCountries = Lens.lens (\Proxy' {phoneNumberCountries} -> phoneNumberCountries) (\s@Proxy' {} a -> s {phoneNumberCountries = a} :: Proxy) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Proxy where
  parseJSON =
    Data.withObject
      "Proxy"
      ( \x ->
          Proxy'
            Prelude.<$> (x Data..:? "DefaultSessionExpiryMinutes")
            Prelude.<*> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "FallBackPhoneNumber")
            Prelude.<*> ( x
                            Data..:? "PhoneNumberCountries"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Proxy where
  hashWithSalt _salt Proxy' {..} =
    _salt
      `Prelude.hashWithSalt` defaultSessionExpiryMinutes
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` fallBackPhoneNumber
      `Prelude.hashWithSalt` phoneNumberCountries

instance Prelude.NFData Proxy where
  rnf Proxy' {..} =
    Prelude.rnf defaultSessionExpiryMinutes
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf fallBackPhoneNumber
      `Prelude.seq` Prelude.rnf phoneNumberCountries
