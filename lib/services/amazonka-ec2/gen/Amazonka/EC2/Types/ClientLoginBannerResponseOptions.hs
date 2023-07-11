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
-- Module      : Amazonka.EC2.Types.ClientLoginBannerResponseOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientLoginBannerResponseOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Current state of options for customizable text banner that will be
-- displayed on Amazon Web Services provided clients when a VPN session is
-- established.
--
-- /See:/ 'newClientLoginBannerResponseOptions' smart constructor.
data ClientLoginBannerResponseOptions = ClientLoginBannerResponseOptions'
  { -- | Customizable text that will be displayed in a banner on Amazon Web
    -- Services provided clients when a VPN session is established. UTF-8
    -- encoded characters only. Maximum of 1400 characters.
    bannerText :: Prelude.Maybe Prelude.Text,
    -- | Current state of text banner feature.
    --
    -- Valid values: @true | false@
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientLoginBannerResponseOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bannerText', 'clientLoginBannerResponseOptions_bannerText' - Customizable text that will be displayed in a banner on Amazon Web
-- Services provided clients when a VPN session is established. UTF-8
-- encoded characters only. Maximum of 1400 characters.
--
-- 'enabled', 'clientLoginBannerResponseOptions_enabled' - Current state of text banner feature.
--
-- Valid values: @true | false@
newClientLoginBannerResponseOptions ::
  ClientLoginBannerResponseOptions
newClientLoginBannerResponseOptions =
  ClientLoginBannerResponseOptions'
    { bannerText =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Customizable text that will be displayed in a banner on Amazon Web
-- Services provided clients when a VPN session is established. UTF-8
-- encoded characters only. Maximum of 1400 characters.
clientLoginBannerResponseOptions_bannerText :: Lens.Lens' ClientLoginBannerResponseOptions (Prelude.Maybe Prelude.Text)
clientLoginBannerResponseOptions_bannerText = Lens.lens (\ClientLoginBannerResponseOptions' {bannerText} -> bannerText) (\s@ClientLoginBannerResponseOptions' {} a -> s {bannerText = a} :: ClientLoginBannerResponseOptions)

-- | Current state of text banner feature.
--
-- Valid values: @true | false@
clientLoginBannerResponseOptions_enabled :: Lens.Lens' ClientLoginBannerResponseOptions (Prelude.Maybe Prelude.Bool)
clientLoginBannerResponseOptions_enabled = Lens.lens (\ClientLoginBannerResponseOptions' {enabled} -> enabled) (\s@ClientLoginBannerResponseOptions' {} a -> s {enabled = a} :: ClientLoginBannerResponseOptions)

instance
  Data.FromXML
    ClientLoginBannerResponseOptions
  where
  parseXML x =
    ClientLoginBannerResponseOptions'
      Prelude.<$> (x Data..@? "bannerText")
      Prelude.<*> (x Data..@? "enabled")

instance
  Prelude.Hashable
    ClientLoginBannerResponseOptions
  where
  hashWithSalt
    _salt
    ClientLoginBannerResponseOptions' {..} =
      _salt
        `Prelude.hashWithSalt` bannerText
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    ClientLoginBannerResponseOptions
  where
  rnf ClientLoginBannerResponseOptions' {..} =
    Prelude.rnf bannerText
      `Prelude.seq` Prelude.rnf enabled
