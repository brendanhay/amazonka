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
-- Module      : Amazonka.EC2.Types.ClientLoginBannerOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientLoginBannerOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for enabling a customizable text banner that will be displayed
-- on Amazon Web Services provided clients when a VPN session is
-- established.
--
-- /See:/ 'newClientLoginBannerOptions' smart constructor.
data ClientLoginBannerOptions = ClientLoginBannerOptions'
  { -- | Customizable text that will be displayed in a banner on Amazon Web
    -- Services provided clients when a VPN session is established. UTF-8
    -- encoded characters only. Maximum of 1400 characters.
    bannerText :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable a customizable text banner that will be displayed on
    -- Amazon Web Services provided clients when a VPN session is established.
    --
    -- Valid values: @true | false@
    --
    -- Default value: @false@
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientLoginBannerOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bannerText', 'clientLoginBannerOptions_bannerText' - Customizable text that will be displayed in a banner on Amazon Web
-- Services provided clients when a VPN session is established. UTF-8
-- encoded characters only. Maximum of 1400 characters.
--
-- 'enabled', 'clientLoginBannerOptions_enabled' - Enable or disable a customizable text banner that will be displayed on
-- Amazon Web Services provided clients when a VPN session is established.
--
-- Valid values: @true | false@
--
-- Default value: @false@
newClientLoginBannerOptions ::
  ClientLoginBannerOptions
newClientLoginBannerOptions =
  ClientLoginBannerOptions'
    { bannerText =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Customizable text that will be displayed in a banner on Amazon Web
-- Services provided clients when a VPN session is established. UTF-8
-- encoded characters only. Maximum of 1400 characters.
clientLoginBannerOptions_bannerText :: Lens.Lens' ClientLoginBannerOptions (Prelude.Maybe Prelude.Text)
clientLoginBannerOptions_bannerText = Lens.lens (\ClientLoginBannerOptions' {bannerText} -> bannerText) (\s@ClientLoginBannerOptions' {} a -> s {bannerText = a} :: ClientLoginBannerOptions)

-- | Enable or disable a customizable text banner that will be displayed on
-- Amazon Web Services provided clients when a VPN session is established.
--
-- Valid values: @true | false@
--
-- Default value: @false@
clientLoginBannerOptions_enabled :: Lens.Lens' ClientLoginBannerOptions (Prelude.Maybe Prelude.Bool)
clientLoginBannerOptions_enabled = Lens.lens (\ClientLoginBannerOptions' {enabled} -> enabled) (\s@ClientLoginBannerOptions' {} a -> s {enabled = a} :: ClientLoginBannerOptions)

instance Prelude.Hashable ClientLoginBannerOptions where
  hashWithSalt _salt ClientLoginBannerOptions' {..} =
    _salt `Prelude.hashWithSalt` bannerText
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ClientLoginBannerOptions where
  rnf ClientLoginBannerOptions' {..} =
    Prelude.rnf bannerText
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToQuery ClientLoginBannerOptions where
  toQuery ClientLoginBannerOptions' {..} =
    Prelude.mconcat
      [ "BannerText" Core.=: bannerText,
        "Enabled" Core.=: enabled
      ]
