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
-- Module      : Amazonka.NetworkManager.Types.SiteToSiteVpnAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.SiteToSiteVpnAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.Attachment
import qualified Amazonka.Prelude as Prelude

-- | Creates a site-to-site VPN attachment.
--
-- /See:/ 'newSiteToSiteVpnAttachment' smart constructor.
data SiteToSiteVpnAttachment = SiteToSiteVpnAttachment'
  { -- | Provides details about a site-to-site VPN attachment.
    attachment :: Prelude.Maybe Attachment,
    -- | The ARN of the site-to-site VPN attachment.
    vpnConnectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SiteToSiteVpnAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'siteToSiteVpnAttachment_attachment' - Provides details about a site-to-site VPN attachment.
--
-- 'vpnConnectionArn', 'siteToSiteVpnAttachment_vpnConnectionArn' - The ARN of the site-to-site VPN attachment.
newSiteToSiteVpnAttachment ::
  SiteToSiteVpnAttachment
newSiteToSiteVpnAttachment =
  SiteToSiteVpnAttachment'
    { attachment =
        Prelude.Nothing,
      vpnConnectionArn = Prelude.Nothing
    }

-- | Provides details about a site-to-site VPN attachment.
siteToSiteVpnAttachment_attachment :: Lens.Lens' SiteToSiteVpnAttachment (Prelude.Maybe Attachment)
siteToSiteVpnAttachment_attachment = Lens.lens (\SiteToSiteVpnAttachment' {attachment} -> attachment) (\s@SiteToSiteVpnAttachment' {} a -> s {attachment = a} :: SiteToSiteVpnAttachment)

-- | The ARN of the site-to-site VPN attachment.
siteToSiteVpnAttachment_vpnConnectionArn :: Lens.Lens' SiteToSiteVpnAttachment (Prelude.Maybe Prelude.Text)
siteToSiteVpnAttachment_vpnConnectionArn = Lens.lens (\SiteToSiteVpnAttachment' {vpnConnectionArn} -> vpnConnectionArn) (\s@SiteToSiteVpnAttachment' {} a -> s {vpnConnectionArn = a} :: SiteToSiteVpnAttachment)

instance Data.FromJSON SiteToSiteVpnAttachment where
  parseJSON =
    Data.withObject
      "SiteToSiteVpnAttachment"
      ( \x ->
          SiteToSiteVpnAttachment'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "VpnConnectionArn")
      )

instance Prelude.Hashable SiteToSiteVpnAttachment where
  hashWithSalt _salt SiteToSiteVpnAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` vpnConnectionArn

instance Prelude.NFData SiteToSiteVpnAttachment where
  rnf SiteToSiteVpnAttachment' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf vpnConnectionArn
