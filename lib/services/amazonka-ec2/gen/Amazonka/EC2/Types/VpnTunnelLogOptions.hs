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
-- Module      : Amazonka.EC2.Types.VpnTunnelLogOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnTunnelLogOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CloudWatchLogOptions
import qualified Amazonka.Prelude as Prelude

-- | Options for logging VPN tunnel activity.
--
-- /See:/ 'newVpnTunnelLogOptions' smart constructor.
data VpnTunnelLogOptions = VpnTunnelLogOptions'
  { -- | Options for sending VPN tunnel logs to CloudWatch.
    cloudWatchLogOptions :: Prelude.Maybe CloudWatchLogOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnTunnelLogOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogOptions', 'vpnTunnelLogOptions_cloudWatchLogOptions' - Options for sending VPN tunnel logs to CloudWatch.
newVpnTunnelLogOptions ::
  VpnTunnelLogOptions
newVpnTunnelLogOptions =
  VpnTunnelLogOptions'
    { cloudWatchLogOptions =
        Prelude.Nothing
    }

-- | Options for sending VPN tunnel logs to CloudWatch.
vpnTunnelLogOptions_cloudWatchLogOptions :: Lens.Lens' VpnTunnelLogOptions (Prelude.Maybe CloudWatchLogOptions)
vpnTunnelLogOptions_cloudWatchLogOptions = Lens.lens (\VpnTunnelLogOptions' {cloudWatchLogOptions} -> cloudWatchLogOptions) (\s@VpnTunnelLogOptions' {} a -> s {cloudWatchLogOptions = a} :: VpnTunnelLogOptions)

instance Data.FromXML VpnTunnelLogOptions where
  parseXML x =
    VpnTunnelLogOptions'
      Prelude.<$> (x Data..@? "cloudWatchLogOptions")

instance Prelude.Hashable VpnTunnelLogOptions where
  hashWithSalt _salt VpnTunnelLogOptions' {..} =
    _salt `Prelude.hashWithSalt` cloudWatchLogOptions

instance Prelude.NFData VpnTunnelLogOptions where
  rnf VpnTunnelLogOptions' {..} =
    Prelude.rnf cloudWatchLogOptions
