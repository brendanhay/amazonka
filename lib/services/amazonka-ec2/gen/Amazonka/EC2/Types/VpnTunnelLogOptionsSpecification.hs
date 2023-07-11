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
-- Module      : Amazonka.EC2.Types.VpnTunnelLogOptionsSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnTunnelLogOptionsSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CloudWatchLogOptionsSpecification
import qualified Amazonka.Prelude as Prelude

-- | Options for logging VPN tunnel activity.
--
-- /See:/ 'newVpnTunnelLogOptionsSpecification' smart constructor.
data VpnTunnelLogOptionsSpecification = VpnTunnelLogOptionsSpecification'
  { -- | Options for sending VPN tunnel logs to CloudWatch.
    cloudWatchLogOptions :: Prelude.Maybe CloudWatchLogOptionsSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnTunnelLogOptionsSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogOptions', 'vpnTunnelLogOptionsSpecification_cloudWatchLogOptions' - Options for sending VPN tunnel logs to CloudWatch.
newVpnTunnelLogOptionsSpecification ::
  VpnTunnelLogOptionsSpecification
newVpnTunnelLogOptionsSpecification =
  VpnTunnelLogOptionsSpecification'
    { cloudWatchLogOptions =
        Prelude.Nothing
    }

-- | Options for sending VPN tunnel logs to CloudWatch.
vpnTunnelLogOptionsSpecification_cloudWatchLogOptions :: Lens.Lens' VpnTunnelLogOptionsSpecification (Prelude.Maybe CloudWatchLogOptionsSpecification)
vpnTunnelLogOptionsSpecification_cloudWatchLogOptions = Lens.lens (\VpnTunnelLogOptionsSpecification' {cloudWatchLogOptions} -> cloudWatchLogOptions) (\s@VpnTunnelLogOptionsSpecification' {} a -> s {cloudWatchLogOptions = a} :: VpnTunnelLogOptionsSpecification)

instance
  Prelude.Hashable
    VpnTunnelLogOptionsSpecification
  where
  hashWithSalt
    _salt
    VpnTunnelLogOptionsSpecification' {..} =
      _salt `Prelude.hashWithSalt` cloudWatchLogOptions

instance
  Prelude.NFData
    VpnTunnelLogOptionsSpecification
  where
  rnf VpnTunnelLogOptionsSpecification' {..} =
    Prelude.rnf cloudWatchLogOptions

instance
  Data.ToQuery
    VpnTunnelLogOptionsSpecification
  where
  toQuery VpnTunnelLogOptionsSpecification' {..} =
    Prelude.mconcat
      ["CloudWatchLogOptions" Data.=: cloudWatchLogOptions]
