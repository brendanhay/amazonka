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
-- Module      : Amazonka.Inspector2.Types.NetworkReachabilityDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.NetworkReachabilityDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.NetworkPath
import Amazonka.Inspector2.Types.NetworkProtocol
import Amazonka.Inspector2.Types.PortRange
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of a network reachability finding.
--
-- /See:/ 'newNetworkReachabilityDetails' smart constructor.
data NetworkReachabilityDetails = NetworkReachabilityDetails'
  { -- | An object that contains details about a network path associated with a
    -- finding.
    networkPath :: NetworkPath,
    -- | An object that contains details about the open port range associated
    -- with a finding.
    openPortRange :: PortRange,
    -- | The protocol associated with a finding.
    protocol :: NetworkProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkReachabilityDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkPath', 'networkReachabilityDetails_networkPath' - An object that contains details about a network path associated with a
-- finding.
--
-- 'openPortRange', 'networkReachabilityDetails_openPortRange' - An object that contains details about the open port range associated
-- with a finding.
--
-- 'protocol', 'networkReachabilityDetails_protocol' - The protocol associated with a finding.
newNetworkReachabilityDetails ::
  -- | 'networkPath'
  NetworkPath ->
  -- | 'openPortRange'
  PortRange ->
  -- | 'protocol'
  NetworkProtocol ->
  NetworkReachabilityDetails
newNetworkReachabilityDetails
  pNetworkPath_
  pOpenPortRange_
  pProtocol_ =
    NetworkReachabilityDetails'
      { networkPath =
          pNetworkPath_,
        openPortRange = pOpenPortRange_,
        protocol = pProtocol_
      }

-- | An object that contains details about a network path associated with a
-- finding.
networkReachabilityDetails_networkPath :: Lens.Lens' NetworkReachabilityDetails NetworkPath
networkReachabilityDetails_networkPath = Lens.lens (\NetworkReachabilityDetails' {networkPath} -> networkPath) (\s@NetworkReachabilityDetails' {} a -> s {networkPath = a} :: NetworkReachabilityDetails)

-- | An object that contains details about the open port range associated
-- with a finding.
networkReachabilityDetails_openPortRange :: Lens.Lens' NetworkReachabilityDetails PortRange
networkReachabilityDetails_openPortRange = Lens.lens (\NetworkReachabilityDetails' {openPortRange} -> openPortRange) (\s@NetworkReachabilityDetails' {} a -> s {openPortRange = a} :: NetworkReachabilityDetails)

-- | The protocol associated with a finding.
networkReachabilityDetails_protocol :: Lens.Lens' NetworkReachabilityDetails NetworkProtocol
networkReachabilityDetails_protocol = Lens.lens (\NetworkReachabilityDetails' {protocol} -> protocol) (\s@NetworkReachabilityDetails' {} a -> s {protocol = a} :: NetworkReachabilityDetails)

instance Core.FromJSON NetworkReachabilityDetails where
  parseJSON =
    Core.withObject
      "NetworkReachabilityDetails"
      ( \x ->
          NetworkReachabilityDetails'
            Prelude.<$> (x Core..: "networkPath")
            Prelude.<*> (x Core..: "openPortRange")
            Prelude.<*> (x Core..: "protocol")
      )

instance Prelude.Hashable NetworkReachabilityDetails where
  hashWithSalt _salt NetworkReachabilityDetails' {..} =
    _salt `Prelude.hashWithSalt` networkPath
      `Prelude.hashWithSalt` openPortRange
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData NetworkReachabilityDetails where
  rnf NetworkReachabilityDetails' {..} =
    Prelude.rnf networkPath
      `Prelude.seq` Prelude.rnf openPortRange
      `Prelude.seq` Prelude.rnf protocol
