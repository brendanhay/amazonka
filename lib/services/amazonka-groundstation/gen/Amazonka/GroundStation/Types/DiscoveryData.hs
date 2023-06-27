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
-- Module      : Amazonka.GroundStation.Types.DiscoveryData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.DiscoveryData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data for agent discovery.
--
-- /See:/ 'newDiscoveryData' smart constructor.
data DiscoveryData = DiscoveryData'
  { -- | List of capabilities to associate with agent.
    capabilityArns :: Prelude.NonEmpty Prelude.Text,
    -- | List of private IP addresses to associate with agent.
    privateIpAddresses :: Prelude.NonEmpty Prelude.Text,
    -- | List of public IP addresses to associate with agent.
    publicIpAddresses :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoveryData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilityArns', 'discoveryData_capabilityArns' - List of capabilities to associate with agent.
--
-- 'privateIpAddresses', 'discoveryData_privateIpAddresses' - List of private IP addresses to associate with agent.
--
-- 'publicIpAddresses', 'discoveryData_publicIpAddresses' - List of public IP addresses to associate with agent.
newDiscoveryData ::
  -- | 'capabilityArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'privateIpAddresses'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'publicIpAddresses'
  Prelude.NonEmpty Prelude.Text ->
  DiscoveryData
newDiscoveryData
  pCapabilityArns_
  pPrivateIpAddresses_
  pPublicIpAddresses_ =
    DiscoveryData'
      { capabilityArns =
          Lens.coerced Lens.# pCapabilityArns_,
        privateIpAddresses =
          Lens.coerced Lens.# pPrivateIpAddresses_,
        publicIpAddresses =
          Lens.coerced Lens.# pPublicIpAddresses_
      }

-- | List of capabilities to associate with agent.
discoveryData_capabilityArns :: Lens.Lens' DiscoveryData (Prelude.NonEmpty Prelude.Text)
discoveryData_capabilityArns = Lens.lens (\DiscoveryData' {capabilityArns} -> capabilityArns) (\s@DiscoveryData' {} a -> s {capabilityArns = a} :: DiscoveryData) Prelude.. Lens.coerced

-- | List of private IP addresses to associate with agent.
discoveryData_privateIpAddresses :: Lens.Lens' DiscoveryData (Prelude.NonEmpty Prelude.Text)
discoveryData_privateIpAddresses = Lens.lens (\DiscoveryData' {privateIpAddresses} -> privateIpAddresses) (\s@DiscoveryData' {} a -> s {privateIpAddresses = a} :: DiscoveryData) Prelude.. Lens.coerced

-- | List of public IP addresses to associate with agent.
discoveryData_publicIpAddresses :: Lens.Lens' DiscoveryData (Prelude.NonEmpty Prelude.Text)
discoveryData_publicIpAddresses = Lens.lens (\DiscoveryData' {publicIpAddresses} -> publicIpAddresses) (\s@DiscoveryData' {} a -> s {publicIpAddresses = a} :: DiscoveryData) Prelude.. Lens.coerced

instance Prelude.Hashable DiscoveryData where
  hashWithSalt _salt DiscoveryData' {..} =
    _salt
      `Prelude.hashWithSalt` capabilityArns
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` publicIpAddresses

instance Prelude.NFData DiscoveryData where
  rnf DiscoveryData' {..} =
    Prelude.rnf capabilityArns
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf publicIpAddresses

instance Data.ToJSON DiscoveryData where
  toJSON DiscoveryData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("capabilityArns" Data..= capabilityArns),
            Prelude.Just
              ("privateIpAddresses" Data..= privateIpAddresses),
            Prelude.Just
              ("publicIpAddresses" Data..= publicIpAddresses)
          ]
      )
