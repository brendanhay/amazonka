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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.CustomRoutingProtocol
import qualified Amazonka.Prelude as Prelude

-- | For a custom routing accelerator, sets the port range and protocol for
-- all endpoints (virtual private cloud subnets) in an endpoint group to
-- accept client traffic on.
--
-- /See:/ 'newCustomRoutingDestinationConfiguration' smart constructor.
data CustomRoutingDestinationConfiguration = CustomRoutingDestinationConfiguration'
  { -- | The first port, inclusive, in the range of ports for the endpoint group
    -- that is associated with a custom routing accelerator.
    fromPort :: Prelude.Natural,
    -- | The last port, inclusive, in the range of ports for the endpoint group
    -- that is associated with a custom routing accelerator.
    toPort :: Prelude.Natural,
    -- | The protocol for the endpoint group that is associated with a custom
    -- routing accelerator. The protocol can be either TCP or UDP.
    protocols :: Prelude.NonEmpty CustomRoutingProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingDestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'customRoutingDestinationConfiguration_fromPort' - The first port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
--
-- 'toPort', 'customRoutingDestinationConfiguration_toPort' - The last port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
--
-- 'protocols', 'customRoutingDestinationConfiguration_protocols' - The protocol for the endpoint group that is associated with a custom
-- routing accelerator. The protocol can be either TCP or UDP.
newCustomRoutingDestinationConfiguration ::
  -- | 'fromPort'
  Prelude.Natural ->
  -- | 'toPort'
  Prelude.Natural ->
  -- | 'protocols'
  Prelude.NonEmpty CustomRoutingProtocol ->
  CustomRoutingDestinationConfiguration
newCustomRoutingDestinationConfiguration
  pFromPort_
  pToPort_
  pProtocols_ =
    CustomRoutingDestinationConfiguration'
      { fromPort =
          pFromPort_,
        toPort = pToPort_,
        protocols =
          Lens.coerced Lens.# pProtocols_
      }

-- | The first port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
customRoutingDestinationConfiguration_fromPort :: Lens.Lens' CustomRoutingDestinationConfiguration Prelude.Natural
customRoutingDestinationConfiguration_fromPort = Lens.lens (\CustomRoutingDestinationConfiguration' {fromPort} -> fromPort) (\s@CustomRoutingDestinationConfiguration' {} a -> s {fromPort = a} :: CustomRoutingDestinationConfiguration)

-- | The last port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
customRoutingDestinationConfiguration_toPort :: Lens.Lens' CustomRoutingDestinationConfiguration Prelude.Natural
customRoutingDestinationConfiguration_toPort = Lens.lens (\CustomRoutingDestinationConfiguration' {toPort} -> toPort) (\s@CustomRoutingDestinationConfiguration' {} a -> s {toPort = a} :: CustomRoutingDestinationConfiguration)

-- | The protocol for the endpoint group that is associated with a custom
-- routing accelerator. The protocol can be either TCP or UDP.
customRoutingDestinationConfiguration_protocols :: Lens.Lens' CustomRoutingDestinationConfiguration (Prelude.NonEmpty CustomRoutingProtocol)
customRoutingDestinationConfiguration_protocols = Lens.lens (\CustomRoutingDestinationConfiguration' {protocols} -> protocols) (\s@CustomRoutingDestinationConfiguration' {} a -> s {protocols = a} :: CustomRoutingDestinationConfiguration) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    CustomRoutingDestinationConfiguration
  where
  hashWithSalt
    _salt
    CustomRoutingDestinationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` fromPort
        `Prelude.hashWithSalt` toPort
        `Prelude.hashWithSalt` protocols

instance
  Prelude.NFData
    CustomRoutingDestinationConfiguration
  where
  rnf CustomRoutingDestinationConfiguration' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf toPort
      `Prelude.seq` Prelude.rnf protocols

instance
  Data.ToJSON
    CustomRoutingDestinationConfiguration
  where
  toJSON CustomRoutingDestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FromPort" Data..= fromPort),
            Prelude.Just ("ToPort" Data..= toPort),
            Prelude.Just ("Protocols" Data..= protocols)
          ]
      )
