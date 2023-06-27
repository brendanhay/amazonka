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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | For a custom routing accelerator, describes the port range and protocol
-- for all endpoints (virtual private cloud subnets) in an endpoint group
-- to accept client traffic on.
--
-- /See:/ 'newCustomRoutingDestinationDescription' smart constructor.
data CustomRoutingDestinationDescription = CustomRoutingDestinationDescription'
  { -- | The first port, inclusive, in the range of ports for the endpoint group
    -- that is associated with a custom routing accelerator.
    fromPort :: Prelude.Maybe Prelude.Natural,
    -- | The protocol for the endpoint group that is associated with a custom
    -- routing accelerator. The protocol can be either TCP or UDP.
    protocols :: Prelude.Maybe [Protocol],
    -- | The last port, inclusive, in the range of ports for the endpoint group
    -- that is associated with a custom routing accelerator.
    toPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingDestinationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'customRoutingDestinationDescription_fromPort' - The first port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
--
-- 'protocols', 'customRoutingDestinationDescription_protocols' - The protocol for the endpoint group that is associated with a custom
-- routing accelerator. The protocol can be either TCP or UDP.
--
-- 'toPort', 'customRoutingDestinationDescription_toPort' - The last port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
newCustomRoutingDestinationDescription ::
  CustomRoutingDestinationDescription
newCustomRoutingDestinationDescription =
  CustomRoutingDestinationDescription'
    { fromPort =
        Prelude.Nothing,
      protocols = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
customRoutingDestinationDescription_fromPort :: Lens.Lens' CustomRoutingDestinationDescription (Prelude.Maybe Prelude.Natural)
customRoutingDestinationDescription_fromPort = Lens.lens (\CustomRoutingDestinationDescription' {fromPort} -> fromPort) (\s@CustomRoutingDestinationDescription' {} a -> s {fromPort = a} :: CustomRoutingDestinationDescription)

-- | The protocol for the endpoint group that is associated with a custom
-- routing accelerator. The protocol can be either TCP or UDP.
customRoutingDestinationDescription_protocols :: Lens.Lens' CustomRoutingDestinationDescription (Prelude.Maybe [Protocol])
customRoutingDestinationDescription_protocols = Lens.lens (\CustomRoutingDestinationDescription' {protocols} -> protocols) (\s@CustomRoutingDestinationDescription' {} a -> s {protocols = a} :: CustomRoutingDestinationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The last port, inclusive, in the range of ports for the endpoint group
-- that is associated with a custom routing accelerator.
customRoutingDestinationDescription_toPort :: Lens.Lens' CustomRoutingDestinationDescription (Prelude.Maybe Prelude.Natural)
customRoutingDestinationDescription_toPort = Lens.lens (\CustomRoutingDestinationDescription' {toPort} -> toPort) (\s@CustomRoutingDestinationDescription' {} a -> s {toPort = a} :: CustomRoutingDestinationDescription)

instance
  Data.FromJSON
    CustomRoutingDestinationDescription
  where
  parseJSON =
    Data.withObject
      "CustomRoutingDestinationDescription"
      ( \x ->
          CustomRoutingDestinationDescription'
            Prelude.<$> (x Data..:? "FromPort")
            Prelude.<*> (x Data..:? "Protocols" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ToPort")
      )

instance
  Prelude.Hashable
    CustomRoutingDestinationDescription
  where
  hashWithSalt
    _salt
    CustomRoutingDestinationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` fromPort
        `Prelude.hashWithSalt` protocols
        `Prelude.hashWithSalt` toPort

instance
  Prelude.NFData
    CustomRoutingDestinationDescription
  where
  rnf CustomRoutingDestinationDescription' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf toPort
