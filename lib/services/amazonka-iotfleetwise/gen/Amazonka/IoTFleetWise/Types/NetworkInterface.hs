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
-- Module      : Amazonka.IoTFleetWise.Types.NetworkInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.CanInterface
import Amazonka.IoTFleetWise.Types.NetworkInterfaceType
import Amazonka.IoTFleetWise.Types.ObdInterface
import qualified Amazonka.Prelude as Prelude

-- | Represents a node and its specifications in an in-vehicle communication
-- network. All signal decoders must be associated with a network node.
--
-- To return this information about all the network interfaces specified in
-- a decoder manifest, use the API operation.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | Information about a network interface specified by the On-board
    -- diagnostic (OBD) II protocol.
    obdInterface :: Prelude.Maybe ObdInterface,
    -- | Information about a network interface specified by the Controller Area
    -- Network (CAN) protocol.
    canInterface :: Prelude.Maybe CanInterface,
    -- | The ID of the network interface.
    interfaceId :: Prelude.Text,
    -- | The network protocol for the vehicle. For example, @CAN_SIGNAL@
    -- specifies a protocol that defines how data is communicated between
    -- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
    -- defines how self-diagnostic data is communicated between ECUs.
    type' :: NetworkInterfaceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'obdInterface', 'networkInterface_obdInterface' - Information about a network interface specified by the On-board
-- diagnostic (OBD) II protocol.
--
-- 'canInterface', 'networkInterface_canInterface' - Information about a network interface specified by the Controller Area
-- Network (CAN) protocol.
--
-- 'interfaceId', 'networkInterface_interfaceId' - The ID of the network interface.
--
-- 'type'', 'networkInterface_type' - The network protocol for the vehicle. For example, @CAN_SIGNAL@
-- specifies a protocol that defines how data is communicated between
-- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
-- defines how self-diagnostic data is communicated between ECUs.
newNetworkInterface ::
  -- | 'interfaceId'
  Prelude.Text ->
  -- | 'type''
  NetworkInterfaceType ->
  NetworkInterface
newNetworkInterface pInterfaceId_ pType_ =
  NetworkInterface'
    { obdInterface = Prelude.Nothing,
      canInterface = Prelude.Nothing,
      interfaceId = pInterfaceId_,
      type' = pType_
    }

-- | Information about a network interface specified by the On-board
-- diagnostic (OBD) II protocol.
networkInterface_obdInterface :: Lens.Lens' NetworkInterface (Prelude.Maybe ObdInterface)
networkInterface_obdInterface = Lens.lens (\NetworkInterface' {obdInterface} -> obdInterface) (\s@NetworkInterface' {} a -> s {obdInterface = a} :: NetworkInterface)

-- | Information about a network interface specified by the Controller Area
-- Network (CAN) protocol.
networkInterface_canInterface :: Lens.Lens' NetworkInterface (Prelude.Maybe CanInterface)
networkInterface_canInterface = Lens.lens (\NetworkInterface' {canInterface} -> canInterface) (\s@NetworkInterface' {} a -> s {canInterface = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_interfaceId :: Lens.Lens' NetworkInterface Prelude.Text
networkInterface_interfaceId = Lens.lens (\NetworkInterface' {interfaceId} -> interfaceId) (\s@NetworkInterface' {} a -> s {interfaceId = a} :: NetworkInterface)

-- | The network protocol for the vehicle. For example, @CAN_SIGNAL@
-- specifies a protocol that defines how data is communicated between
-- electronic control units (ECUs). @OBD_SIGNAL@ specifies a protocol that
-- defines how self-diagnostic data is communicated between ECUs.
networkInterface_type :: Lens.Lens' NetworkInterface NetworkInterfaceType
networkInterface_type = Lens.lens (\NetworkInterface' {type'} -> type') (\s@NetworkInterface' {} a -> s {type' = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "obdInterface")
            Prelude.<*> (x Data..:? "canInterface")
            Prelude.<*> (x Data..: "interfaceId")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` obdInterface
      `Prelude.hashWithSalt` canInterface
      `Prelude.hashWithSalt` interfaceId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf obdInterface
      `Prelude.seq` Prelude.rnf canInterface
      `Prelude.seq` Prelude.rnf interfaceId
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON NetworkInterface where
  toJSON NetworkInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("obdInterface" Data..=) Prelude.<$> obdInterface,
            ("canInterface" Data..=) Prelude.<$> canInterface,
            Prelude.Just ("interfaceId" Data..= interfaceId),
            Prelude.Just ("type" Data..= type')
          ]
      )
