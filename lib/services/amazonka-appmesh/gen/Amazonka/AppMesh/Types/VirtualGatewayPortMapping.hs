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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayPortMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayPortMapping where

import Amazonka.AppMesh.Types.VirtualGatewayPortProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a port mapping.
--
-- /See:/ 'newVirtualGatewayPortMapping' smart constructor.
data VirtualGatewayPortMapping = VirtualGatewayPortMapping'
  { -- | The port used for the port mapping. Specify one protocol.
    port :: Prelude.Natural,
    -- | The protocol used for the port mapping.
    protocol :: VirtualGatewayPortProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayPortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'virtualGatewayPortMapping_port' - The port used for the port mapping. Specify one protocol.
--
-- 'protocol', 'virtualGatewayPortMapping_protocol' - The protocol used for the port mapping.
newVirtualGatewayPortMapping ::
  -- | 'port'
  Prelude.Natural ->
  -- | 'protocol'
  VirtualGatewayPortProtocol ->
  VirtualGatewayPortMapping
newVirtualGatewayPortMapping pPort_ pProtocol_ =
  VirtualGatewayPortMapping'
    { port = pPort_,
      protocol = pProtocol_
    }

-- | The port used for the port mapping. Specify one protocol.
virtualGatewayPortMapping_port :: Lens.Lens' VirtualGatewayPortMapping Prelude.Natural
virtualGatewayPortMapping_port = Lens.lens (\VirtualGatewayPortMapping' {port} -> port) (\s@VirtualGatewayPortMapping' {} a -> s {port = a} :: VirtualGatewayPortMapping)

-- | The protocol used for the port mapping.
virtualGatewayPortMapping_protocol :: Lens.Lens' VirtualGatewayPortMapping VirtualGatewayPortProtocol
virtualGatewayPortMapping_protocol = Lens.lens (\VirtualGatewayPortMapping' {protocol} -> protocol) (\s@VirtualGatewayPortMapping' {} a -> s {protocol = a} :: VirtualGatewayPortMapping)

instance Data.FromJSON VirtualGatewayPortMapping where
  parseJSON =
    Data.withObject
      "VirtualGatewayPortMapping"
      ( \x ->
          VirtualGatewayPortMapping'
            Prelude.<$> (x Data..: "port")
            Prelude.<*> (x Data..: "protocol")
      )

instance Prelude.Hashable VirtualGatewayPortMapping where
  hashWithSalt _salt VirtualGatewayPortMapping' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData VirtualGatewayPortMapping where
  rnf VirtualGatewayPortMapping' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf protocol

instance Data.ToJSON VirtualGatewayPortMapping where
  toJSON VirtualGatewayPortMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("port" Data..= port),
            Prelude.Just ("protocol" Data..= protocol)
          ]
      )
