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
-- Module      : Amazonka.EC2.Types.VerifiedAccessEndpointEniOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessEndpointEniOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Options for a network-interface type endpoint.
--
-- /See:/ 'newVerifiedAccessEndpointEniOptions' smart constructor.
data VerifiedAccessEndpointEniOptions = VerifiedAccessEndpointEniOptions'
  { -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IP port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The IP protocol.
    protocol :: Prelude.Maybe VerifiedAccessEndpointProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessEndpointEniOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'verifiedAccessEndpointEniOptions_networkInterfaceId' - The ID of the network interface.
--
-- 'port', 'verifiedAccessEndpointEniOptions_port' - The IP port number.
--
-- 'protocol', 'verifiedAccessEndpointEniOptions_protocol' - The IP protocol.
newVerifiedAccessEndpointEniOptions ::
  VerifiedAccessEndpointEniOptions
newVerifiedAccessEndpointEniOptions =
  VerifiedAccessEndpointEniOptions'
    { networkInterfaceId =
        Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The ID of the network interface.
verifiedAccessEndpointEniOptions_networkInterfaceId :: Lens.Lens' VerifiedAccessEndpointEniOptions (Prelude.Maybe Prelude.Text)
verifiedAccessEndpointEniOptions_networkInterfaceId = Lens.lens (\VerifiedAccessEndpointEniOptions' {networkInterfaceId} -> networkInterfaceId) (\s@VerifiedAccessEndpointEniOptions' {} a -> s {networkInterfaceId = a} :: VerifiedAccessEndpointEniOptions)

-- | The IP port number.
verifiedAccessEndpointEniOptions_port :: Lens.Lens' VerifiedAccessEndpointEniOptions (Prelude.Maybe Prelude.Natural)
verifiedAccessEndpointEniOptions_port = Lens.lens (\VerifiedAccessEndpointEniOptions' {port} -> port) (\s@VerifiedAccessEndpointEniOptions' {} a -> s {port = a} :: VerifiedAccessEndpointEniOptions)

-- | The IP protocol.
verifiedAccessEndpointEniOptions_protocol :: Lens.Lens' VerifiedAccessEndpointEniOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
verifiedAccessEndpointEniOptions_protocol = Lens.lens (\VerifiedAccessEndpointEniOptions' {protocol} -> protocol) (\s@VerifiedAccessEndpointEniOptions' {} a -> s {protocol = a} :: VerifiedAccessEndpointEniOptions)

instance
  Data.FromXML
    VerifiedAccessEndpointEniOptions
  where
  parseXML x =
    VerifiedAccessEndpointEniOptions'
      Prelude.<$> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "port")
      Prelude.<*> (x Data..@? "protocol")

instance
  Prelude.Hashable
    VerifiedAccessEndpointEniOptions
  where
  hashWithSalt
    _salt
    VerifiedAccessEndpointEniOptions' {..} =
      _salt `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    VerifiedAccessEndpointEniOptions
  where
  rnf VerifiedAccessEndpointEniOptions' {..} =
    Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
