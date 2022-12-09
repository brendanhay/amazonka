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
-- Module      : Amazonka.EC2.Types.CreateVerifiedAccessEndpointEniOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVerifiedAccessEndpointEniOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VerifiedAccessEndpointProtocol
import qualified Amazonka.Prelude as Prelude

-- | Options for a network interface-type endpoint.
--
-- /See:/ 'newCreateVerifiedAccessEndpointEniOptions' smart constructor.
data CreateVerifiedAccessEndpointEniOptions = CreateVerifiedAccessEndpointEniOptions'
  { -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IP port number.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The IP protocol.
    protocol :: Prelude.Maybe VerifiedAccessEndpointProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVerifiedAccessEndpointEniOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'createVerifiedAccessEndpointEniOptions_networkInterfaceId' - The ID of the network interface.
--
-- 'port', 'createVerifiedAccessEndpointEniOptions_port' - The IP port number.
--
-- 'protocol', 'createVerifiedAccessEndpointEniOptions_protocol' - The IP protocol.
newCreateVerifiedAccessEndpointEniOptions ::
  CreateVerifiedAccessEndpointEniOptions
newCreateVerifiedAccessEndpointEniOptions =
  CreateVerifiedAccessEndpointEniOptions'
    { networkInterfaceId =
        Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The ID of the network interface.
createVerifiedAccessEndpointEniOptions_networkInterfaceId :: Lens.Lens' CreateVerifiedAccessEndpointEniOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessEndpointEniOptions_networkInterfaceId = Lens.lens (\CreateVerifiedAccessEndpointEniOptions' {networkInterfaceId} -> networkInterfaceId) (\s@CreateVerifiedAccessEndpointEniOptions' {} a -> s {networkInterfaceId = a} :: CreateVerifiedAccessEndpointEniOptions)

-- | The IP port number.
createVerifiedAccessEndpointEniOptions_port :: Lens.Lens' CreateVerifiedAccessEndpointEniOptions (Prelude.Maybe Prelude.Natural)
createVerifiedAccessEndpointEniOptions_port = Lens.lens (\CreateVerifiedAccessEndpointEniOptions' {port} -> port) (\s@CreateVerifiedAccessEndpointEniOptions' {} a -> s {port = a} :: CreateVerifiedAccessEndpointEniOptions)

-- | The IP protocol.
createVerifiedAccessEndpointEniOptions_protocol :: Lens.Lens' CreateVerifiedAccessEndpointEniOptions (Prelude.Maybe VerifiedAccessEndpointProtocol)
createVerifiedAccessEndpointEniOptions_protocol = Lens.lens (\CreateVerifiedAccessEndpointEniOptions' {protocol} -> protocol) (\s@CreateVerifiedAccessEndpointEniOptions' {} a -> s {protocol = a} :: CreateVerifiedAccessEndpointEniOptions)

instance
  Prelude.Hashable
    CreateVerifiedAccessEndpointEniOptions
  where
  hashWithSalt
    _salt
    CreateVerifiedAccessEndpointEniOptions' {..} =
      _salt `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` port
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    CreateVerifiedAccessEndpointEniOptions
  where
  rnf CreateVerifiedAccessEndpointEniOptions' {..} =
    Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol

instance
  Data.ToQuery
    CreateVerifiedAccessEndpointEniOptions
  where
  toQuery CreateVerifiedAccessEndpointEniOptions' {..} =
    Prelude.mconcat
      [ "NetworkInterfaceId" Data.=: networkInterfaceId,
        "Port" Data.=: port,
        "Protocol" Data.=: protocol
      ]
