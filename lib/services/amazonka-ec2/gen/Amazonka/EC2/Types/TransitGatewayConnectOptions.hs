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
-- Module      : Amazonka.EC2.Types.TransitGatewayConnectOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayConnectOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ProtocolValue
import qualified Amazonka.Prelude as Prelude

-- | Describes the Connect attachment options.
--
-- /See:/ 'newTransitGatewayConnectOptions' smart constructor.
data TransitGatewayConnectOptions = TransitGatewayConnectOptions'
  { -- | The tunnel protocol.
    protocol :: Prelude.Maybe ProtocolValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayConnectOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'transitGatewayConnectOptions_protocol' - The tunnel protocol.
newTransitGatewayConnectOptions ::
  TransitGatewayConnectOptions
newTransitGatewayConnectOptions =
  TransitGatewayConnectOptions'
    { protocol =
        Prelude.Nothing
    }

-- | The tunnel protocol.
transitGatewayConnectOptions_protocol :: Lens.Lens' TransitGatewayConnectOptions (Prelude.Maybe ProtocolValue)
transitGatewayConnectOptions_protocol = Lens.lens (\TransitGatewayConnectOptions' {protocol} -> protocol) (\s@TransitGatewayConnectOptions' {} a -> s {protocol = a} :: TransitGatewayConnectOptions)

instance Core.FromXML TransitGatewayConnectOptions where
  parseXML x =
    TransitGatewayConnectOptions'
      Prelude.<$> (x Core..@? "protocol")

instance
  Prelude.Hashable
    TransitGatewayConnectOptions
  where
  hashWithSalt _salt TransitGatewayConnectOptions' {..} =
    _salt `Prelude.hashWithSalt` protocol

instance Prelude.NFData TransitGatewayConnectOptions where
  rnf TransitGatewayConnectOptions' {..} =
    Prelude.rnf protocol
