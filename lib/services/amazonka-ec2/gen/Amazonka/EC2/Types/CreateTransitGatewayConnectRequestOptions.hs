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
-- Module      : Amazonka.EC2.Types.CreateTransitGatewayConnectRequestOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateTransitGatewayConnectRequestOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ProtocolValue
import qualified Amazonka.Prelude as Prelude

-- | The options for a Connect attachment.
--
-- /See:/ 'newCreateTransitGatewayConnectRequestOptions' smart constructor.
data CreateTransitGatewayConnectRequestOptions = CreateTransitGatewayConnectRequestOptions'
  { -- | The tunnel protocol.
    protocol :: ProtocolValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayConnectRequestOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'createTransitGatewayConnectRequestOptions_protocol' - The tunnel protocol.
newCreateTransitGatewayConnectRequestOptions ::
  -- | 'protocol'
  ProtocolValue ->
  CreateTransitGatewayConnectRequestOptions
newCreateTransitGatewayConnectRequestOptions
  pProtocol_ =
    CreateTransitGatewayConnectRequestOptions'
      { protocol =
          pProtocol_
      }

-- | The tunnel protocol.
createTransitGatewayConnectRequestOptions_protocol :: Lens.Lens' CreateTransitGatewayConnectRequestOptions ProtocolValue
createTransitGatewayConnectRequestOptions_protocol = Lens.lens (\CreateTransitGatewayConnectRequestOptions' {protocol} -> protocol) (\s@CreateTransitGatewayConnectRequestOptions' {} a -> s {protocol = a} :: CreateTransitGatewayConnectRequestOptions)

instance
  Prelude.Hashable
    CreateTransitGatewayConnectRequestOptions
  where
  hashWithSalt
    _salt
    CreateTransitGatewayConnectRequestOptions' {..} =
      _salt `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    CreateTransitGatewayConnectRequestOptions
  where
  rnf CreateTransitGatewayConnectRequestOptions' {..} =
    Prelude.rnf protocol

instance
  Data.ToQuery
    CreateTransitGatewayConnectRequestOptions
  where
  toQuery
    CreateTransitGatewayConnectRequestOptions' {..} =
      Prelude.mconcat ["Protocol" Data.=: protocol]
