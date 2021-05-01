{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CreateTransitGatewayConnectRequestOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateTransitGatewayConnectRequestOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ProtocolValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The options for a Connect attachment.
--
-- /See:/ 'newCreateTransitGatewayConnectRequestOptions' smart constructor.
data CreateTransitGatewayConnectRequestOptions = CreateTransitGatewayConnectRequestOptions'
  { -- | The tunnel protocol.
    protocol :: ProtocolValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.NFData
    CreateTransitGatewayConnectRequestOptions

instance
  Prelude.ToQuery
    CreateTransitGatewayConnectRequestOptions
  where
  toQuery
    CreateTransitGatewayConnectRequestOptions' {..} =
      Prelude.mconcat ["Protocol" Prelude.=: protocol]
