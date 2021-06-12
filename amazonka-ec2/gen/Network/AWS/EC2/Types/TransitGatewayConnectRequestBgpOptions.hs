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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The BGP options for the Connect attachment.
--
-- /See:/ 'newTransitGatewayConnectRequestBgpOptions' smart constructor.
data TransitGatewayConnectRequestBgpOptions = TransitGatewayConnectRequestBgpOptions'
  { -- | The peer Autonomous System Number (ASN).
    peerAsn :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayConnectRequestBgpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAsn', 'transitGatewayConnectRequestBgpOptions_peerAsn' - The peer Autonomous System Number (ASN).
newTransitGatewayConnectRequestBgpOptions ::
  TransitGatewayConnectRequestBgpOptions
newTransitGatewayConnectRequestBgpOptions =
  TransitGatewayConnectRequestBgpOptions'
    { peerAsn =
        Core.Nothing
    }

-- | The peer Autonomous System Number (ASN).
transitGatewayConnectRequestBgpOptions_peerAsn :: Lens.Lens' TransitGatewayConnectRequestBgpOptions (Core.Maybe Core.Integer)
transitGatewayConnectRequestBgpOptions_peerAsn = Lens.lens (\TransitGatewayConnectRequestBgpOptions' {peerAsn} -> peerAsn) (\s@TransitGatewayConnectRequestBgpOptions' {} a -> s {peerAsn = a} :: TransitGatewayConnectRequestBgpOptions)

instance
  Core.Hashable
    TransitGatewayConnectRequestBgpOptions

instance
  Core.NFData
    TransitGatewayConnectRequestBgpOptions

instance
  Core.ToQuery
    TransitGatewayConnectRequestBgpOptions
  where
  toQuery TransitGatewayConnectRequestBgpOptions' {..} =
    Core.mconcat ["PeerAsn" Core.=: peerAsn]
