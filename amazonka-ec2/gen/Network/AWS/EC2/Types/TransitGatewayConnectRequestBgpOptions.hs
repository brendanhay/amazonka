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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectRequestBgpOptions where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The BGP options for the Connect attachment.
--
-- /See:/ 'newTransitGatewayConnectRequestBgpOptions' smart constructor.
data TransitGatewayConnectRequestBgpOptions = TransitGatewayConnectRequestBgpOptions'
  { -- | The peer Autonomous System Number (ASN).
    peerAsn :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The peer Autonomous System Number (ASN).
transitGatewayConnectRequestBgpOptions_peerAsn :: Lens.Lens' TransitGatewayConnectRequestBgpOptions (Prelude.Maybe Prelude.Integer)
transitGatewayConnectRequestBgpOptions_peerAsn = Lens.lens (\TransitGatewayConnectRequestBgpOptions' {peerAsn} -> peerAsn) (\s@TransitGatewayConnectRequestBgpOptions' {} a -> s {peerAsn = a} :: TransitGatewayConnectRequestBgpOptions)

instance
  Prelude.Hashable
    TransitGatewayConnectRequestBgpOptions

instance
  Prelude.NFData
    TransitGatewayConnectRequestBgpOptions

instance
  Prelude.ToQuery
    TransitGatewayConnectRequestBgpOptions
  where
  toQuery TransitGatewayConnectRequestBgpOptions' {..} =
    Prelude.mconcat ["PeerAsn" Prelude.=: peerAsn]
