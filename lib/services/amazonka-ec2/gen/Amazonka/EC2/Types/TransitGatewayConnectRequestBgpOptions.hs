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
-- Module      : Amazonka.EC2.Types.TransitGatewayConnectRequestBgpOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayConnectRequestBgpOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The BGP options for the Connect attachment.
--
-- /See:/ 'newTransitGatewayConnectRequestBgpOptions' smart constructor.
data TransitGatewayConnectRequestBgpOptions = TransitGatewayConnectRequestBgpOptions'
  { -- | The peer Autonomous System Number (ASN).
    peerAsn :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  hashWithSalt
    _salt
    TransitGatewayConnectRequestBgpOptions' {..} =
      _salt `Prelude.hashWithSalt` peerAsn

instance
  Prelude.NFData
    TransitGatewayConnectRequestBgpOptions
  where
  rnf TransitGatewayConnectRequestBgpOptions' {..} =
    Prelude.rnf peerAsn

instance
  Data.ToQuery
    TransitGatewayConnectRequestBgpOptions
  where
  toQuery TransitGatewayConnectRequestBgpOptions' {..} =
    Prelude.mconcat ["PeerAsn" Data.=: peerAsn]
