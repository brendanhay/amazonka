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
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGateway where

import Network.AWS.DirectConnect.Types.DirectConnectGatewayState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Direct Connect gateway, which enables you to connect
-- virtual interfaces and virtual private gateway or transit gateways.
--
-- /See:/ 'newDirectConnectGateway' smart constructor.
data DirectConnectGateway = DirectConnectGateway'
  { -- | The state of the Direct Connect gateway. The following are the possible
    -- values:
    --
    -- -   @pending@: The initial state after calling
    --     CreateDirectConnectGateway.
    --
    -- -   @available@: The Direct Connect gateway is ready for use.
    --
    -- -   @deleting@: The initial state after calling
    --     DeleteDirectConnectGateway.
    --
    -- -   @deleted@: The Direct Connect gateway is deleted and cannot pass
    --     traffic.
    directConnectGatewayState :: Prelude.Maybe DirectConnectGatewayState,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text,
    -- | The name of the Direct Connect gateway.
    directConnectGatewayName :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system number (ASN) for the Amazon side of the
    -- connection.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account that owns the Direct Connect gateway.
    ownerAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DirectConnectGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayState', 'directConnectGateway_directConnectGatewayState' - The state of the Direct Connect gateway. The following are the possible
-- values:
--
-- -   @pending@: The initial state after calling
--     CreateDirectConnectGateway.
--
-- -   @available@: The Direct Connect gateway is ready for use.
--
-- -   @deleting@: The initial state after calling
--     DeleteDirectConnectGateway.
--
-- -   @deleted@: The Direct Connect gateway is deleted and cannot pass
--     traffic.
--
-- 'stateChangeError', 'directConnectGateway_stateChangeError' - The error message if the state of an object failed to advance.
--
-- 'directConnectGatewayName', 'directConnectGateway_directConnectGatewayName' - The name of the Direct Connect gateway.
--
-- 'amazonSideAsn', 'directConnectGateway_amazonSideAsn' - The autonomous system number (ASN) for the Amazon side of the
-- connection.
--
-- 'directConnectGatewayId', 'directConnectGateway_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'ownerAccount', 'directConnectGateway_ownerAccount' - The ID of the AWS account that owns the Direct Connect gateway.
newDirectConnectGateway ::
  DirectConnectGateway
newDirectConnectGateway =
  DirectConnectGateway'
    { directConnectGatewayState =
        Prelude.Nothing,
      stateChangeError = Prelude.Nothing,
      directConnectGatewayName = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      ownerAccount = Prelude.Nothing
    }

-- | The state of the Direct Connect gateway. The following are the possible
-- values:
--
-- -   @pending@: The initial state after calling
--     CreateDirectConnectGateway.
--
-- -   @available@: The Direct Connect gateway is ready for use.
--
-- -   @deleting@: The initial state after calling
--     DeleteDirectConnectGateway.
--
-- -   @deleted@: The Direct Connect gateway is deleted and cannot pass
--     traffic.
directConnectGateway_directConnectGatewayState :: Lens.Lens' DirectConnectGateway (Prelude.Maybe DirectConnectGatewayState)
directConnectGateway_directConnectGatewayState = Lens.lens (\DirectConnectGateway' {directConnectGatewayState} -> directConnectGatewayState) (\s@DirectConnectGateway' {} a -> s {directConnectGatewayState = a} :: DirectConnectGateway)

-- | The error message if the state of an object failed to advance.
directConnectGateway_stateChangeError :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_stateChangeError = Lens.lens (\DirectConnectGateway' {stateChangeError} -> stateChangeError) (\s@DirectConnectGateway' {} a -> s {stateChangeError = a} :: DirectConnectGateway)

-- | The name of the Direct Connect gateway.
directConnectGateway_directConnectGatewayName :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_directConnectGatewayName = Lens.lens (\DirectConnectGateway' {directConnectGatewayName} -> directConnectGatewayName) (\s@DirectConnectGateway' {} a -> s {directConnectGatewayName = a} :: DirectConnectGateway)

-- | The autonomous system number (ASN) for the Amazon side of the
-- connection.
directConnectGateway_amazonSideAsn :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Integer)
directConnectGateway_amazonSideAsn = Lens.lens (\DirectConnectGateway' {amazonSideAsn} -> amazonSideAsn) (\s@DirectConnectGateway' {} a -> s {amazonSideAsn = a} :: DirectConnectGateway)

-- | The ID of the Direct Connect gateway.
directConnectGateway_directConnectGatewayId :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_directConnectGatewayId = Lens.lens (\DirectConnectGateway' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGateway' {} a -> s {directConnectGatewayId = a} :: DirectConnectGateway)

-- | The ID of the AWS account that owns the Direct Connect gateway.
directConnectGateway_ownerAccount :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_ownerAccount = Lens.lens (\DirectConnectGateway' {ownerAccount} -> ownerAccount) (\s@DirectConnectGateway' {} a -> s {ownerAccount = a} :: DirectConnectGateway)

instance Prelude.FromJSON DirectConnectGateway where
  parseJSON =
    Prelude.withObject
      "DirectConnectGateway"
      ( \x ->
          DirectConnectGateway'
            Prelude.<$> (x Prelude..:? "directConnectGatewayState")
            Prelude.<*> (x Prelude..:? "stateChangeError")
            Prelude.<*> (x Prelude..:? "directConnectGatewayName")
            Prelude.<*> (x Prelude..:? "amazonSideAsn")
            Prelude.<*> (x Prelude..:? "directConnectGatewayId")
            Prelude.<*> (x Prelude..:? "ownerAccount")
      )

instance Prelude.Hashable DirectConnectGateway

instance Prelude.NFData DirectConnectGateway
