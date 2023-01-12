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
-- Module      : Amazonka.DirectConnect.Types.DirectConnectGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.DirectConnectGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.DirectConnectGatewayState
import qualified Amazonka.Prelude as Prelude

-- | Information about a Direct Connect gateway, which enables you to connect
-- virtual interfaces and virtual private gateway or transit gateways.
--
-- /See:/ 'newDirectConnectGateway' smart constructor.
data DirectConnectGateway = DirectConnectGateway'
  { -- | The autonomous system number (ASN) for the Amazon side of the
    -- connection.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Direct Connect gateway.
    directConnectGatewayName :: Prelude.Maybe Prelude.Text,
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
    directConnectGatewayState :: Prelude.Maybe DirectConnectGatewayState,
    -- | The ID of the Amazon Web Services account that owns the Direct Connect
    -- gateway.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectConnectGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonSideAsn', 'directConnectGateway_amazonSideAsn' - The autonomous system number (ASN) for the Amazon side of the
-- connection.
--
-- 'directConnectGatewayId', 'directConnectGateway_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'directConnectGatewayName', 'directConnectGateway_directConnectGatewayName' - The name of the Direct Connect gateway.
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
-- 'ownerAccount', 'directConnectGateway_ownerAccount' - The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
--
-- 'stateChangeError', 'directConnectGateway_stateChangeError' - The error message if the state of an object failed to advance.
newDirectConnectGateway ::
  DirectConnectGateway
newDirectConnectGateway =
  DirectConnectGateway'
    { amazonSideAsn =
        Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      directConnectGatewayName = Prelude.Nothing,
      directConnectGatewayState = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      stateChangeError = Prelude.Nothing
    }

-- | The autonomous system number (ASN) for the Amazon side of the
-- connection.
directConnectGateway_amazonSideAsn :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Integer)
directConnectGateway_amazonSideAsn = Lens.lens (\DirectConnectGateway' {amazonSideAsn} -> amazonSideAsn) (\s@DirectConnectGateway' {} a -> s {amazonSideAsn = a} :: DirectConnectGateway)

-- | The ID of the Direct Connect gateway.
directConnectGateway_directConnectGatewayId :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_directConnectGatewayId = Lens.lens (\DirectConnectGateway' {directConnectGatewayId} -> directConnectGatewayId) (\s@DirectConnectGateway' {} a -> s {directConnectGatewayId = a} :: DirectConnectGateway)

-- | The name of the Direct Connect gateway.
directConnectGateway_directConnectGatewayName :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_directConnectGatewayName = Lens.lens (\DirectConnectGateway' {directConnectGatewayName} -> directConnectGatewayName) (\s@DirectConnectGateway' {} a -> s {directConnectGatewayName = a} :: DirectConnectGateway)

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

-- | The ID of the Amazon Web Services account that owns the Direct Connect
-- gateway.
directConnectGateway_ownerAccount :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_ownerAccount = Lens.lens (\DirectConnectGateway' {ownerAccount} -> ownerAccount) (\s@DirectConnectGateway' {} a -> s {ownerAccount = a} :: DirectConnectGateway)

-- | The error message if the state of an object failed to advance.
directConnectGateway_stateChangeError :: Lens.Lens' DirectConnectGateway (Prelude.Maybe Prelude.Text)
directConnectGateway_stateChangeError = Lens.lens (\DirectConnectGateway' {stateChangeError} -> stateChangeError) (\s@DirectConnectGateway' {} a -> s {stateChangeError = a} :: DirectConnectGateway)

instance Data.FromJSON DirectConnectGateway where
  parseJSON =
    Data.withObject
      "DirectConnectGateway"
      ( \x ->
          DirectConnectGateway'
            Prelude.<$> (x Data..:? "amazonSideAsn")
            Prelude.<*> (x Data..:? "directConnectGatewayId")
            Prelude.<*> (x Data..:? "directConnectGatewayName")
            Prelude.<*> (x Data..:? "directConnectGatewayState")
            Prelude.<*> (x Data..:? "ownerAccount")
            Prelude.<*> (x Data..:? "stateChangeError")
      )

instance Prelude.Hashable DirectConnectGateway where
  hashWithSalt _salt DirectConnectGateway' {..} =
    _salt `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` directConnectGatewayId
      `Prelude.hashWithSalt` directConnectGatewayName
      `Prelude.hashWithSalt` directConnectGatewayState
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` stateChangeError

instance Prelude.NFData DirectConnectGateway where
  rnf DirectConnectGateway' {..} =
    Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf directConnectGatewayName
      `Prelude.seq` Prelude.rnf directConnectGatewayState
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf stateChangeError
