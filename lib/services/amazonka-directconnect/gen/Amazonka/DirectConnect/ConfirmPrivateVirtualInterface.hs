{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectConnect.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a private virtual interface created by another
-- Amazon Web Services account.
--
-- After the virtual interface owner makes this call, the virtual interface
-- is created and attached to the specified virtual private gateway or
-- Direct Connect gateway, and is made available to handle traffic.
module Amazonka.DirectConnect.ConfirmPrivateVirtualInterface
  ( -- * Creating a Request
    ConfirmPrivateVirtualInterface (..),
    newConfirmPrivateVirtualInterface,

    -- * Request Lenses
    confirmPrivateVirtualInterface_directConnectGatewayId,
    confirmPrivateVirtualInterface_virtualGatewayId,
    confirmPrivateVirtualInterface_virtualInterfaceId,

    -- * Destructuring the Response
    ConfirmPrivateVirtualInterfaceResponse (..),
    newConfirmPrivateVirtualInterfaceResponse,

    -- * Response Lenses
    confirmPrivateVirtualInterfaceResponse_virtualInterfaceState,
    confirmPrivateVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfirmPrivateVirtualInterface' smart constructor.
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmPrivateVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayId', 'confirmPrivateVirtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'virtualGatewayId', 'confirmPrivateVirtualInterface_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'virtualInterfaceId', 'confirmPrivateVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
newConfirmPrivateVirtualInterface ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  ConfirmPrivateVirtualInterface
newConfirmPrivateVirtualInterface
  pVirtualInterfaceId_ =
    ConfirmPrivateVirtualInterface'
      { directConnectGatewayId =
          Prelude.Nothing,
        virtualGatewayId = Prelude.Nothing,
        virtualInterfaceId = pVirtualInterfaceId_
      }

-- | The ID of the Direct Connect gateway.
confirmPrivateVirtualInterface_directConnectGatewayId :: Lens.Lens' ConfirmPrivateVirtualInterface (Prelude.Maybe Prelude.Text)
confirmPrivateVirtualInterface_directConnectGatewayId = Lens.lens (\ConfirmPrivateVirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@ConfirmPrivateVirtualInterface' {} a -> s {directConnectGatewayId = a} :: ConfirmPrivateVirtualInterface)

-- | The ID of the virtual private gateway.
confirmPrivateVirtualInterface_virtualGatewayId :: Lens.Lens' ConfirmPrivateVirtualInterface (Prelude.Maybe Prelude.Text)
confirmPrivateVirtualInterface_virtualGatewayId = Lens.lens (\ConfirmPrivateVirtualInterface' {virtualGatewayId} -> virtualGatewayId) (\s@ConfirmPrivateVirtualInterface' {} a -> s {virtualGatewayId = a} :: ConfirmPrivateVirtualInterface)

-- | The ID of the virtual interface.
confirmPrivateVirtualInterface_virtualInterfaceId :: Lens.Lens' ConfirmPrivateVirtualInterface Prelude.Text
confirmPrivateVirtualInterface_virtualInterfaceId = Lens.lens (\ConfirmPrivateVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@ConfirmPrivateVirtualInterface' {} a -> s {virtualInterfaceId = a} :: ConfirmPrivateVirtualInterface)

instance
  Core.AWSRequest
    ConfirmPrivateVirtualInterface
  where
  type
    AWSResponse ConfirmPrivateVirtualInterface =
      ConfirmPrivateVirtualInterfaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmPrivateVirtualInterfaceResponse'
            Prelude.<$> (x Data..?> "virtualInterfaceState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ConfirmPrivateVirtualInterface
  where
  hashWithSalt
    _salt
    ConfirmPrivateVirtualInterface' {..} =
      _salt
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` virtualGatewayId
        `Prelude.hashWithSalt` virtualInterfaceId

instance
  Prelude.NFData
    ConfirmPrivateVirtualInterface
  where
  rnf ConfirmPrivateVirtualInterface' {..} =
    Prelude.rnf directConnectGatewayId `Prelude.seq`
      Prelude.rnf virtualGatewayId `Prelude.seq`
        Prelude.rnf virtualInterfaceId

instance
  Data.ToHeaders
    ConfirmPrivateVirtualInterface
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.ConfirmPrivateVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfirmPrivateVirtualInterface where
  toJSON ConfirmPrivateVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("directConnectGatewayId" Data..=)
              Prelude.<$> directConnectGatewayId,
            ("virtualGatewayId" Data..=)
              Prelude.<$> virtualGatewayId,
            Prelude.Just
              ("virtualInterfaceId" Data..= virtualInterfaceId)
          ]
      )

instance Data.ToPath ConfirmPrivateVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmPrivateVirtualInterface where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfirmPrivateVirtualInterfaceResponse' smart constructor.
data ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'
  { -- | The state of the virtual interface. The following are the possible
    -- values:
    --
    -- -   @confirming@: The creation of the virtual interface is pending
    --     confirmation from the virtual interface owner. If the owner of the
    --     virtual interface is different from the owner of the connection on
    --     which it is provisioned, then the virtual interface will remain in
    --     this state until it is confirmed by the virtual interface owner.
    --
    -- -   @verifying@: This state only applies to public virtual interfaces.
    --     Each public virtual interface needs validation before the virtual
    --     interface can be created.
    --
    -- -   @pending@: A virtual interface is in this state from the time that
    --     it is created until the virtual interface is ready to forward
    --     traffic.
    --
    -- -   @available@: A virtual interface that is able to forward traffic.
    --
    -- -   @down@: A virtual interface that is BGP down.
    --
    -- -   @deleting@: A virtual interface is in this state immediately after
    --     calling DeleteVirtualInterface until it can no longer forward
    --     traffic.
    --
    -- -   @deleted@: A virtual interface that cannot forward traffic.
    --
    -- -   @rejected@: The virtual interface owner has declined creation of the
    --     virtual interface. If a virtual interface in the @Confirming@ state
    --     is deleted by the virtual interface owner, the virtual interface
    --     enters the @Rejected@ state.
    --
    -- -   @unknown@: The state of the virtual interface is not available.
    virtualInterfaceState :: Prelude.Maybe VirtualInterfaceState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmPrivateVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceState', 'confirmPrivateVirtualInterfaceResponse_virtualInterfaceState' - The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
--
-- 'httpStatus', 'confirmPrivateVirtualInterfaceResponse_httpStatus' - The response's http status code.
newConfirmPrivateVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmPrivateVirtualInterfaceResponse
newConfirmPrivateVirtualInterfaceResponse
  pHttpStatus_ =
    ConfirmPrivateVirtualInterfaceResponse'
      { virtualInterfaceState =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
confirmPrivateVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' ConfirmPrivateVirtualInterfaceResponse (Prelude.Maybe VirtualInterfaceState)
confirmPrivateVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\ConfirmPrivateVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@ConfirmPrivateVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: ConfirmPrivateVirtualInterfaceResponse)

-- | The response's http status code.
confirmPrivateVirtualInterfaceResponse_httpStatus :: Lens.Lens' ConfirmPrivateVirtualInterfaceResponse Prelude.Int
confirmPrivateVirtualInterfaceResponse_httpStatus = Lens.lens (\ConfirmPrivateVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@ConfirmPrivateVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: ConfirmPrivateVirtualInterfaceResponse)

instance
  Prelude.NFData
    ConfirmPrivateVirtualInterfaceResponse
  where
  rnf ConfirmPrivateVirtualInterfaceResponse' {..} =
    Prelude.rnf virtualInterfaceState `Prelude.seq`
      Prelude.rnf httpStatus
