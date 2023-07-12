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
-- Module      : Amazonka.DirectConnect.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts ownership of a public virtual interface created by another
-- Amazon Web Services account.
--
-- After the virtual interface owner makes this call, the specified virtual
-- interface is created and made available to handle traffic.
module Amazonka.DirectConnect.ConfirmPublicVirtualInterface
  ( -- * Creating a Request
    ConfirmPublicVirtualInterface (..),
    newConfirmPublicVirtualInterface,

    -- * Request Lenses
    confirmPublicVirtualInterface_virtualInterfaceId,

    -- * Destructuring the Response
    ConfirmPublicVirtualInterfaceResponse (..),
    newConfirmPublicVirtualInterfaceResponse,

    -- * Response Lenses
    confirmPublicVirtualInterfaceResponse_virtualInterfaceState,
    confirmPublicVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newConfirmPublicVirtualInterface' smart constructor.
data ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfirmPublicVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'confirmPublicVirtualInterface_virtualInterfaceId' - The ID of the virtual interface.
newConfirmPublicVirtualInterface ::
  -- | 'virtualInterfaceId'
  Prelude.Text ->
  ConfirmPublicVirtualInterface
newConfirmPublicVirtualInterface pVirtualInterfaceId_ =
  ConfirmPublicVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_
    }

-- | The ID of the virtual interface.
confirmPublicVirtualInterface_virtualInterfaceId :: Lens.Lens' ConfirmPublicVirtualInterface Prelude.Text
confirmPublicVirtualInterface_virtualInterfaceId = Lens.lens (\ConfirmPublicVirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@ConfirmPublicVirtualInterface' {} a -> s {virtualInterfaceId = a} :: ConfirmPublicVirtualInterface)

instance
  Core.AWSRequest
    ConfirmPublicVirtualInterface
  where
  type
    AWSResponse ConfirmPublicVirtualInterface =
      ConfirmPublicVirtualInterfaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConfirmPublicVirtualInterfaceResponse'
            Prelude.<$> (x Data..?> "virtualInterfaceState")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ConfirmPublicVirtualInterface
  where
  hashWithSalt _salt ConfirmPublicVirtualInterface' {..} =
    _salt `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData ConfirmPublicVirtualInterface where
  rnf ConfirmPublicVirtualInterface' {..} =
    Prelude.rnf virtualInterfaceId

instance Data.ToHeaders ConfirmPublicVirtualInterface where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.ConfirmPublicVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConfirmPublicVirtualInterface where
  toJSON ConfirmPublicVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualInterfaceId" Data..= virtualInterfaceId)
          ]
      )

instance Data.ToPath ConfirmPublicVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery ConfirmPublicVirtualInterface where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newConfirmPublicVirtualInterfaceResponse' smart constructor.
data ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse'
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
-- Create a value of 'ConfirmPublicVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceState', 'confirmPublicVirtualInterfaceResponse_virtualInterfaceState' - The state of the virtual interface. The following are the possible
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
-- 'httpStatus', 'confirmPublicVirtualInterfaceResponse_httpStatus' - The response's http status code.
newConfirmPublicVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConfirmPublicVirtualInterfaceResponse
newConfirmPublicVirtualInterfaceResponse pHttpStatus_ =
  ConfirmPublicVirtualInterfaceResponse'
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
confirmPublicVirtualInterfaceResponse_virtualInterfaceState :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse (Prelude.Maybe VirtualInterfaceState)
confirmPublicVirtualInterfaceResponse_virtualInterfaceState = Lens.lens (\ConfirmPublicVirtualInterfaceResponse' {virtualInterfaceState} -> virtualInterfaceState) (\s@ConfirmPublicVirtualInterfaceResponse' {} a -> s {virtualInterfaceState = a} :: ConfirmPublicVirtualInterfaceResponse)

-- | The response's http status code.
confirmPublicVirtualInterfaceResponse_httpStatus :: Lens.Lens' ConfirmPublicVirtualInterfaceResponse Prelude.Int
confirmPublicVirtualInterfaceResponse_httpStatus = Lens.lens (\ConfirmPublicVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@ConfirmPublicVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: ConfirmPublicVirtualInterfaceResponse)

instance
  Prelude.NFData
    ConfirmPublicVirtualInterfaceResponse
  where
  rnf ConfirmPublicVirtualInterfaceResponse' {..} =
    Prelude.rnf virtualInterfaceState
      `Prelude.seq` Prelude.rnf httpStatus
