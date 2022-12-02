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
-- Module      : Amazonka.DirectConnect.AllocateTransitVirtualInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a transit virtual interface to be owned by the specified
-- Amazon Web Services account. Use this type of interface to connect a
-- transit gateway to your Direct Connect gateway.
--
-- The owner of a connection provisions a transit virtual interface to be
-- owned by the specified Amazon Web Services account.
--
-- After you create a transit virtual interface, it must be confirmed by
-- the owner using ConfirmTransitVirtualInterface. Until this step has been
-- completed, the transit virtual interface is in the @requested@ state and
-- is not available to handle traffic.
module Amazonka.DirectConnect.AllocateTransitVirtualInterface
  ( -- * Creating a Request
    AllocateTransitVirtualInterface (..),
    newAllocateTransitVirtualInterface,

    -- * Request Lenses
    allocateTransitVirtualInterface_connectionId,
    allocateTransitVirtualInterface_ownerAccount,
    allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation,

    -- * Destructuring the Response
    AllocateTransitVirtualInterfaceResponse (..),
    newAllocateTransitVirtualInterfaceResponse,

    -- * Response Lenses
    allocateTransitVirtualInterfaceResponse_virtualInterface,
    allocateTransitVirtualInterfaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllocateTransitVirtualInterface' smart constructor.
data AllocateTransitVirtualInterface = AllocateTransitVirtualInterface'
  { -- | The ID of the connection on which the transit virtual interface is
    -- provisioned.
    connectionId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the transit virtual
    -- interface.
    ownerAccount :: Prelude.Text,
    -- | Information about the transit virtual interface.
    newTransitVirtualInterfaceAllocation' :: NewTransitVirtualInterfaceAllocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllocateTransitVirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'allocateTransitVirtualInterface_connectionId' - The ID of the connection on which the transit virtual interface is
-- provisioned.
--
-- 'ownerAccount', 'allocateTransitVirtualInterface_ownerAccount' - The ID of the Amazon Web Services account that owns the transit virtual
-- interface.
--
-- 'newTransitVirtualInterfaceAllocation'', 'allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation' - Information about the transit virtual interface.
newAllocateTransitVirtualInterface ::
  -- | 'connectionId'
  Prelude.Text ->
  -- | 'ownerAccount'
  Prelude.Text ->
  -- | 'newTransitVirtualInterfaceAllocation''
  NewTransitVirtualInterfaceAllocation ->
  AllocateTransitVirtualInterface
newAllocateTransitVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewTransitVirtualInterfaceAllocation_ =
    AllocateTransitVirtualInterface'
      { connectionId =
          pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newTransitVirtualInterfaceAllocation' =
          pNewTransitVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the transit virtual interface is
-- provisioned.
allocateTransitVirtualInterface_connectionId :: Lens.Lens' AllocateTransitVirtualInterface Prelude.Text
allocateTransitVirtualInterface_connectionId = Lens.lens (\AllocateTransitVirtualInterface' {connectionId} -> connectionId) (\s@AllocateTransitVirtualInterface' {} a -> s {connectionId = a} :: AllocateTransitVirtualInterface)

-- | The ID of the Amazon Web Services account that owns the transit virtual
-- interface.
allocateTransitVirtualInterface_ownerAccount :: Lens.Lens' AllocateTransitVirtualInterface Prelude.Text
allocateTransitVirtualInterface_ownerAccount = Lens.lens (\AllocateTransitVirtualInterface' {ownerAccount} -> ownerAccount) (\s@AllocateTransitVirtualInterface' {} a -> s {ownerAccount = a} :: AllocateTransitVirtualInterface)

-- | Information about the transit virtual interface.
allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation :: Lens.Lens' AllocateTransitVirtualInterface NewTransitVirtualInterfaceAllocation
allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation = Lens.lens (\AllocateTransitVirtualInterface' {newTransitVirtualInterfaceAllocation'} -> newTransitVirtualInterfaceAllocation') (\s@AllocateTransitVirtualInterface' {} a -> s {newTransitVirtualInterfaceAllocation' = a} :: AllocateTransitVirtualInterface)

instance
  Core.AWSRequest
    AllocateTransitVirtualInterface
  where
  type
    AWSResponse AllocateTransitVirtualInterface =
      AllocateTransitVirtualInterfaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AllocateTransitVirtualInterfaceResponse'
            Prelude.<$> (x Data..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AllocateTransitVirtualInterface
  where
  hashWithSalt
    _salt
    AllocateTransitVirtualInterface' {..} =
      _salt `Prelude.hashWithSalt` connectionId
        `Prelude.hashWithSalt` ownerAccount
        `Prelude.hashWithSalt` newTransitVirtualInterfaceAllocation'

instance
  Prelude.NFData
    AllocateTransitVirtualInterface
  where
  rnf AllocateTransitVirtualInterface' {..} =
    Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf newTransitVirtualInterfaceAllocation'

instance
  Data.ToHeaders
    AllocateTransitVirtualInterface
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.AllocateTransitVirtualInterface" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AllocateTransitVirtualInterface where
  toJSON AllocateTransitVirtualInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("connectionId" Data..= connectionId),
            Prelude.Just ("ownerAccount" Data..= ownerAccount),
            Prelude.Just
              ( "newTransitVirtualInterfaceAllocation"
                  Data..= newTransitVirtualInterfaceAllocation'
              )
          ]
      )

instance Data.ToPath AllocateTransitVirtualInterface where
  toPath = Prelude.const "/"

instance Data.ToQuery AllocateTransitVirtualInterface where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAllocateTransitVirtualInterfaceResponse' smart constructor.
data AllocateTransitVirtualInterfaceResponse = AllocateTransitVirtualInterfaceResponse'
  { virtualInterface :: Prelude.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllocateTransitVirtualInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterface', 'allocateTransitVirtualInterfaceResponse_virtualInterface' - Undocumented member.
--
-- 'httpStatus', 'allocateTransitVirtualInterfaceResponse_httpStatus' - The response's http status code.
newAllocateTransitVirtualInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AllocateTransitVirtualInterfaceResponse
newAllocateTransitVirtualInterfaceResponse
  pHttpStatus_ =
    AllocateTransitVirtualInterfaceResponse'
      { virtualInterface =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
allocateTransitVirtualInterfaceResponse_virtualInterface :: Lens.Lens' AllocateTransitVirtualInterfaceResponse (Prelude.Maybe VirtualInterface)
allocateTransitVirtualInterfaceResponse_virtualInterface = Lens.lens (\AllocateTransitVirtualInterfaceResponse' {virtualInterface} -> virtualInterface) (\s@AllocateTransitVirtualInterfaceResponse' {} a -> s {virtualInterface = a} :: AllocateTransitVirtualInterfaceResponse)

-- | The response's http status code.
allocateTransitVirtualInterfaceResponse_httpStatus :: Lens.Lens' AllocateTransitVirtualInterfaceResponse Prelude.Int
allocateTransitVirtualInterfaceResponse_httpStatus = Lens.lens (\AllocateTransitVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@AllocateTransitVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: AllocateTransitVirtualInterfaceResponse)

instance
  Prelude.NFData
    AllocateTransitVirtualInterfaceResponse
  where
  rnf AllocateTransitVirtualInterfaceResponse' {..} =
    Prelude.rnf virtualInterface
      `Prelude.seq` Prelude.rnf httpStatus
