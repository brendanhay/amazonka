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
-- Module      : Network.AWS.DirectConnect.AllocateTransitVirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a transit virtual interface to be owned by the specified AWS
-- account. Use this type of interface to connect a transit gateway to your
-- Direct Connect gateway.
--
-- The owner of a connection provisions a transit virtual interface to be
-- owned by the specified AWS account.
--
-- After you create a transit virtual interface, it must be confirmed by
-- the owner using ConfirmTransitVirtualInterface. Until this step has been
-- completed, the transit virtual interface is in the @requested@ state and
-- is not available to handle traffic.
module Network.AWS.DirectConnect.AllocateTransitVirtualInterface
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAllocateTransitVirtualInterface' smart constructor.
data AllocateTransitVirtualInterface = AllocateTransitVirtualInterface'
  { -- | The ID of the connection on which the transit virtual interface is
    -- provisioned.
    connectionId :: Core.Text,
    -- | The ID of the AWS account that owns the transit virtual interface.
    ownerAccount :: Core.Text,
    -- | Information about the transit virtual interface.
    newTransitVirtualInterfaceAllocation' :: NewTransitVirtualInterfaceAllocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'ownerAccount', 'allocateTransitVirtualInterface_ownerAccount' - The ID of the AWS account that owns the transit virtual interface.
--
-- 'newTransitVirtualInterfaceAllocation'', 'allocateTransitVirtualInterface_newTransitVirtualInterfaceAllocation' - Information about the transit virtual interface.
newAllocateTransitVirtualInterface ::
  -- | 'connectionId'
  Core.Text ->
  -- | 'ownerAccount'
  Core.Text ->
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
allocateTransitVirtualInterface_connectionId :: Lens.Lens' AllocateTransitVirtualInterface Core.Text
allocateTransitVirtualInterface_connectionId = Lens.lens (\AllocateTransitVirtualInterface' {connectionId} -> connectionId) (\s@AllocateTransitVirtualInterface' {} a -> s {connectionId = a} :: AllocateTransitVirtualInterface)

-- | The ID of the AWS account that owns the transit virtual interface.
allocateTransitVirtualInterface_ownerAccount :: Lens.Lens' AllocateTransitVirtualInterface Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AllocateTransitVirtualInterfaceResponse'
            Core.<$> (x Core..?> "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AllocateTransitVirtualInterface

instance Core.NFData AllocateTransitVirtualInterface

instance
  Core.ToHeaders
    AllocateTransitVirtualInterface
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.AllocateTransitVirtualInterface" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AllocateTransitVirtualInterface where
  toJSON AllocateTransitVirtualInterface' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just
              ( "newTransitVirtualInterfaceAllocation"
                  Core..= newTransitVirtualInterfaceAllocation'
              )
          ]
      )

instance Core.ToPath AllocateTransitVirtualInterface where
  toPath = Core.const "/"

instance Core.ToQuery AllocateTransitVirtualInterface where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAllocateTransitVirtualInterfaceResponse' smart constructor.
data AllocateTransitVirtualInterfaceResponse = AllocateTransitVirtualInterfaceResponse'
  { virtualInterface :: Core.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AllocateTransitVirtualInterfaceResponse
newAllocateTransitVirtualInterfaceResponse
  pHttpStatus_ =
    AllocateTransitVirtualInterfaceResponse'
      { virtualInterface =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
allocateTransitVirtualInterfaceResponse_virtualInterface :: Lens.Lens' AllocateTransitVirtualInterfaceResponse (Core.Maybe VirtualInterface)
allocateTransitVirtualInterfaceResponse_virtualInterface = Lens.lens (\AllocateTransitVirtualInterfaceResponse' {virtualInterface} -> virtualInterface) (\s@AllocateTransitVirtualInterfaceResponse' {} a -> s {virtualInterface = a} :: AllocateTransitVirtualInterfaceResponse)

-- | The response's http status code.
allocateTransitVirtualInterfaceResponse_httpStatus :: Lens.Lens' AllocateTransitVirtualInterfaceResponse Core.Int
allocateTransitVirtualInterfaceResponse_httpStatus = Lens.lens (\AllocateTransitVirtualInterfaceResponse' {httpStatus} -> httpStatus) (\s@AllocateTransitVirtualInterfaceResponse' {} a -> s {httpStatus = a} :: AllocateTransitVirtualInterfaceResponse)

instance
  Core.NFData
    AllocateTransitVirtualInterfaceResponse
