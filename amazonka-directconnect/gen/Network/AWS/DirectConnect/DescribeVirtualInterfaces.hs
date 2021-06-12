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
-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays all virtual interfaces for an AWS account. Virtual interfaces
-- deleted fewer than 15 minutes before you make the request are also
-- returned. If you specify a connection ID, only the virtual interfaces
-- associated with the connection are returned. If you specify a virtual
-- interface ID, then only a single virtual interface is returned.
--
-- A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer network.
module Network.AWS.DirectConnect.DescribeVirtualInterfaces
  ( -- * Creating a Request
    DescribeVirtualInterfaces (..),
    newDescribeVirtualInterfaces,

    -- * Request Lenses
    describeVirtualInterfaces_connectionId,
    describeVirtualInterfaces_virtualInterfaceId,

    -- * Destructuring the Response
    DescribeVirtualInterfacesResponse (..),
    newDescribeVirtualInterfacesResponse,

    -- * Response Lenses
    describeVirtualInterfacesResponse_virtualInterfaces,
    describeVirtualInterfacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVirtualInterfaces' smart constructor.
data DescribeVirtualInterfaces = DescribeVirtualInterfaces'
  { -- | The ID of the connection.
    connectionId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVirtualInterfaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'describeVirtualInterfaces_connectionId' - The ID of the connection.
--
-- 'virtualInterfaceId', 'describeVirtualInterfaces_virtualInterfaceId' - The ID of the virtual interface.
newDescribeVirtualInterfaces ::
  DescribeVirtualInterfaces
newDescribeVirtualInterfaces =
  DescribeVirtualInterfaces'
    { connectionId =
        Core.Nothing,
      virtualInterfaceId = Core.Nothing
    }

-- | The ID of the connection.
describeVirtualInterfaces_connectionId :: Lens.Lens' DescribeVirtualInterfaces (Core.Maybe Core.Text)
describeVirtualInterfaces_connectionId = Lens.lens (\DescribeVirtualInterfaces' {connectionId} -> connectionId) (\s@DescribeVirtualInterfaces' {} a -> s {connectionId = a} :: DescribeVirtualInterfaces)

-- | The ID of the virtual interface.
describeVirtualInterfaces_virtualInterfaceId :: Lens.Lens' DescribeVirtualInterfaces (Core.Maybe Core.Text)
describeVirtualInterfaces_virtualInterfaceId = Lens.lens (\DescribeVirtualInterfaces' {virtualInterfaceId} -> virtualInterfaceId) (\s@DescribeVirtualInterfaces' {} a -> s {virtualInterfaceId = a} :: DescribeVirtualInterfaces)

instance Core.AWSRequest DescribeVirtualInterfaces where
  type
    AWSResponse DescribeVirtualInterfaces =
      DescribeVirtualInterfacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualInterfacesResponse'
            Core.<$> (x Core..?> "virtualInterfaces" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVirtualInterfaces

instance Core.NFData DescribeVirtualInterfaces

instance Core.ToHeaders DescribeVirtualInterfaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeVirtualInterfaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVirtualInterfaces where
  toJSON DescribeVirtualInterfaces' {..} =
    Core.object
      ( Core.catMaybes
          [ ("connectionId" Core..=) Core.<$> connectionId,
            ("virtualInterfaceId" Core..=)
              Core.<$> virtualInterfaceId
          ]
      )

instance Core.ToPath DescribeVirtualInterfaces where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVirtualInterfaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeVirtualInterfacesResponse' smart constructor.
data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'
  { -- | The virtual interfaces
    virtualInterfaces :: Core.Maybe [VirtualInterface],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVirtualInterfacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaces', 'describeVirtualInterfacesResponse_virtualInterfaces' - The virtual interfaces
--
-- 'httpStatus', 'describeVirtualInterfacesResponse_httpStatus' - The response's http status code.
newDescribeVirtualInterfacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVirtualInterfacesResponse
newDescribeVirtualInterfacesResponse pHttpStatus_ =
  DescribeVirtualInterfacesResponse'
    { virtualInterfaces =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual interfaces
describeVirtualInterfacesResponse_virtualInterfaces :: Lens.Lens' DescribeVirtualInterfacesResponse (Core.Maybe [VirtualInterface])
describeVirtualInterfacesResponse_virtualInterfaces = Lens.lens (\DescribeVirtualInterfacesResponse' {virtualInterfaces} -> virtualInterfaces) (\s@DescribeVirtualInterfacesResponse' {} a -> s {virtualInterfaces = a} :: DescribeVirtualInterfacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVirtualInterfacesResponse_httpStatus :: Lens.Lens' DescribeVirtualInterfacesResponse Core.Int
describeVirtualInterfacesResponse_httpStatus = Lens.lens (\DescribeVirtualInterfacesResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualInterfacesResponse' {} a -> s {httpStatus = a} :: DescribeVirtualInterfacesResponse)

instance
  Core.NFData
    DescribeVirtualInterfacesResponse
