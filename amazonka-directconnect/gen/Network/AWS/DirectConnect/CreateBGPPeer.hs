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
-- Module      : Network.AWS.DirectConnect.CreateBGPPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a BGP peer on the specified virtual interface.
--
-- You must create a BGP peer for the corresponding address family
-- (IPv4\/IPv6) in order to access AWS resources that also use that address
-- family.
--
-- If logical redundancy is not supported by the connection, interconnect,
-- or LAG, the BGP peer cannot be in the same address family as an existing
-- BGP peer on the virtual interface.
--
-- When creating a IPv6 BGP peer, omit the Amazon address and customer
-- address. IPv6 addresses are automatically assigned from the Amazon pool
-- of IPv6 addresses; you cannot specify custom IPv6 addresses.
--
-- For a public virtual interface, the Autonomous System Number (ASN) must
-- be private or already on the allow list for the virtual interface.
module Network.AWS.DirectConnect.CreateBGPPeer
  ( -- * Creating a Request
    CreateBGPPeer (..),
    newCreateBGPPeer,

    -- * Request Lenses
    createBGPPeer_virtualInterfaceId,
    createBGPPeer_newBGPPeer,

    -- * Destructuring the Response
    CreateBGPPeerResponse (..),
    newCreateBGPPeerResponse,

    -- * Response Lenses
    createBGPPeerResponse_virtualInterface,
    createBGPPeerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBGPPeer' smart constructor.
data CreateBGPPeer = CreateBGPPeer'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text,
    -- | Information about the BGP peer.
    newBGPPeer' :: Core.Maybe NewBGPPeer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterfaceId', 'createBGPPeer_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'newBGPPeer'', 'createBGPPeer_newBGPPeer' - Information about the BGP peer.
newCreateBGPPeer ::
  CreateBGPPeer
newCreateBGPPeer =
  CreateBGPPeer'
    { virtualInterfaceId = Core.Nothing,
      newBGPPeer' = Core.Nothing
    }

-- | The ID of the virtual interface.
createBGPPeer_virtualInterfaceId :: Lens.Lens' CreateBGPPeer (Core.Maybe Core.Text)
createBGPPeer_virtualInterfaceId = Lens.lens (\CreateBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@CreateBGPPeer' {} a -> s {virtualInterfaceId = a} :: CreateBGPPeer)

-- | Information about the BGP peer.
createBGPPeer_newBGPPeer :: Lens.Lens' CreateBGPPeer (Core.Maybe NewBGPPeer)
createBGPPeer_newBGPPeer = Lens.lens (\CreateBGPPeer' {newBGPPeer'} -> newBGPPeer') (\s@CreateBGPPeer' {} a -> s {newBGPPeer' = a} :: CreateBGPPeer)

instance Core.AWSRequest CreateBGPPeer where
  type
    AWSResponse CreateBGPPeer =
      CreateBGPPeerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBGPPeerResponse'
            Core.<$> (x Core..?> "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBGPPeer

instance Core.NFData CreateBGPPeer

instance Core.ToHeaders CreateBGPPeer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.CreateBGPPeer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBGPPeer where
  toJSON CreateBGPPeer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("virtualInterfaceId" Core..=)
              Core.<$> virtualInterfaceId,
            ("newBGPPeer" Core..=) Core.<$> newBGPPeer'
          ]
      )

instance Core.ToPath CreateBGPPeer where
  toPath = Core.const "/"

instance Core.ToQuery CreateBGPPeer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse = CreateBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Core.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBGPPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterface', 'createBGPPeerResponse_virtualInterface' - The virtual interface.
--
-- 'httpStatus', 'createBGPPeerResponse_httpStatus' - The response's http status code.
newCreateBGPPeerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBGPPeerResponse
newCreateBGPPeerResponse pHttpStatus_ =
  CreateBGPPeerResponse'
    { virtualInterface =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual interface.
createBGPPeerResponse_virtualInterface :: Lens.Lens' CreateBGPPeerResponse (Core.Maybe VirtualInterface)
createBGPPeerResponse_virtualInterface = Lens.lens (\CreateBGPPeerResponse' {virtualInterface} -> virtualInterface) (\s@CreateBGPPeerResponse' {} a -> s {virtualInterface = a} :: CreateBGPPeerResponse)

-- | The response's http status code.
createBGPPeerResponse_httpStatus :: Lens.Lens' CreateBGPPeerResponse Core.Int
createBGPPeerResponse_httpStatus = Lens.lens (\CreateBGPPeerResponse' {httpStatus} -> httpStatus) (\s@CreateBGPPeerResponse' {} a -> s {httpStatus = a} :: CreateBGPPeerResponse)

instance Core.NFData CreateBGPPeerResponse
