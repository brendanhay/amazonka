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
-- Module      : Network.AWS.DirectConnect.DeleteBGPPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified BGP peer on the specified virtual interface with
-- the specified customer address and ASN.
--
-- You cannot delete the last BGP peer from a virtual interface.
module Network.AWS.DirectConnect.DeleteBGPPeer
  ( -- * Creating a Request
    DeleteBGPPeer (..),
    newDeleteBGPPeer,

    -- * Request Lenses
    deleteBGPPeer_asn,
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_virtualInterfaceId,
    deleteBGPPeer_customerAddress,

    -- * Destructuring the Response
    DeleteBGPPeerResponse (..),
    newDeleteBGPPeerResponse,

    -- * Response Lenses
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBGPPeer' smart constructor.
data DeleteBGPPeer = DeleteBGPPeer'
  { -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Core.Maybe Core.Int,
    -- | The ID of the BGP peer.
    bgpPeerId :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asn', 'deleteBGPPeer_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- 'bgpPeerId', 'deleteBGPPeer_bgpPeerId' - The ID of the BGP peer.
--
-- 'virtualInterfaceId', 'deleteBGPPeer_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'customerAddress', 'deleteBGPPeer_customerAddress' - The IP address assigned to the customer interface.
newDeleteBGPPeer ::
  DeleteBGPPeer
newDeleteBGPPeer =
  DeleteBGPPeer'
    { asn = Core.Nothing,
      bgpPeerId = Core.Nothing,
      virtualInterfaceId = Core.Nothing,
      customerAddress = Core.Nothing
    }

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
deleteBGPPeer_asn :: Lens.Lens' DeleteBGPPeer (Core.Maybe Core.Int)
deleteBGPPeer_asn = Lens.lens (\DeleteBGPPeer' {asn} -> asn) (\s@DeleteBGPPeer' {} a -> s {asn = a} :: DeleteBGPPeer)

-- | The ID of the BGP peer.
deleteBGPPeer_bgpPeerId :: Lens.Lens' DeleteBGPPeer (Core.Maybe Core.Text)
deleteBGPPeer_bgpPeerId = Lens.lens (\DeleteBGPPeer' {bgpPeerId} -> bgpPeerId) (\s@DeleteBGPPeer' {} a -> s {bgpPeerId = a} :: DeleteBGPPeer)

-- | The ID of the virtual interface.
deleteBGPPeer_virtualInterfaceId :: Lens.Lens' DeleteBGPPeer (Core.Maybe Core.Text)
deleteBGPPeer_virtualInterfaceId = Lens.lens (\DeleteBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@DeleteBGPPeer' {} a -> s {virtualInterfaceId = a} :: DeleteBGPPeer)

-- | The IP address assigned to the customer interface.
deleteBGPPeer_customerAddress :: Lens.Lens' DeleteBGPPeer (Core.Maybe Core.Text)
deleteBGPPeer_customerAddress = Lens.lens (\DeleteBGPPeer' {customerAddress} -> customerAddress) (\s@DeleteBGPPeer' {} a -> s {customerAddress = a} :: DeleteBGPPeer)

instance Core.AWSRequest DeleteBGPPeer where
  type
    AWSResponse DeleteBGPPeer =
      DeleteBGPPeerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBGPPeerResponse'
            Core.<$> (x Core..?> "virtualInterface")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteBGPPeer

instance Core.NFData DeleteBGPPeer

instance Core.ToHeaders DeleteBGPPeer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OvertureService.DeleteBGPPeer" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteBGPPeer where
  toJSON DeleteBGPPeer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("asn" Core..=) Core.<$> asn,
            ("bgpPeerId" Core..=) Core.<$> bgpPeerId,
            ("virtualInterfaceId" Core..=)
              Core.<$> virtualInterfaceId,
            ("customerAddress" Core..=)
              Core.<$> customerAddress
          ]
      )

instance Core.ToPath DeleteBGPPeer where
  toPath = Core.const "/"

instance Core.ToQuery DeleteBGPPeer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBGPPeerResponse' smart constructor.
data DeleteBGPPeerResponse = DeleteBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Core.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBGPPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualInterface', 'deleteBGPPeerResponse_virtualInterface' - The virtual interface.
--
-- 'httpStatus', 'deleteBGPPeerResponse_httpStatus' - The response's http status code.
newDeleteBGPPeerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteBGPPeerResponse
newDeleteBGPPeerResponse pHttpStatus_ =
  DeleteBGPPeerResponse'
    { virtualInterface =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual interface.
deleteBGPPeerResponse_virtualInterface :: Lens.Lens' DeleteBGPPeerResponse (Core.Maybe VirtualInterface)
deleteBGPPeerResponse_virtualInterface = Lens.lens (\DeleteBGPPeerResponse' {virtualInterface} -> virtualInterface) (\s@DeleteBGPPeerResponse' {} a -> s {virtualInterface = a} :: DeleteBGPPeerResponse)

-- | The response's http status code.
deleteBGPPeerResponse_httpStatus :: Lens.Lens' DeleteBGPPeerResponse Core.Int
deleteBGPPeerResponse_httpStatus = Lens.lens (\DeleteBGPPeerResponse' {httpStatus} -> httpStatus) (\s@DeleteBGPPeerResponse' {} a -> s {httpStatus = a} :: DeleteBGPPeerResponse)

instance Core.NFData DeleteBGPPeerResponse
