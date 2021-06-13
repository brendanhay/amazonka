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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBGPPeer' smart constructor.
data DeleteBGPPeer = DeleteBGPPeer'
  { -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The ID of the BGP peer.
    bgpPeerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { asn = Prelude.Nothing,
      bgpPeerId = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      customerAddress = Prelude.Nothing
    }

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
deleteBGPPeer_asn :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Int)
deleteBGPPeer_asn = Lens.lens (\DeleteBGPPeer' {asn} -> asn) (\s@DeleteBGPPeer' {} a -> s {asn = a} :: DeleteBGPPeer)

-- | The ID of the BGP peer.
deleteBGPPeer_bgpPeerId :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
deleteBGPPeer_bgpPeerId = Lens.lens (\DeleteBGPPeer' {bgpPeerId} -> bgpPeerId) (\s@DeleteBGPPeer' {} a -> s {bgpPeerId = a} :: DeleteBGPPeer)

-- | The ID of the virtual interface.
deleteBGPPeer_virtualInterfaceId :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
deleteBGPPeer_virtualInterfaceId = Lens.lens (\DeleteBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@DeleteBGPPeer' {} a -> s {virtualInterfaceId = a} :: DeleteBGPPeer)

-- | The IP address assigned to the customer interface.
deleteBGPPeer_customerAddress :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBGPPeer

instance Prelude.NFData DeleteBGPPeer

instance Core.ToHeaders DeleteBGPPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteBGPPeer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteBGPPeer where
  toJSON DeleteBGPPeer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("asn" Core..=) Prelude.<$> asn,
            ("bgpPeerId" Core..=) Prelude.<$> bgpPeerId,
            ("virtualInterfaceId" Core..=)
              Prelude.<$> virtualInterfaceId,
            ("customerAddress" Core..=)
              Prelude.<$> customerAddress
          ]
      )

instance Core.ToPath DeleteBGPPeer where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteBGPPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBGPPeerResponse' smart constructor.
data DeleteBGPPeerResponse = DeleteBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Prelude.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteBGPPeerResponse
newDeleteBGPPeerResponse pHttpStatus_ =
  DeleteBGPPeerResponse'
    { virtualInterface =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual interface.
deleteBGPPeerResponse_virtualInterface :: Lens.Lens' DeleteBGPPeerResponse (Prelude.Maybe VirtualInterface)
deleteBGPPeerResponse_virtualInterface = Lens.lens (\DeleteBGPPeerResponse' {virtualInterface} -> virtualInterface) (\s@DeleteBGPPeerResponse' {} a -> s {virtualInterface = a} :: DeleteBGPPeerResponse)

-- | The response's http status code.
deleteBGPPeerResponse_httpStatus :: Lens.Lens' DeleteBGPPeerResponse Prelude.Int
deleteBGPPeerResponse_httpStatus = Lens.lens (\DeleteBGPPeerResponse' {httpStatus} -> httpStatus) (\s@DeleteBGPPeerResponse' {} a -> s {httpStatus = a} :: DeleteBGPPeerResponse)

instance Prelude.NFData DeleteBGPPeerResponse
