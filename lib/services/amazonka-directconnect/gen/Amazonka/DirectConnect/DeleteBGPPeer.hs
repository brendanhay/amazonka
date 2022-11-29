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
-- Module      : Amazonka.DirectConnect.DeleteBGPPeer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified BGP peer on the specified virtual interface with
-- the specified customer address and ASN.
--
-- You cannot delete the last BGP peer from a virtual interface.
module Amazonka.DirectConnect.DeleteBGPPeer
  ( -- * Creating a Request
    DeleteBGPPeer (..),
    newDeleteBGPPeer,

    -- * Request Lenses
    deleteBGPPeer_bgpPeerId,
    deleteBGPPeer_customerAddress,
    deleteBGPPeer_asn,
    deleteBGPPeer_virtualInterfaceId,

    -- * Destructuring the Response
    DeleteBGPPeerResponse (..),
    newDeleteBGPPeerResponse,

    -- * Response Lenses
    deleteBGPPeerResponse_virtualInterface,
    deleteBGPPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBGPPeer' smart constructor.
data DeleteBGPPeer = DeleteBGPPeer'
  { -- | The ID of the BGP peer.
    bgpPeerId :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text
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
-- 'bgpPeerId', 'deleteBGPPeer_bgpPeerId' - The ID of the BGP peer.
--
-- 'customerAddress', 'deleteBGPPeer_customerAddress' - The IP address assigned to the customer interface.
--
-- 'asn', 'deleteBGPPeer_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- 'virtualInterfaceId', 'deleteBGPPeer_virtualInterfaceId' - The ID of the virtual interface.
newDeleteBGPPeer ::
  DeleteBGPPeer
newDeleteBGPPeer =
  DeleteBGPPeer'
    { bgpPeerId = Prelude.Nothing,
      customerAddress = Prelude.Nothing,
      asn = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing
    }

-- | The ID of the BGP peer.
deleteBGPPeer_bgpPeerId :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
deleteBGPPeer_bgpPeerId = Lens.lens (\DeleteBGPPeer' {bgpPeerId} -> bgpPeerId) (\s@DeleteBGPPeer' {} a -> s {bgpPeerId = a} :: DeleteBGPPeer)

-- | The IP address assigned to the customer interface.
deleteBGPPeer_customerAddress :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
deleteBGPPeer_customerAddress = Lens.lens (\DeleteBGPPeer' {customerAddress} -> customerAddress) (\s@DeleteBGPPeer' {} a -> s {customerAddress = a} :: DeleteBGPPeer)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
deleteBGPPeer_asn :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Int)
deleteBGPPeer_asn = Lens.lens (\DeleteBGPPeer' {asn} -> asn) (\s@DeleteBGPPeer' {} a -> s {asn = a} :: DeleteBGPPeer)

-- | The ID of the virtual interface.
deleteBGPPeer_virtualInterfaceId :: Lens.Lens' DeleteBGPPeer (Prelude.Maybe Prelude.Text)
deleteBGPPeer_virtualInterfaceId = Lens.lens (\DeleteBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@DeleteBGPPeer' {} a -> s {virtualInterfaceId = a} :: DeleteBGPPeer)

instance Core.AWSRequest DeleteBGPPeer where
  type
    AWSResponse DeleteBGPPeer =
      DeleteBGPPeerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBGPPeerResponse'
            Prelude.<$> (x Core..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBGPPeer where
  hashWithSalt _salt DeleteBGPPeer' {..} =
    _salt `Prelude.hashWithSalt` bgpPeerId
      `Prelude.hashWithSalt` customerAddress
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData DeleteBGPPeer where
  rnf DeleteBGPPeer' {..} =
    Prelude.rnf bgpPeerId
      `Prelude.seq` Prelude.rnf customerAddress
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf virtualInterfaceId

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
          [ ("bgpPeerId" Core..=) Prelude.<$> bgpPeerId,
            ("customerAddress" Core..=)
              Prelude.<$> customerAddress,
            ("asn" Core..=) Prelude.<$> asn,
            ("virtualInterfaceId" Core..=)
              Prelude.<$> virtualInterfaceId
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

instance Prelude.NFData DeleteBGPPeerResponse where
  rnf DeleteBGPPeerResponse' {..} =
    Prelude.rnf virtualInterface
      `Prelude.seq` Prelude.rnf httpStatus
