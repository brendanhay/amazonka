{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBGPPeer' smart constructor.
data CreateBGPPeer = CreateBGPPeer'
  { -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | Information about the BGP peer.
    newBGPPeer' :: Prelude.Maybe NewBGPPeer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { virtualInterfaceId =
        Prelude.Nothing,
      newBGPPeer' = Prelude.Nothing
    }

-- | The ID of the virtual interface.
createBGPPeer_virtualInterfaceId :: Lens.Lens' CreateBGPPeer (Prelude.Maybe Prelude.Text)
createBGPPeer_virtualInterfaceId = Lens.lens (\CreateBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@CreateBGPPeer' {} a -> s {virtualInterfaceId = a} :: CreateBGPPeer)

-- | Information about the BGP peer.
createBGPPeer_newBGPPeer :: Lens.Lens' CreateBGPPeer (Prelude.Maybe NewBGPPeer)
createBGPPeer_newBGPPeer = Lens.lens (\CreateBGPPeer' {newBGPPeer'} -> newBGPPeer') (\s@CreateBGPPeer' {} a -> s {newBGPPeer' = a} :: CreateBGPPeer)

instance Prelude.AWSRequest CreateBGPPeer where
  type Rs CreateBGPPeer = CreateBGPPeerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBGPPeerResponse'
            Prelude.<$> (x Prelude..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBGPPeer

instance Prelude.NFData CreateBGPPeer

instance Prelude.ToHeaders CreateBGPPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.CreateBGPPeer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateBGPPeer where
  toJSON CreateBGPPeer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("virtualInterfaceId" Prelude..=)
              Prelude.<$> virtualInterfaceId,
            ("newBGPPeer" Prelude..=) Prelude.<$> newBGPPeer'
          ]
      )

instance Prelude.ToPath CreateBGPPeer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateBGPPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse = CreateBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Prelude.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateBGPPeerResponse
newCreateBGPPeerResponse pHttpStatus_ =
  CreateBGPPeerResponse'
    { virtualInterface =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual interface.
createBGPPeerResponse_virtualInterface :: Lens.Lens' CreateBGPPeerResponse (Prelude.Maybe VirtualInterface)
createBGPPeerResponse_virtualInterface = Lens.lens (\CreateBGPPeerResponse' {virtualInterface} -> virtualInterface) (\s@CreateBGPPeerResponse' {} a -> s {virtualInterface = a} :: CreateBGPPeerResponse)

-- | The response's http status code.
createBGPPeerResponse_httpStatus :: Lens.Lens' CreateBGPPeerResponse Prelude.Int
createBGPPeerResponse_httpStatus = Lens.lens (\CreateBGPPeerResponse' {httpStatus} -> httpStatus) (\s@CreateBGPPeerResponse' {} a -> s {httpStatus = a} :: CreateBGPPeerResponse)

instance Prelude.NFData CreateBGPPeerResponse
