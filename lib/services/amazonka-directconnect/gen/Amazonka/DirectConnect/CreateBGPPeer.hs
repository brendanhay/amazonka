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
-- Module      : Amazonka.DirectConnect.CreateBGPPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a BGP peer on the specified virtual interface.
--
-- You must create a BGP peer for the corresponding address family
-- (IPv4\/IPv6) in order to access Amazon Web Services resources that also
-- use that address family.
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
module Amazonka.DirectConnect.CreateBGPPeer
  ( -- * Creating a Request
    CreateBGPPeer (..),
    newCreateBGPPeer,

    -- * Request Lenses
    createBGPPeer_newBGPPeer,
    createBGPPeer_virtualInterfaceId,

    -- * Destructuring the Response
    CreateBGPPeerResponse (..),
    newCreateBGPPeerResponse,

    -- * Response Lenses
    createBGPPeerResponse_virtualInterface,
    createBGPPeerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBGPPeer' smart constructor.
data CreateBGPPeer = CreateBGPPeer'
  { -- | Information about the BGP peer.
    newBGPPeer' :: Prelude.Maybe NewBGPPeer,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newBGPPeer'', 'createBGPPeer_newBGPPeer' - Information about the BGP peer.
--
-- 'virtualInterfaceId', 'createBGPPeer_virtualInterfaceId' - The ID of the virtual interface.
newCreateBGPPeer ::
  CreateBGPPeer
newCreateBGPPeer =
  CreateBGPPeer'
    { newBGPPeer' = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing
    }

-- | Information about the BGP peer.
createBGPPeer_newBGPPeer :: Lens.Lens' CreateBGPPeer (Prelude.Maybe NewBGPPeer)
createBGPPeer_newBGPPeer = Lens.lens (\CreateBGPPeer' {newBGPPeer'} -> newBGPPeer') (\s@CreateBGPPeer' {} a -> s {newBGPPeer' = a} :: CreateBGPPeer)

-- | The ID of the virtual interface.
createBGPPeer_virtualInterfaceId :: Lens.Lens' CreateBGPPeer (Prelude.Maybe Prelude.Text)
createBGPPeer_virtualInterfaceId = Lens.lens (\CreateBGPPeer' {virtualInterfaceId} -> virtualInterfaceId) (\s@CreateBGPPeer' {} a -> s {virtualInterfaceId = a} :: CreateBGPPeer)

instance Core.AWSRequest CreateBGPPeer where
  type
    AWSResponse CreateBGPPeer =
      CreateBGPPeerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBGPPeerResponse'
            Prelude.<$> (x Data..?> "virtualInterface")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBGPPeer where
  hashWithSalt _salt CreateBGPPeer' {..} =
    _salt
      `Prelude.hashWithSalt` newBGPPeer'
      `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData CreateBGPPeer where
  rnf CreateBGPPeer' {..} =
    Prelude.rnf newBGPPeer'
      `Prelude.seq` Prelude.rnf virtualInterfaceId

instance Data.ToHeaders CreateBGPPeer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.CreateBGPPeer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBGPPeer where
  toJSON CreateBGPPeer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("newBGPPeer" Data..=) Prelude.<$> newBGPPeer',
            ("virtualInterfaceId" Data..=)
              Prelude.<$> virtualInterfaceId
          ]
      )

instance Data.ToPath CreateBGPPeer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateBGPPeer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse = CreateBGPPeerResponse'
  { -- | The virtual interface.
    virtualInterface :: Prelude.Maybe VirtualInterface,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateBGPPeerResponse where
  rnf CreateBGPPeerResponse' {..} =
    Prelude.rnf virtualInterface
      `Prelude.seq` Prelude.rnf httpStatus
