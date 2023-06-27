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
-- Module      : Amazonka.PrivateNetworks.DeleteNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network. You must delete network sites before you
-- delete the network. For more information, see
-- <https://docs.aws.amazon.com/private-networks/latest/APIReference/API_DeleteNetworkSite.html DeleteNetworkSite>
-- in the /API Reference for Amazon Web Services Private 5G/.
module Amazonka.PrivateNetworks.DeleteNetwork
  ( -- * Creating a Request
    DeleteNetwork (..),
    newDeleteNetwork,

    -- * Request Lenses
    deleteNetwork_clientToken,
    deleteNetwork_networkArn,

    -- * Destructuring the Response
    DeleteNetworkResponse (..),
    newDeleteNetworkResponse,

    -- * Response Lenses
    deleteNetworkResponse_httpStatus,
    deleteNetworkResponse_network,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetwork' smart constructor.
data DeleteNetwork = DeleteNetwork'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteNetwork_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'networkArn', 'deleteNetwork_networkArn' - The Amazon Resource Name (ARN) of the network.
newDeleteNetwork ::
  -- | 'networkArn'
  Prelude.Text ->
  DeleteNetwork
newDeleteNetwork pNetworkArn_ =
  DeleteNetwork'
    { clientToken = Prelude.Nothing,
      networkArn = pNetworkArn_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
deleteNetwork_clientToken :: Lens.Lens' DeleteNetwork (Prelude.Maybe Prelude.Text)
deleteNetwork_clientToken = Lens.lens (\DeleteNetwork' {clientToken} -> clientToken) (\s@DeleteNetwork' {} a -> s {clientToken = a} :: DeleteNetwork)

-- | The Amazon Resource Name (ARN) of the network.
deleteNetwork_networkArn :: Lens.Lens' DeleteNetwork Prelude.Text
deleteNetwork_networkArn = Lens.lens (\DeleteNetwork' {networkArn} -> networkArn) (\s@DeleteNetwork' {} a -> s {networkArn = a} :: DeleteNetwork)

instance Core.AWSRequest DeleteNetwork where
  type
    AWSResponse DeleteNetwork =
      DeleteNetworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNetworkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "network")
      )

instance Prelude.Hashable DeleteNetwork where
  hashWithSalt _salt DeleteNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` networkArn

instance Prelude.NFData DeleteNetwork where
  rnf DeleteNetwork' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf networkArn

instance Data.ToHeaders DeleteNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteNetwork where
  toPath DeleteNetwork' {..} =
    Prelude.mconcat
      ["/v1/networks/", Data.toBS networkArn]

instance Data.ToQuery DeleteNetwork where
  toQuery DeleteNetwork' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteNetworkResponse' smart constructor.
data DeleteNetworkResponse = DeleteNetworkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the network.
    network :: Network
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNetworkResponse_httpStatus' - The response's http status code.
--
-- 'network', 'deleteNetworkResponse_network' - Information about the network.
newDeleteNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'network'
  Network ->
  DeleteNetworkResponse
newDeleteNetworkResponse pHttpStatus_ pNetwork_ =
  DeleteNetworkResponse'
    { httpStatus = pHttpStatus_,
      network = pNetwork_
    }

-- | The response's http status code.
deleteNetworkResponse_httpStatus :: Lens.Lens' DeleteNetworkResponse Prelude.Int
deleteNetworkResponse_httpStatus = Lens.lens (\DeleteNetworkResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkResponse' {} a -> s {httpStatus = a} :: DeleteNetworkResponse)

-- | Information about the network.
deleteNetworkResponse_network :: Lens.Lens' DeleteNetworkResponse Network
deleteNetworkResponse_network = Lens.lens (\DeleteNetworkResponse' {network} -> network) (\s@DeleteNetworkResponse' {} a -> s {network = a} :: DeleteNetworkResponse)

instance Prelude.NFData DeleteNetworkResponse where
  rnf DeleteNetworkResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf network
