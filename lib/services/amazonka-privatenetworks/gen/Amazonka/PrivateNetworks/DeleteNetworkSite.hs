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
-- Module      : Amazonka.PrivateNetworks.DeleteNetworkSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network site. Return the hardware after you delete
-- the network site. You are responsible for minimum charges. For more
-- information, see
-- <https://docs.aws.amazon.com/private-networks/latest/userguide/hardware-maintenance.html Hardware returns>
-- in the /Amazon Web Services Private 5G User Guide/.
module Amazonka.PrivateNetworks.DeleteNetworkSite
  ( -- * Creating a Request
    DeleteNetworkSite (..),
    newDeleteNetworkSite,

    -- * Request Lenses
    deleteNetworkSite_clientToken,
    deleteNetworkSite_networkSiteArn,

    -- * Destructuring the Response
    DeleteNetworkSiteResponse (..),
    newDeleteNetworkSiteResponse,

    -- * Response Lenses
    deleteNetworkSiteResponse_networkSite,
    deleteNetworkSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkSite' smart constructor.
data DeleteNetworkSite = DeleteNetworkSite'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network site.
    networkSiteArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteNetworkSite_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'networkSiteArn', 'deleteNetworkSite_networkSiteArn' - The Amazon Resource Name (ARN) of the network site.
newDeleteNetworkSite ::
  -- | 'networkSiteArn'
  Prelude.Text ->
  DeleteNetworkSite
newDeleteNetworkSite pNetworkSiteArn_ =
  DeleteNetworkSite'
    { clientToken = Prelude.Nothing,
      networkSiteArn = pNetworkSiteArn_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
deleteNetworkSite_clientToken :: Lens.Lens' DeleteNetworkSite (Prelude.Maybe Prelude.Text)
deleteNetworkSite_clientToken = Lens.lens (\DeleteNetworkSite' {clientToken} -> clientToken) (\s@DeleteNetworkSite' {} a -> s {clientToken = a} :: DeleteNetworkSite)

-- | The Amazon Resource Name (ARN) of the network site.
deleteNetworkSite_networkSiteArn :: Lens.Lens' DeleteNetworkSite Prelude.Text
deleteNetworkSite_networkSiteArn = Lens.lens (\DeleteNetworkSite' {networkSiteArn} -> networkSiteArn) (\s@DeleteNetworkSite' {} a -> s {networkSiteArn = a} :: DeleteNetworkSite)

instance Core.AWSRequest DeleteNetworkSite where
  type
    AWSResponse DeleteNetworkSite =
      DeleteNetworkSiteResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteNetworkSiteResponse'
            Prelude.<$> (x Data..?> "networkSite")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNetworkSite where
  hashWithSalt _salt DeleteNetworkSite' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` networkSiteArn

instance Prelude.NFData DeleteNetworkSite where
  rnf DeleteNetworkSite' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf networkSiteArn

instance Data.ToHeaders DeleteNetworkSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteNetworkSite where
  toPath DeleteNetworkSite' {..} =
    Prelude.mconcat
      ["/v1/network-sites/", Data.toBS networkSiteArn]

instance Data.ToQuery DeleteNetworkSite where
  toQuery DeleteNetworkSite' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteNetworkSiteResponse' smart constructor.
data DeleteNetworkSiteResponse = DeleteNetworkSiteResponse'
  { -- | Information about the network site.
    networkSite :: Prelude.Maybe NetworkSite,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSite', 'deleteNetworkSiteResponse_networkSite' - Information about the network site.
--
-- 'httpStatus', 'deleteNetworkSiteResponse_httpStatus' - The response's http status code.
newDeleteNetworkSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkSiteResponse
newDeleteNetworkSiteResponse pHttpStatus_ =
  DeleteNetworkSiteResponse'
    { networkSite =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network site.
deleteNetworkSiteResponse_networkSite :: Lens.Lens' DeleteNetworkSiteResponse (Prelude.Maybe NetworkSite)
deleteNetworkSiteResponse_networkSite = Lens.lens (\DeleteNetworkSiteResponse' {networkSite} -> networkSite) (\s@DeleteNetworkSiteResponse' {} a -> s {networkSite = a} :: DeleteNetworkSiteResponse)

-- | The response's http status code.
deleteNetworkSiteResponse_httpStatus :: Lens.Lens' DeleteNetworkSiteResponse Prelude.Int
deleteNetworkSiteResponse_httpStatus = Lens.lens (\DeleteNetworkSiteResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkSiteResponse' {} a -> s {httpStatus = a} :: DeleteNetworkSiteResponse)

instance Prelude.NFData DeleteNetworkSiteResponse where
  rnf DeleteNetworkSiteResponse' {..} =
    Prelude.rnf networkSite
      `Prelude.seq` Prelude.rnf httpStatus
