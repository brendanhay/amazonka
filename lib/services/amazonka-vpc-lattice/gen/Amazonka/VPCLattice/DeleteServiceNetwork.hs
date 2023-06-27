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
-- Module      : Amazonka.VPCLattice.DeleteServiceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a service network. You can only delete the service network if
-- there is no service or VPC associated with it. If you delete a service
-- network, all resources related to the service network, such as the
-- resource policy, auth policy, and access log subscriptions, are also
-- deleted. For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html#delete-service-network Delete a service network>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.DeleteServiceNetwork
  ( -- * Creating a Request
    DeleteServiceNetwork (..),
    newDeleteServiceNetwork,

    -- * Request Lenses
    deleteServiceNetwork_serviceNetworkIdentifier,

    -- * Destructuring the Response
    DeleteServiceNetworkResponse (..),
    newDeleteServiceNetworkResponse,

    -- * Response Lenses
    deleteServiceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeleteServiceNetwork' smart constructor.
data DeleteServiceNetwork = DeleteServiceNetwork'
  { -- | The Amazon Resource Name (ARN) or ID of the service network.
    serviceNetworkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkIdentifier', 'deleteServiceNetwork_serviceNetworkIdentifier' - The Amazon Resource Name (ARN) or ID of the service network.
newDeleteServiceNetwork ::
  -- | 'serviceNetworkIdentifier'
  Prelude.Text ->
  DeleteServiceNetwork
newDeleteServiceNetwork pServiceNetworkIdentifier_ =
  DeleteServiceNetwork'
    { serviceNetworkIdentifier =
        pServiceNetworkIdentifier_
    }

-- | The Amazon Resource Name (ARN) or ID of the service network.
deleteServiceNetwork_serviceNetworkIdentifier :: Lens.Lens' DeleteServiceNetwork Prelude.Text
deleteServiceNetwork_serviceNetworkIdentifier = Lens.lens (\DeleteServiceNetwork' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@DeleteServiceNetwork' {} a -> s {serviceNetworkIdentifier = a} :: DeleteServiceNetwork)

instance Core.AWSRequest DeleteServiceNetwork where
  type
    AWSResponse DeleteServiceNetwork =
      DeleteServiceNetworkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServiceNetworkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServiceNetwork where
  hashWithSalt _salt DeleteServiceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` serviceNetworkIdentifier

instance Prelude.NFData DeleteServiceNetwork where
  rnf DeleteServiceNetwork' {..} =
    Prelude.rnf serviceNetworkIdentifier

instance Data.ToHeaders DeleteServiceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteServiceNetwork where
  toPath DeleteServiceNetwork' {..} =
    Prelude.mconcat
      [ "/servicenetworks/",
        Data.toBS serviceNetworkIdentifier
      ]

instance Data.ToQuery DeleteServiceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceNetworkResponse' smart constructor.
data DeleteServiceNetworkResponse = DeleteServiceNetworkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServiceNetworkResponse_httpStatus' - The response's http status code.
newDeleteServiceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceNetworkResponse
newDeleteServiceNetworkResponse pHttpStatus_ =
  DeleteServiceNetworkResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteServiceNetworkResponse_httpStatus :: Lens.Lens' DeleteServiceNetworkResponse Prelude.Int
deleteServiceNetworkResponse_httpStatus = Lens.lens (\DeleteServiceNetworkResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceNetworkResponse' {} a -> s {httpStatus = a} :: DeleteServiceNetworkResponse)

instance Prelude.NFData DeleteServiceNetworkResponse where
  rnf DeleteServiceNetworkResponse' {..} =
    Prelude.rnf httpStatus
