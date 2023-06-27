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
-- Module      : Amazonka.VPCLattice.GetServiceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified service network.
module Amazonka.VPCLattice.GetServiceNetwork
  ( -- * Creating a Request
    GetServiceNetwork (..),
    newGetServiceNetwork,

    -- * Request Lenses
    getServiceNetwork_serviceNetworkIdentifier,

    -- * Destructuring the Response
    GetServiceNetworkResponse (..),
    newGetServiceNetworkResponse,

    -- * Response Lenses
    getServiceNetworkResponse_arn,
    getServiceNetworkResponse_authType,
    getServiceNetworkResponse_createdAt,
    getServiceNetworkResponse_id,
    getServiceNetworkResponse_lastUpdatedAt,
    getServiceNetworkResponse_name,
    getServiceNetworkResponse_numberOfAssociatedServices,
    getServiceNetworkResponse_numberOfAssociatedVPCs,
    getServiceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetServiceNetwork' smart constructor.
data GetServiceNetwork = GetServiceNetwork'
  { -- | The ID or Amazon Resource Name (ARN) of the service network.
    serviceNetworkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkIdentifier', 'getServiceNetwork_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network.
newGetServiceNetwork ::
  -- | 'serviceNetworkIdentifier'
  Prelude.Text ->
  GetServiceNetwork
newGetServiceNetwork pServiceNetworkIdentifier_ =
  GetServiceNetwork'
    { serviceNetworkIdentifier =
        pServiceNetworkIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the service network.
getServiceNetwork_serviceNetworkIdentifier :: Lens.Lens' GetServiceNetwork Prelude.Text
getServiceNetwork_serviceNetworkIdentifier = Lens.lens (\GetServiceNetwork' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@GetServiceNetwork' {} a -> s {serviceNetworkIdentifier = a} :: GetServiceNetwork)

instance Core.AWSRequest GetServiceNetwork where
  type
    AWSResponse GetServiceNetwork =
      GetServiceNetworkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceNetworkResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "numberOfAssociatedServices")
            Prelude.<*> (x Data..?> "numberOfAssociatedVPCs")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceNetwork where
  hashWithSalt _salt GetServiceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` serviceNetworkIdentifier

instance Prelude.NFData GetServiceNetwork where
  rnf GetServiceNetwork' {..} =
    Prelude.rnf serviceNetworkIdentifier

instance Data.ToHeaders GetServiceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetServiceNetwork where
  toPath GetServiceNetwork' {..} =
    Prelude.mconcat
      [ "/servicenetworks/",
        Data.toBS serviceNetworkIdentifier
      ]

instance Data.ToQuery GetServiceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceNetworkResponse' smart constructor.
data GetServiceNetworkResponse = GetServiceNetworkResponse'
  { -- | The Amazon Resource Name (ARN) of the service network.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of IAM policy.
    authType :: Prelude.Maybe AuthType,
    -- | The date and time that the service network was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the service network.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the last update, specified in ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the service network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of services associated with the service network.
    numberOfAssociatedServices :: Prelude.Maybe Prelude.Integer,
    -- | The number of VPCs associated with the service network.
    numberOfAssociatedVPCs :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getServiceNetworkResponse_arn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'authType', 'getServiceNetworkResponse_authType' - The type of IAM policy.
--
-- 'createdAt', 'getServiceNetworkResponse_createdAt' - The date and time that the service network was created, specified in
-- ISO-8601 format.
--
-- 'id', 'getServiceNetworkResponse_id' - The ID of the service network.
--
-- 'lastUpdatedAt', 'getServiceNetworkResponse_lastUpdatedAt' - The date and time of the last update, specified in ISO-8601 format.
--
-- 'name', 'getServiceNetworkResponse_name' - The name of the service network.
--
-- 'numberOfAssociatedServices', 'getServiceNetworkResponse_numberOfAssociatedServices' - The number of services associated with the service network.
--
-- 'numberOfAssociatedVPCs', 'getServiceNetworkResponse_numberOfAssociatedVPCs' - The number of VPCs associated with the service network.
--
-- 'httpStatus', 'getServiceNetworkResponse_httpStatus' - The response's http status code.
newGetServiceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceNetworkResponse
newGetServiceNetworkResponse pHttpStatus_ =
  GetServiceNetworkResponse'
    { arn = Prelude.Nothing,
      authType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      numberOfAssociatedServices = Prelude.Nothing,
      numberOfAssociatedVPCs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service network.
getServiceNetworkResponse_arn :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkResponse_arn = Lens.lens (\GetServiceNetworkResponse' {arn} -> arn) (\s@GetServiceNetworkResponse' {} a -> s {arn = a} :: GetServiceNetworkResponse)

-- | The type of IAM policy.
getServiceNetworkResponse_authType :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe AuthType)
getServiceNetworkResponse_authType = Lens.lens (\GetServiceNetworkResponse' {authType} -> authType) (\s@GetServiceNetworkResponse' {} a -> s {authType = a} :: GetServiceNetworkResponse)

-- | The date and time that the service network was created, specified in
-- ISO-8601 format.
getServiceNetworkResponse_createdAt :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.UTCTime)
getServiceNetworkResponse_createdAt = Lens.lens (\GetServiceNetworkResponse' {createdAt} -> createdAt) (\s@GetServiceNetworkResponse' {} a -> s {createdAt = a} :: GetServiceNetworkResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the service network.
getServiceNetworkResponse_id :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkResponse_id = Lens.lens (\GetServiceNetworkResponse' {id} -> id) (\s@GetServiceNetworkResponse' {} a -> s {id = a} :: GetServiceNetworkResponse)

-- | The date and time of the last update, specified in ISO-8601 format.
getServiceNetworkResponse_lastUpdatedAt :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.UTCTime)
getServiceNetworkResponse_lastUpdatedAt = Lens.lens (\GetServiceNetworkResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetServiceNetworkResponse' {} a -> s {lastUpdatedAt = a} :: GetServiceNetworkResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the service network.
getServiceNetworkResponse_name :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkResponse_name = Lens.lens (\GetServiceNetworkResponse' {name} -> name) (\s@GetServiceNetworkResponse' {} a -> s {name = a} :: GetServiceNetworkResponse)

-- | The number of services associated with the service network.
getServiceNetworkResponse_numberOfAssociatedServices :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.Integer)
getServiceNetworkResponse_numberOfAssociatedServices = Lens.lens (\GetServiceNetworkResponse' {numberOfAssociatedServices} -> numberOfAssociatedServices) (\s@GetServiceNetworkResponse' {} a -> s {numberOfAssociatedServices = a} :: GetServiceNetworkResponse)

-- | The number of VPCs associated with the service network.
getServiceNetworkResponse_numberOfAssociatedVPCs :: Lens.Lens' GetServiceNetworkResponse (Prelude.Maybe Prelude.Integer)
getServiceNetworkResponse_numberOfAssociatedVPCs = Lens.lens (\GetServiceNetworkResponse' {numberOfAssociatedVPCs} -> numberOfAssociatedVPCs) (\s@GetServiceNetworkResponse' {} a -> s {numberOfAssociatedVPCs = a} :: GetServiceNetworkResponse)

-- | The response's http status code.
getServiceNetworkResponse_httpStatus :: Lens.Lens' GetServiceNetworkResponse Prelude.Int
getServiceNetworkResponse_httpStatus = Lens.lens (\GetServiceNetworkResponse' {httpStatus} -> httpStatus) (\s@GetServiceNetworkResponse' {} a -> s {httpStatus = a} :: GetServiceNetworkResponse)

instance Prelude.NFData GetServiceNetworkResponse where
  rnf GetServiceNetworkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberOfAssociatedServices
      `Prelude.seq` Prelude.rnf numberOfAssociatedVPCs
      `Prelude.seq` Prelude.rnf httpStatus
