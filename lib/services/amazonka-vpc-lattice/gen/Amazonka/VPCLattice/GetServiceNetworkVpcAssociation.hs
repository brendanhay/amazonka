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
-- Module      : Amazonka.VPCLattice.GetServiceNetworkVpcAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the association between a service network
-- and a VPC.
module Amazonka.VPCLattice.GetServiceNetworkVpcAssociation
  ( -- * Creating a Request
    GetServiceNetworkVpcAssociation (..),
    newGetServiceNetworkVpcAssociation,

    -- * Request Lenses
    getServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier,

    -- * Destructuring the Response
    GetServiceNetworkVpcAssociationResponse (..),
    newGetServiceNetworkVpcAssociationResponse,

    -- * Response Lenses
    getServiceNetworkVpcAssociationResponse_arn,
    getServiceNetworkVpcAssociationResponse_createdAt,
    getServiceNetworkVpcAssociationResponse_createdBy,
    getServiceNetworkVpcAssociationResponse_failureCode,
    getServiceNetworkVpcAssociationResponse_failureMessage,
    getServiceNetworkVpcAssociationResponse_id,
    getServiceNetworkVpcAssociationResponse_lastUpdatedAt,
    getServiceNetworkVpcAssociationResponse_securityGroupIds,
    getServiceNetworkVpcAssociationResponse_serviceNetworkArn,
    getServiceNetworkVpcAssociationResponse_serviceNetworkId,
    getServiceNetworkVpcAssociationResponse_serviceNetworkName,
    getServiceNetworkVpcAssociationResponse_status,
    getServiceNetworkVpcAssociationResponse_vpcId,
    getServiceNetworkVpcAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetServiceNetworkVpcAssociation' smart constructor.
data GetServiceNetworkVpcAssociation = GetServiceNetworkVpcAssociation'
  { -- | The ID or Amazon Resource Name (ARN) of the association.
    serviceNetworkVpcAssociationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetworkVpcAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkVpcAssociationIdentifier', 'getServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier' - The ID or Amazon Resource Name (ARN) of the association.
newGetServiceNetworkVpcAssociation ::
  -- | 'serviceNetworkVpcAssociationIdentifier'
  Prelude.Text ->
  GetServiceNetworkVpcAssociation
newGetServiceNetworkVpcAssociation
  pServiceNetworkVpcAssociationIdentifier_ =
    GetServiceNetworkVpcAssociation'
      { serviceNetworkVpcAssociationIdentifier =
          pServiceNetworkVpcAssociationIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the association.
getServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier :: Lens.Lens' GetServiceNetworkVpcAssociation Prelude.Text
getServiceNetworkVpcAssociation_serviceNetworkVpcAssociationIdentifier = Lens.lens (\GetServiceNetworkVpcAssociation' {serviceNetworkVpcAssociationIdentifier} -> serviceNetworkVpcAssociationIdentifier) (\s@GetServiceNetworkVpcAssociation' {} a -> s {serviceNetworkVpcAssociationIdentifier = a} :: GetServiceNetworkVpcAssociation)

instance
  Core.AWSRequest
    GetServiceNetworkVpcAssociation
  where
  type
    AWSResponse GetServiceNetworkVpcAssociation =
      GetServiceNetworkVpcAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceNetworkVpcAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "createdBy")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "failureMessage")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> ( x
                            Data..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "serviceNetworkArn")
            Prelude.<*> (x Data..?> "serviceNetworkId")
            Prelude.<*> (x Data..?> "serviceNetworkName")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "vpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetServiceNetworkVpcAssociation
  where
  hashWithSalt
    _salt
    GetServiceNetworkVpcAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` serviceNetworkVpcAssociationIdentifier

instance
  Prelude.NFData
    GetServiceNetworkVpcAssociation
  where
  rnf GetServiceNetworkVpcAssociation' {..} =
    Prelude.rnf serviceNetworkVpcAssociationIdentifier

instance
  Data.ToHeaders
    GetServiceNetworkVpcAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetServiceNetworkVpcAssociation where
  toPath GetServiceNetworkVpcAssociation' {..} =
    Prelude.mconcat
      [ "/servicenetworkvpcassociations/",
        Data.toBS serviceNetworkVpcAssociationIdentifier
      ]

instance Data.ToQuery GetServiceNetworkVpcAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceNetworkVpcAssociationResponse' smart constructor.
data GetServiceNetworkVpcAssociationResponse = GetServiceNetworkVpcAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the specified association between the service network and the
    -- VPC.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The IDs of the security groups.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the service network.
    serviceNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service network.
    serviceNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service network.
    serviceNetworkName :: Prelude.Maybe Prelude.Text,
    -- | The status of the association.
    status :: Prelude.Maybe ServiceNetworkVpcAssociationStatus,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetworkVpcAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getServiceNetworkVpcAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdAt', 'getServiceNetworkVpcAssociationResponse_createdAt' - The date and time that the association was created, specified in
-- ISO-8601 format.
--
-- 'createdBy', 'getServiceNetworkVpcAssociationResponse_createdBy' - The account that created the association.
--
-- 'failureCode', 'getServiceNetworkVpcAssociationResponse_failureCode' - The failure code.
--
-- 'failureMessage', 'getServiceNetworkVpcAssociationResponse_failureMessage' - The failure message.
--
-- 'id', 'getServiceNetworkVpcAssociationResponse_id' - The ID of the specified association between the service network and the
-- VPC.
--
-- 'lastUpdatedAt', 'getServiceNetworkVpcAssociationResponse_lastUpdatedAt' - The date and time that the association was last updated, specified in
-- ISO-8601 format.
--
-- 'securityGroupIds', 'getServiceNetworkVpcAssociationResponse_securityGroupIds' - The IDs of the security groups.
--
-- 'serviceNetworkArn', 'getServiceNetworkVpcAssociationResponse_serviceNetworkArn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'serviceNetworkId', 'getServiceNetworkVpcAssociationResponse_serviceNetworkId' - The ID of the service network.
--
-- 'serviceNetworkName', 'getServiceNetworkVpcAssociationResponse_serviceNetworkName' - The name of the service network.
--
-- 'status', 'getServiceNetworkVpcAssociationResponse_status' - The status of the association.
--
-- 'vpcId', 'getServiceNetworkVpcAssociationResponse_vpcId' - The ID of the VPC.
--
-- 'httpStatus', 'getServiceNetworkVpcAssociationResponse_httpStatus' - The response's http status code.
newGetServiceNetworkVpcAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceNetworkVpcAssociationResponse
newGetServiceNetworkVpcAssociationResponse
  pHttpStatus_ =
    GetServiceNetworkVpcAssociationResponse'
      { arn =
          Prelude.Nothing,
        createdAt = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        failureCode = Prelude.Nothing,
        failureMessage = Prelude.Nothing,
        id = Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        serviceNetworkArn =
          Prelude.Nothing,
        serviceNetworkId = Prelude.Nothing,
        serviceNetworkName =
          Prelude.Nothing,
        status = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
getServiceNetworkVpcAssociationResponse_arn :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_arn = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {arn} -> arn) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {arn = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The date and time that the association was created, specified in
-- ISO-8601 format.
getServiceNetworkVpcAssociationResponse_createdAt :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.UTCTime)
getServiceNetworkVpcAssociationResponse_createdAt = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {createdAt} -> createdAt) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {createdAt = a} :: GetServiceNetworkVpcAssociationResponse) Prelude.. Lens.mapping Data._Time

-- | The account that created the association.
getServiceNetworkVpcAssociationResponse_createdBy :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_createdBy = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {createdBy} -> createdBy) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {createdBy = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The failure code.
getServiceNetworkVpcAssociationResponse_failureCode :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_failureCode = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {failureCode} -> failureCode) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {failureCode = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The failure message.
getServiceNetworkVpcAssociationResponse_failureMessage :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_failureMessage = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {failureMessage} -> failureMessage) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {failureMessage = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The ID of the specified association between the service network and the
-- VPC.
getServiceNetworkVpcAssociationResponse_id :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_id = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {id} -> id) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {id = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The date and time that the association was last updated, specified in
-- ISO-8601 format.
getServiceNetworkVpcAssociationResponse_lastUpdatedAt :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.UTCTime)
getServiceNetworkVpcAssociationResponse_lastUpdatedAt = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {lastUpdatedAt = a} :: GetServiceNetworkVpcAssociationResponse) Prelude.. Lens.mapping Data._Time

-- | The IDs of the security groups.
getServiceNetworkVpcAssociationResponse_securityGroupIds :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe [Prelude.Text])
getServiceNetworkVpcAssociationResponse_securityGroupIds = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {securityGroupIds} -> securityGroupIds) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {securityGroupIds = a} :: GetServiceNetworkVpcAssociationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the service network.
getServiceNetworkVpcAssociationResponse_serviceNetworkArn :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_serviceNetworkArn = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {serviceNetworkArn} -> serviceNetworkArn) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {serviceNetworkArn = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The ID of the service network.
getServiceNetworkVpcAssociationResponse_serviceNetworkId :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_serviceNetworkId = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {serviceNetworkId} -> serviceNetworkId) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {serviceNetworkId = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The name of the service network.
getServiceNetworkVpcAssociationResponse_serviceNetworkName :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_serviceNetworkName = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {serviceNetworkName} -> serviceNetworkName) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {serviceNetworkName = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The status of the association.
getServiceNetworkVpcAssociationResponse_status :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe ServiceNetworkVpcAssociationStatus)
getServiceNetworkVpcAssociationResponse_status = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {status} -> status) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {status = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The ID of the VPC.
getServiceNetworkVpcAssociationResponse_vpcId :: Lens.Lens' GetServiceNetworkVpcAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkVpcAssociationResponse_vpcId = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {vpcId} -> vpcId) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {vpcId = a} :: GetServiceNetworkVpcAssociationResponse)

-- | The response's http status code.
getServiceNetworkVpcAssociationResponse_httpStatus :: Lens.Lens' GetServiceNetworkVpcAssociationResponse Prelude.Int
getServiceNetworkVpcAssociationResponse_httpStatus = Lens.lens (\GetServiceNetworkVpcAssociationResponse' {httpStatus} -> httpStatus) (\s@GetServiceNetworkVpcAssociationResponse' {} a -> s {httpStatus = a} :: GetServiceNetworkVpcAssociationResponse)

instance
  Prelude.NFData
    GetServiceNetworkVpcAssociationResponse
  where
  rnf GetServiceNetworkVpcAssociationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf serviceNetworkArn
      `Prelude.seq` Prelude.rnf serviceNetworkId
      `Prelude.seq` Prelude.rnf serviceNetworkName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf httpStatus
