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
-- Module      : Amazonka.VPCLattice.GetServiceNetworkServiceAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified association between a service
-- network and a service.
module Amazonka.VPCLattice.GetServiceNetworkServiceAssociation
  ( -- * Creating a Request
    GetServiceNetworkServiceAssociation (..),
    newGetServiceNetworkServiceAssociation,

    -- * Request Lenses
    getServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier,

    -- * Destructuring the Response
    GetServiceNetworkServiceAssociationResponse (..),
    newGetServiceNetworkServiceAssociationResponse,

    -- * Response Lenses
    getServiceNetworkServiceAssociationResponse_arn,
    getServiceNetworkServiceAssociationResponse_createdAt,
    getServiceNetworkServiceAssociationResponse_createdBy,
    getServiceNetworkServiceAssociationResponse_customDomainName,
    getServiceNetworkServiceAssociationResponse_dnsEntry,
    getServiceNetworkServiceAssociationResponse_failureCode,
    getServiceNetworkServiceAssociationResponse_failureMessage,
    getServiceNetworkServiceAssociationResponse_id,
    getServiceNetworkServiceAssociationResponse_serviceArn,
    getServiceNetworkServiceAssociationResponse_serviceId,
    getServiceNetworkServiceAssociationResponse_serviceName,
    getServiceNetworkServiceAssociationResponse_serviceNetworkArn,
    getServiceNetworkServiceAssociationResponse_serviceNetworkId,
    getServiceNetworkServiceAssociationResponse_serviceNetworkName,
    getServiceNetworkServiceAssociationResponse_status,
    getServiceNetworkServiceAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetServiceNetworkServiceAssociation' smart constructor.
data GetServiceNetworkServiceAssociation = GetServiceNetworkServiceAssociation'
  { -- | The ID or Amazon Resource Name (ARN) of the association.
    serviceNetworkServiceAssociationIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetworkServiceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNetworkServiceAssociationIdentifier', 'getServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier' - The ID or Amazon Resource Name (ARN) of the association.
newGetServiceNetworkServiceAssociation ::
  -- | 'serviceNetworkServiceAssociationIdentifier'
  Prelude.Text ->
  GetServiceNetworkServiceAssociation
newGetServiceNetworkServiceAssociation
  pServiceNetworkServiceAssociationIdentifier_ =
    GetServiceNetworkServiceAssociation'
      { serviceNetworkServiceAssociationIdentifier =
          pServiceNetworkServiceAssociationIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the association.
getServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier :: Lens.Lens' GetServiceNetworkServiceAssociation Prelude.Text
getServiceNetworkServiceAssociation_serviceNetworkServiceAssociationIdentifier = Lens.lens (\GetServiceNetworkServiceAssociation' {serviceNetworkServiceAssociationIdentifier} -> serviceNetworkServiceAssociationIdentifier) (\s@GetServiceNetworkServiceAssociation' {} a -> s {serviceNetworkServiceAssociationIdentifier = a} :: GetServiceNetworkServiceAssociation)

instance
  Core.AWSRequest
    GetServiceNetworkServiceAssociation
  where
  type
    AWSResponse GetServiceNetworkServiceAssociation =
      GetServiceNetworkServiceAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceNetworkServiceAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "createdBy")
            Prelude.<*> (x Data..?> "customDomainName")
            Prelude.<*> (x Data..?> "dnsEntry")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "failureMessage")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "serviceArn")
            Prelude.<*> (x Data..?> "serviceId")
            Prelude.<*> (x Data..?> "serviceName")
            Prelude.<*> (x Data..?> "serviceNetworkArn")
            Prelude.<*> (x Data..?> "serviceNetworkId")
            Prelude.<*> (x Data..?> "serviceNetworkName")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetServiceNetworkServiceAssociation
  where
  hashWithSalt
    _salt
    GetServiceNetworkServiceAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` serviceNetworkServiceAssociationIdentifier

instance
  Prelude.NFData
    GetServiceNetworkServiceAssociation
  where
  rnf GetServiceNetworkServiceAssociation' {..} =
    Prelude.rnf
      serviceNetworkServiceAssociationIdentifier

instance
  Data.ToHeaders
    GetServiceNetworkServiceAssociation
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

instance
  Data.ToPath
    GetServiceNetworkServiceAssociation
  where
  toPath GetServiceNetworkServiceAssociation' {..} =
    Prelude.mconcat
      [ "/servicenetworkserviceassociations/",
        Data.toBS serviceNetworkServiceAssociationIdentifier
      ]

instance
  Data.ToQuery
    GetServiceNetworkServiceAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceNetworkServiceAssociationResponse' smart constructor.
data GetServiceNetworkServiceAssociationResponse = GetServiceNetworkServiceAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the association was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service network and service association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the service network.
    serviceNetworkArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service network.
    serviceNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service network.
    serviceNetworkName :: Prelude.Maybe Prelude.Text,
    -- | The status of the association.
    status :: Prelude.Maybe ServiceNetworkServiceAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceNetworkServiceAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getServiceNetworkServiceAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdAt', 'getServiceNetworkServiceAssociationResponse_createdAt' - The date and time that the association was created, specified in
-- ISO-8601 format.
--
-- 'createdBy', 'getServiceNetworkServiceAssociationResponse_createdBy' - The account that created the association.
--
-- 'customDomainName', 'getServiceNetworkServiceAssociationResponse_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'getServiceNetworkServiceAssociationResponse_dnsEntry' - The DNS name of the service.
--
-- 'failureCode', 'getServiceNetworkServiceAssociationResponse_failureCode' - The failure code.
--
-- 'failureMessage', 'getServiceNetworkServiceAssociationResponse_failureMessage' - The failure message.
--
-- 'id', 'getServiceNetworkServiceAssociationResponse_id' - The ID of the service network and service association.
--
-- 'serviceArn', 'getServiceNetworkServiceAssociationResponse_serviceArn' - The Amazon Resource Name (ARN) of the service.
--
-- 'serviceId', 'getServiceNetworkServiceAssociationResponse_serviceId' - The ID of the service.
--
-- 'serviceName', 'getServiceNetworkServiceAssociationResponse_serviceName' - The name of the service.
--
-- 'serviceNetworkArn', 'getServiceNetworkServiceAssociationResponse_serviceNetworkArn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'serviceNetworkId', 'getServiceNetworkServiceAssociationResponse_serviceNetworkId' - The ID of the service network.
--
-- 'serviceNetworkName', 'getServiceNetworkServiceAssociationResponse_serviceNetworkName' - The name of the service network.
--
-- 'status', 'getServiceNetworkServiceAssociationResponse_status' - The status of the association.
--
-- 'httpStatus', 'getServiceNetworkServiceAssociationResponse_httpStatus' - The response's http status code.
newGetServiceNetworkServiceAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceNetworkServiceAssociationResponse
newGetServiceNetworkServiceAssociationResponse
  pHttpStatus_ =
    GetServiceNetworkServiceAssociationResponse'
      { arn =
          Prelude.Nothing,
        createdAt = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        customDomainName =
          Prelude.Nothing,
        dnsEntry = Prelude.Nothing,
        failureCode = Prelude.Nothing,
        failureMessage =
          Prelude.Nothing,
        id = Prelude.Nothing,
        serviceArn = Prelude.Nothing,
        serviceId = Prelude.Nothing,
        serviceName = Prelude.Nothing,
        serviceNetworkArn =
          Prelude.Nothing,
        serviceNetworkId =
          Prelude.Nothing,
        serviceNetworkName =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
getServiceNetworkServiceAssociationResponse_arn :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_arn = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {arn} -> arn) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {arn = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The date and time that the association was created, specified in
-- ISO-8601 format.
getServiceNetworkServiceAssociationResponse_createdAt :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.UTCTime)
getServiceNetworkServiceAssociationResponse_createdAt = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {createdAt} -> createdAt) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {createdAt = a} :: GetServiceNetworkServiceAssociationResponse) Prelude.. Lens.mapping Data._Time

-- | The account that created the association.
getServiceNetworkServiceAssociationResponse_createdBy :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_createdBy = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {createdBy} -> createdBy) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {createdBy = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The custom domain name of the service.
getServiceNetworkServiceAssociationResponse_customDomainName :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_customDomainName = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {customDomainName} -> customDomainName) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {customDomainName = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The DNS name of the service.
getServiceNetworkServiceAssociationResponse_dnsEntry :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe DnsEntry)
getServiceNetworkServiceAssociationResponse_dnsEntry = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {dnsEntry} -> dnsEntry) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {dnsEntry = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The failure code.
getServiceNetworkServiceAssociationResponse_failureCode :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_failureCode = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {failureCode} -> failureCode) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {failureCode = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The failure message.
getServiceNetworkServiceAssociationResponse_failureMessage :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_failureMessage = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {failureMessage} -> failureMessage) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {failureMessage = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The ID of the service network and service association.
getServiceNetworkServiceAssociationResponse_id :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_id = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {id} -> id) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {id = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The Amazon Resource Name (ARN) of the service.
getServiceNetworkServiceAssociationResponse_serviceArn :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceArn = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceArn} -> serviceArn) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceArn = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The ID of the service.
getServiceNetworkServiceAssociationResponse_serviceId :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceId = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceId} -> serviceId) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceId = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The name of the service.
getServiceNetworkServiceAssociationResponse_serviceName :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceName = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceName} -> serviceName) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceName = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The Amazon Resource Name (ARN) of the service network.
getServiceNetworkServiceAssociationResponse_serviceNetworkArn :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceNetworkArn = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceNetworkArn} -> serviceNetworkArn) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceNetworkArn = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The ID of the service network.
getServiceNetworkServiceAssociationResponse_serviceNetworkId :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceNetworkId = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceNetworkId} -> serviceNetworkId) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceNetworkId = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The name of the service network.
getServiceNetworkServiceAssociationResponse_serviceNetworkName :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
getServiceNetworkServiceAssociationResponse_serviceNetworkName = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {serviceNetworkName} -> serviceNetworkName) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {serviceNetworkName = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The status of the association.
getServiceNetworkServiceAssociationResponse_status :: Lens.Lens' GetServiceNetworkServiceAssociationResponse (Prelude.Maybe ServiceNetworkServiceAssociationStatus)
getServiceNetworkServiceAssociationResponse_status = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {status} -> status) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {status = a} :: GetServiceNetworkServiceAssociationResponse)

-- | The response's http status code.
getServiceNetworkServiceAssociationResponse_httpStatus :: Lens.Lens' GetServiceNetworkServiceAssociationResponse Prelude.Int
getServiceNetworkServiceAssociationResponse_httpStatus = Lens.lens (\GetServiceNetworkServiceAssociationResponse' {httpStatus} -> httpStatus) (\s@GetServiceNetworkServiceAssociationResponse' {} a -> s {httpStatus = a} :: GetServiceNetworkServiceAssociationResponse)

instance
  Prelude.NFData
    GetServiceNetworkServiceAssociationResponse
  where
  rnf GetServiceNetworkServiceAssociationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dnsEntry
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceNetworkArn
      `Prelude.seq` Prelude.rnf serviceNetworkId
      `Prelude.seq` Prelude.rnf serviceNetworkName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
