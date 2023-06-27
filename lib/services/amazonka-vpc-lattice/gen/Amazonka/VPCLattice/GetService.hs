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
-- Module      : Amazonka.VPCLattice.GetService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified service.
module Amazonka.VPCLattice.GetService
  ( -- * Creating a Request
    GetService (..),
    newGetService,

    -- * Request Lenses
    getService_serviceIdentifier,

    -- * Destructuring the Response
    GetServiceResponse (..),
    newGetServiceResponse,

    -- * Response Lenses
    getServiceResponse_arn,
    getServiceResponse_authType,
    getServiceResponse_certificateArn,
    getServiceResponse_createdAt,
    getServiceResponse_customDomainName,
    getServiceResponse_dnsEntry,
    getServiceResponse_failureCode,
    getServiceResponse_failureMessage,
    getServiceResponse_id,
    getServiceResponse_lastUpdatedAt,
    getServiceResponse_name,
    getServiceResponse_status,
    getServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetService' smart constructor.
data GetService = GetService'
  { -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIdentifier', 'getService_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newGetService ::
  -- | 'serviceIdentifier'
  Prelude.Text ->
  GetService
newGetService pServiceIdentifier_ =
  GetService'
    { serviceIdentifier =
        pServiceIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the service.
getService_serviceIdentifier :: Lens.Lens' GetService Prelude.Text
getService_serviceIdentifier = Lens.lens (\GetService' {serviceIdentifier} -> serviceIdentifier) (\s@GetService' {} a -> s {serviceIdentifier = a} :: GetService)

instance Core.AWSRequest GetService where
  type AWSResponse GetService = GetServiceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "certificateArn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "customDomainName")
            Prelude.<*> (x Data..?> "dnsEntry")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "failureMessage")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetService where
  hashWithSalt _salt GetService' {..} =
    _salt `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData GetService where
  rnf GetService' {..} = Prelude.rnf serviceIdentifier

instance Data.ToHeaders GetService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetService where
  toPath GetService' {..} =
    Prelude.mconcat
      ["/services/", Data.toBS serviceIdentifier]

instance Data.ToQuery GetService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceResponse' smart constructor.
data GetServiceResponse = GetServiceResponse'
  { -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of IAM policy.
    authType :: Prelude.Maybe AuthType,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service was created, specified in ISO-8601
    -- format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the service was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the service.
    status :: Prelude.Maybe ServiceStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'authType', 'getServiceResponse_authType' - The type of IAM policy.
--
-- 'certificateArn', 'getServiceResponse_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'createdAt', 'getServiceResponse_createdAt' - The date and time that the service was created, specified in ISO-8601
-- format.
--
-- 'customDomainName', 'getServiceResponse_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'getServiceResponse_dnsEntry' - The DNS name of the service.
--
-- 'failureCode', 'getServiceResponse_failureCode' - The failure code.
--
-- 'failureMessage', 'getServiceResponse_failureMessage' - The failure message.
--
-- 'id', 'getServiceResponse_id' - The ID of the service.
--
-- 'lastUpdatedAt', 'getServiceResponse_lastUpdatedAt' - The date and time that the service was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'getServiceResponse_name' - The name of the service.
--
-- 'status', 'getServiceResponse_status' - The status of the service.
--
-- 'httpStatus', 'getServiceResponse_httpStatus' - The response's http status code.
newGetServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceResponse
newGetServiceResponse pHttpStatus_ =
  GetServiceResponse'
    { arn = Prelude.Nothing,
      authType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      dnsEntry = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service.
getServiceResponse_arn :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_arn = Lens.lens (\GetServiceResponse' {arn} -> arn) (\s@GetServiceResponse' {} a -> s {arn = a} :: GetServiceResponse)

-- | The type of IAM policy.
getServiceResponse_authType :: Lens.Lens' GetServiceResponse (Prelude.Maybe AuthType)
getServiceResponse_authType = Lens.lens (\GetServiceResponse' {authType} -> authType) (\s@GetServiceResponse' {} a -> s {authType = a} :: GetServiceResponse)

-- | The Amazon Resource Name (ARN) of the certificate.
getServiceResponse_certificateArn :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_certificateArn = Lens.lens (\GetServiceResponse' {certificateArn} -> certificateArn) (\s@GetServiceResponse' {} a -> s {certificateArn = a} :: GetServiceResponse)

-- | The date and time that the service was created, specified in ISO-8601
-- format.
getServiceResponse_createdAt :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.UTCTime)
getServiceResponse_createdAt = Lens.lens (\GetServiceResponse' {createdAt} -> createdAt) (\s@GetServiceResponse' {} a -> s {createdAt = a} :: GetServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The custom domain name of the service.
getServiceResponse_customDomainName :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_customDomainName = Lens.lens (\GetServiceResponse' {customDomainName} -> customDomainName) (\s@GetServiceResponse' {} a -> s {customDomainName = a} :: GetServiceResponse)

-- | The DNS name of the service.
getServiceResponse_dnsEntry :: Lens.Lens' GetServiceResponse (Prelude.Maybe DnsEntry)
getServiceResponse_dnsEntry = Lens.lens (\GetServiceResponse' {dnsEntry} -> dnsEntry) (\s@GetServiceResponse' {} a -> s {dnsEntry = a} :: GetServiceResponse)

-- | The failure code.
getServiceResponse_failureCode :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_failureCode = Lens.lens (\GetServiceResponse' {failureCode} -> failureCode) (\s@GetServiceResponse' {} a -> s {failureCode = a} :: GetServiceResponse)

-- | The failure message.
getServiceResponse_failureMessage :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_failureMessage = Lens.lens (\GetServiceResponse' {failureMessage} -> failureMessage) (\s@GetServiceResponse' {} a -> s {failureMessage = a} :: GetServiceResponse)

-- | The ID of the service.
getServiceResponse_id :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_id = Lens.lens (\GetServiceResponse' {id} -> id) (\s@GetServiceResponse' {} a -> s {id = a} :: GetServiceResponse)

-- | The date and time that the service was last updated, specified in
-- ISO-8601 format.
getServiceResponse_lastUpdatedAt :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.UTCTime)
getServiceResponse_lastUpdatedAt = Lens.lens (\GetServiceResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetServiceResponse' {} a -> s {lastUpdatedAt = a} :: GetServiceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the service.
getServiceResponse_name :: Lens.Lens' GetServiceResponse (Prelude.Maybe Prelude.Text)
getServiceResponse_name = Lens.lens (\GetServiceResponse' {name} -> name) (\s@GetServiceResponse' {} a -> s {name = a} :: GetServiceResponse)

-- | The status of the service.
getServiceResponse_status :: Lens.Lens' GetServiceResponse (Prelude.Maybe ServiceStatus)
getServiceResponse_status = Lens.lens (\GetServiceResponse' {status} -> status) (\s@GetServiceResponse' {} a -> s {status = a} :: GetServiceResponse)

-- | The response's http status code.
getServiceResponse_httpStatus :: Lens.Lens' GetServiceResponse Prelude.Int
getServiceResponse_httpStatus = Lens.lens (\GetServiceResponse' {httpStatus} -> httpStatus) (\s@GetServiceResponse' {} a -> s {httpStatus = a} :: GetServiceResponse)

instance Prelude.NFData GetServiceResponse where
  rnf GetServiceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dnsEntry
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
