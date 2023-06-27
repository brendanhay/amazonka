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
-- Module      : Amazonka.FinSpace.GetKxEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the information for the specified kdb environment.
module Amazonka.FinSpace.GetKxEnvironment
  ( -- * Creating a Request
    GetKxEnvironment (..),
    newGetKxEnvironment,

    -- * Request Lenses
    getKxEnvironment_environmentId,

    -- * Destructuring the Response
    GetKxEnvironmentResponse (..),
    newGetKxEnvironmentResponse,

    -- * Response Lenses
    getKxEnvironmentResponse_availabilityZoneIds,
    getKxEnvironmentResponse_awsAccountId,
    getKxEnvironmentResponse_certificateAuthorityArn,
    getKxEnvironmentResponse_creationTimestamp,
    getKxEnvironmentResponse_customDNSConfiguration,
    getKxEnvironmentResponse_dedicatedServiceAccountId,
    getKxEnvironmentResponse_description,
    getKxEnvironmentResponse_dnsStatus,
    getKxEnvironmentResponse_environmentArn,
    getKxEnvironmentResponse_environmentId,
    getKxEnvironmentResponse_errorMessage,
    getKxEnvironmentResponse_kmsKeyId,
    getKxEnvironmentResponse_name,
    getKxEnvironmentResponse_status,
    getKxEnvironmentResponse_tgwStatus,
    getKxEnvironmentResponse_transitGatewayConfiguration,
    getKxEnvironmentResponse_updateTimestamp,
    getKxEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxEnvironment' smart constructor.
data GetKxEnvironment = GetKxEnvironment'
  { -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getKxEnvironment_environmentId' - A unique identifier for the kdb environment.
newGetKxEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  GetKxEnvironment
newGetKxEnvironment pEnvironmentId_ =
  GetKxEnvironment' {environmentId = pEnvironmentId_}

-- | A unique identifier for the kdb environment.
getKxEnvironment_environmentId :: Lens.Lens' GetKxEnvironment Prelude.Text
getKxEnvironment_environmentId = Lens.lens (\GetKxEnvironment' {environmentId} -> environmentId) (\s@GetKxEnvironment' {} a -> s {environmentId = a} :: GetKxEnvironment)

instance Core.AWSRequest GetKxEnvironment where
  type
    AWSResponse GetKxEnvironment =
      GetKxEnvironmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxEnvironmentResponse'
            Prelude.<$> ( x
                            Data..?> "availabilityZoneIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "awsAccountId")
            Prelude.<*> (x Data..?> "certificateAuthorityArn")
            Prelude.<*> (x Data..?> "creationTimestamp")
            Prelude.<*> ( x
                            Data..?> "customDNSConfiguration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "dedicatedServiceAccountId")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "dnsStatus")
            Prelude.<*> (x Data..?> "environmentArn")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "errorMessage")
            Prelude.<*> (x Data..?> "kmsKeyId")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tgwStatus")
            Prelude.<*> (x Data..?> "transitGatewayConfiguration")
            Prelude.<*> (x Data..?> "updateTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxEnvironment where
  hashWithSalt _salt GetKxEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentId

instance Prelude.NFData GetKxEnvironment where
  rnf GetKxEnvironment' {..} = Prelude.rnf environmentId

instance Data.ToHeaders GetKxEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxEnvironment where
  toPath GetKxEnvironment' {..} =
    Prelude.mconcat
      ["/kx/environments/", Data.toBS environmentId]

instance Data.ToQuery GetKxEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKxEnvironmentResponse' smart constructor.
data GetKxEnvironmentResponse = GetKxEnvironmentResponse'
  { -- | The identifier of the availability zones where subnets for the
    -- environment are created.
    availabilityZoneIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier of the AWS account that is used to create the kdb
    -- environment.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate authority of the kdb
    -- environment.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the kdb environment was created in FinSpace.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of DNS server name and server IP. This is used to set up Route-53
    -- outbound resolvers.
    customDNSConfiguration :: Prelude.Maybe [CustomDNSServer],
    -- | A unique identifier for the AWS environment infrastructure account.
    dedicatedServiceAccountId :: Prelude.Maybe Prelude.Text,
    -- | A description for the kdb environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of DNS configuration.
    dnsStatus :: Prelude.Maybe DnsStatus,
    -- | The ARN identifier of the environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the error message that appears if a flow fails.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The KMS key ID to encrypt your data in the FinSpace environment.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the kdb environment.
    status :: Prelude.Maybe EnvironmentStatus,
    -- | The status of the network configuration.
    tgwStatus :: Prelude.Maybe TgwStatus,
    transitGatewayConfiguration :: Prelude.Maybe TransitGatewayConfiguration,
    -- | The timestamp at which the kdb environment was updated.
    updateTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneIds', 'getKxEnvironmentResponse_availabilityZoneIds' - The identifier of the availability zones where subnets for the
-- environment are created.
--
-- 'awsAccountId', 'getKxEnvironmentResponse_awsAccountId' - The unique identifier of the AWS account that is used to create the kdb
-- environment.
--
-- 'certificateAuthorityArn', 'getKxEnvironmentResponse_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the certificate authority of the kdb
-- environment.
--
-- 'creationTimestamp', 'getKxEnvironmentResponse_creationTimestamp' - The timestamp at which the kdb environment was created in FinSpace.
--
-- 'customDNSConfiguration', 'getKxEnvironmentResponse_customDNSConfiguration' - A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- 'dedicatedServiceAccountId', 'getKxEnvironmentResponse_dedicatedServiceAccountId' - A unique identifier for the AWS environment infrastructure account.
--
-- 'description', 'getKxEnvironmentResponse_description' - A description for the kdb environment.
--
-- 'dnsStatus', 'getKxEnvironmentResponse_dnsStatus' - The status of DNS configuration.
--
-- 'environmentArn', 'getKxEnvironmentResponse_environmentArn' - The ARN identifier of the environment.
--
-- 'environmentId', 'getKxEnvironmentResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorMessage', 'getKxEnvironmentResponse_errorMessage' - Specifies the error message that appears if a flow fails.
--
-- 'kmsKeyId', 'getKxEnvironmentResponse_kmsKeyId' - The KMS key ID to encrypt your data in the FinSpace environment.
--
-- 'name', 'getKxEnvironmentResponse_name' - The name of the kdb environment.
--
-- 'status', 'getKxEnvironmentResponse_status' - The status of the kdb environment.
--
-- 'tgwStatus', 'getKxEnvironmentResponse_tgwStatus' - The status of the network configuration.
--
-- 'transitGatewayConfiguration', 'getKxEnvironmentResponse_transitGatewayConfiguration' - Undocumented member.
--
-- 'updateTimestamp', 'getKxEnvironmentResponse_updateTimestamp' - The timestamp at which the kdb environment was updated.
--
-- 'httpStatus', 'getKxEnvironmentResponse_httpStatus' - The response's http status code.
newGetKxEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxEnvironmentResponse
newGetKxEnvironmentResponse pHttpStatus_ =
  GetKxEnvironmentResponse'
    { availabilityZoneIds =
        Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      certificateAuthorityArn = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      customDNSConfiguration = Prelude.Nothing,
      dedicatedServiceAccountId = Prelude.Nothing,
      description = Prelude.Nothing,
      dnsStatus = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      tgwStatus = Prelude.Nothing,
      transitGatewayConfiguration = Prelude.Nothing,
      updateTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the availability zones where subnets for the
-- environment are created.
getKxEnvironmentResponse_availabilityZoneIds :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe [Prelude.Text])
getKxEnvironmentResponse_availabilityZoneIds = Lens.lens (\GetKxEnvironmentResponse' {availabilityZoneIds} -> availabilityZoneIds) (\s@GetKxEnvironmentResponse' {} a -> s {availabilityZoneIds = a} :: GetKxEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the AWS account that is used to create the kdb
-- environment.
getKxEnvironmentResponse_awsAccountId :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_awsAccountId = Lens.lens (\GetKxEnvironmentResponse' {awsAccountId} -> awsAccountId) (\s@GetKxEnvironmentResponse' {} a -> s {awsAccountId = a} :: GetKxEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the certificate authority of the kdb
-- environment.
getKxEnvironmentResponse_certificateAuthorityArn :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_certificateAuthorityArn = Lens.lens (\GetKxEnvironmentResponse' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@GetKxEnvironmentResponse' {} a -> s {certificateAuthorityArn = a} :: GetKxEnvironmentResponse)

-- | The timestamp at which the kdb environment was created in FinSpace.
getKxEnvironmentResponse_creationTimestamp :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
getKxEnvironmentResponse_creationTimestamp = Lens.lens (\GetKxEnvironmentResponse' {creationTimestamp} -> creationTimestamp) (\s@GetKxEnvironmentResponse' {} a -> s {creationTimestamp = a} :: GetKxEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
getKxEnvironmentResponse_customDNSConfiguration :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe [CustomDNSServer])
getKxEnvironmentResponse_customDNSConfiguration = Lens.lens (\GetKxEnvironmentResponse' {customDNSConfiguration} -> customDNSConfiguration) (\s@GetKxEnvironmentResponse' {} a -> s {customDNSConfiguration = a} :: GetKxEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the AWS environment infrastructure account.
getKxEnvironmentResponse_dedicatedServiceAccountId :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_dedicatedServiceAccountId = Lens.lens (\GetKxEnvironmentResponse' {dedicatedServiceAccountId} -> dedicatedServiceAccountId) (\s@GetKxEnvironmentResponse' {} a -> s {dedicatedServiceAccountId = a} :: GetKxEnvironmentResponse)

-- | A description for the kdb environment.
getKxEnvironmentResponse_description :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_description = Lens.lens (\GetKxEnvironmentResponse' {description} -> description) (\s@GetKxEnvironmentResponse' {} a -> s {description = a} :: GetKxEnvironmentResponse)

-- | The status of DNS configuration.
getKxEnvironmentResponse_dnsStatus :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe DnsStatus)
getKxEnvironmentResponse_dnsStatus = Lens.lens (\GetKxEnvironmentResponse' {dnsStatus} -> dnsStatus) (\s@GetKxEnvironmentResponse' {} a -> s {dnsStatus = a} :: GetKxEnvironmentResponse)

-- | The ARN identifier of the environment.
getKxEnvironmentResponse_environmentArn :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_environmentArn = Lens.lens (\GetKxEnvironmentResponse' {environmentArn} -> environmentArn) (\s@GetKxEnvironmentResponse' {} a -> s {environmentArn = a} :: GetKxEnvironmentResponse)

-- | A unique identifier for the kdb environment.
getKxEnvironmentResponse_environmentId :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_environmentId = Lens.lens (\GetKxEnvironmentResponse' {environmentId} -> environmentId) (\s@GetKxEnvironmentResponse' {} a -> s {environmentId = a} :: GetKxEnvironmentResponse)

-- | Specifies the error message that appears if a flow fails.
getKxEnvironmentResponse_errorMessage :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_errorMessage = Lens.lens (\GetKxEnvironmentResponse' {errorMessage} -> errorMessage) (\s@GetKxEnvironmentResponse' {} a -> s {errorMessage = a} :: GetKxEnvironmentResponse)

-- | The KMS key ID to encrypt your data in the FinSpace environment.
getKxEnvironmentResponse_kmsKeyId :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_kmsKeyId = Lens.lens (\GetKxEnvironmentResponse' {kmsKeyId} -> kmsKeyId) (\s@GetKxEnvironmentResponse' {} a -> s {kmsKeyId = a} :: GetKxEnvironmentResponse)

-- | The name of the kdb environment.
getKxEnvironmentResponse_name :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
getKxEnvironmentResponse_name = Lens.lens (\GetKxEnvironmentResponse' {name} -> name) (\s@GetKxEnvironmentResponse' {} a -> s {name = a} :: GetKxEnvironmentResponse)

-- | The status of the kdb environment.
getKxEnvironmentResponse_status :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe EnvironmentStatus)
getKxEnvironmentResponse_status = Lens.lens (\GetKxEnvironmentResponse' {status} -> status) (\s@GetKxEnvironmentResponse' {} a -> s {status = a} :: GetKxEnvironmentResponse)

-- | The status of the network configuration.
getKxEnvironmentResponse_tgwStatus :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe TgwStatus)
getKxEnvironmentResponse_tgwStatus = Lens.lens (\GetKxEnvironmentResponse' {tgwStatus} -> tgwStatus) (\s@GetKxEnvironmentResponse' {} a -> s {tgwStatus = a} :: GetKxEnvironmentResponse)

-- | Undocumented member.
getKxEnvironmentResponse_transitGatewayConfiguration :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe TransitGatewayConfiguration)
getKxEnvironmentResponse_transitGatewayConfiguration = Lens.lens (\GetKxEnvironmentResponse' {transitGatewayConfiguration} -> transitGatewayConfiguration) (\s@GetKxEnvironmentResponse' {} a -> s {transitGatewayConfiguration = a} :: GetKxEnvironmentResponse)

-- | The timestamp at which the kdb environment was updated.
getKxEnvironmentResponse_updateTimestamp :: Lens.Lens' GetKxEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
getKxEnvironmentResponse_updateTimestamp = Lens.lens (\GetKxEnvironmentResponse' {updateTimestamp} -> updateTimestamp) (\s@GetKxEnvironmentResponse' {} a -> s {updateTimestamp = a} :: GetKxEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getKxEnvironmentResponse_httpStatus :: Lens.Lens' GetKxEnvironmentResponse Prelude.Int
getKxEnvironmentResponse_httpStatus = Lens.lens (\GetKxEnvironmentResponse' {httpStatus} -> httpStatus) (\s@GetKxEnvironmentResponse' {} a -> s {httpStatus = a} :: GetKxEnvironmentResponse)

instance Prelude.NFData GetKxEnvironmentResponse where
  rnf GetKxEnvironmentResponse' {..} =
    Prelude.rnf availabilityZoneIds
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf customDNSConfiguration
      `Prelude.seq` Prelude.rnf dedicatedServiceAccountId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dnsStatus
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tgwStatus
      `Prelude.seq` Prelude.rnf
        transitGatewayConfiguration
      `Prelude.seq` Prelude.rnf updateTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
