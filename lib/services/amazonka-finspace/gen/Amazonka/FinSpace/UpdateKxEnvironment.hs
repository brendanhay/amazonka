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
-- Module      : Amazonka.FinSpace.UpdateKxEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information for the given kdb environment.
module Amazonka.FinSpace.UpdateKxEnvironment
  ( -- * Creating a Request
    UpdateKxEnvironment (..),
    newUpdateKxEnvironment,

    -- * Request Lenses
    updateKxEnvironment_clientToken,
    updateKxEnvironment_description,
    updateKxEnvironment_name,
    updateKxEnvironment_environmentId,

    -- * Destructuring the Response
    UpdateKxEnvironmentResponse (..),
    newUpdateKxEnvironmentResponse,

    -- * Response Lenses
    updateKxEnvironmentResponse_availabilityZoneIds,
    updateKxEnvironmentResponse_awsAccountId,
    updateKxEnvironmentResponse_creationTimestamp,
    updateKxEnvironmentResponse_customDNSConfiguration,
    updateKxEnvironmentResponse_dedicatedServiceAccountId,
    updateKxEnvironmentResponse_description,
    updateKxEnvironmentResponse_dnsStatus,
    updateKxEnvironmentResponse_environmentArn,
    updateKxEnvironmentResponse_environmentId,
    updateKxEnvironmentResponse_errorMessage,
    updateKxEnvironmentResponse_kmsKeyId,
    updateKxEnvironmentResponse_name,
    updateKxEnvironmentResponse_status,
    updateKxEnvironmentResponse_tgwStatus,
    updateKxEnvironmentResponse_transitGatewayConfiguration,
    updateKxEnvironmentResponse_updateTimestamp,
    updateKxEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKxEnvironment' smart constructor.
data UpdateKxEnvironment = UpdateKxEnvironment'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the kdb environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateKxEnvironment_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'description', 'updateKxEnvironment_description' - A description of the kdb environment.
--
-- 'name', 'updateKxEnvironment_name' - The name of the kdb environment.
--
-- 'environmentId', 'updateKxEnvironment_environmentId' - A unique identifier for the kdb environment.
newUpdateKxEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  UpdateKxEnvironment
newUpdateKxEnvironment pEnvironmentId_ =
  UpdateKxEnvironment'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateKxEnvironment_clientToken :: Lens.Lens' UpdateKxEnvironment (Prelude.Maybe Prelude.Text)
updateKxEnvironment_clientToken = Lens.lens (\UpdateKxEnvironment' {clientToken} -> clientToken) (\s@UpdateKxEnvironment' {} a -> s {clientToken = a} :: UpdateKxEnvironment)

-- | A description of the kdb environment.
updateKxEnvironment_description :: Lens.Lens' UpdateKxEnvironment (Prelude.Maybe Prelude.Text)
updateKxEnvironment_description = Lens.lens (\UpdateKxEnvironment' {description} -> description) (\s@UpdateKxEnvironment' {} a -> s {description = a} :: UpdateKxEnvironment)

-- | The name of the kdb environment.
updateKxEnvironment_name :: Lens.Lens' UpdateKxEnvironment (Prelude.Maybe Prelude.Text)
updateKxEnvironment_name = Lens.lens (\UpdateKxEnvironment' {name} -> name) (\s@UpdateKxEnvironment' {} a -> s {name = a} :: UpdateKxEnvironment)

-- | A unique identifier for the kdb environment.
updateKxEnvironment_environmentId :: Lens.Lens' UpdateKxEnvironment Prelude.Text
updateKxEnvironment_environmentId = Lens.lens (\UpdateKxEnvironment' {environmentId} -> environmentId) (\s@UpdateKxEnvironment' {} a -> s {environmentId = a} :: UpdateKxEnvironment)

instance Core.AWSRequest UpdateKxEnvironment where
  type
    AWSResponse UpdateKxEnvironment =
      UpdateKxEnvironmentResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKxEnvironmentResponse'
            Prelude.<$> ( x
                            Data..?> "availabilityZoneIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "awsAccountId")
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

instance Prelude.Hashable UpdateKxEnvironment where
  hashWithSalt _salt UpdateKxEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData UpdateKxEnvironment where
  rnf UpdateKxEnvironment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders UpdateKxEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKxEnvironment where
  toJSON UpdateKxEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateKxEnvironment where
  toPath UpdateKxEnvironment' {..} =
    Prelude.mconcat
      ["/kx/environments/", Data.toBS environmentId]

instance Data.ToQuery UpdateKxEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKxEnvironmentResponse' smart constructor.
data UpdateKxEnvironmentResponse = UpdateKxEnvironmentResponse'
  { -- | The identifier of the availability zones where subnets for the
    -- environment are created.
    availabilityZoneIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier of the AWS account that is used to create the kdb
    -- environment.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the kdb environment was created in FinSpace.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of DNS server name and server IP. This is used to set up Route-53
    -- outbound resolvers.
    customDNSConfiguration :: Prelude.Maybe [CustomDNSServer],
    -- | A unique identifier for the AWS environment infrastructure account.
    dedicatedServiceAccountId :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment.
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
-- Create a value of 'UpdateKxEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneIds', 'updateKxEnvironmentResponse_availabilityZoneIds' - The identifier of the availability zones where subnets for the
-- environment are created.
--
-- 'awsAccountId', 'updateKxEnvironmentResponse_awsAccountId' - The unique identifier of the AWS account that is used to create the kdb
-- environment.
--
-- 'creationTimestamp', 'updateKxEnvironmentResponse_creationTimestamp' - The timestamp at which the kdb environment was created in FinSpace.
--
-- 'customDNSConfiguration', 'updateKxEnvironmentResponse_customDNSConfiguration' - A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- 'dedicatedServiceAccountId', 'updateKxEnvironmentResponse_dedicatedServiceAccountId' - A unique identifier for the AWS environment infrastructure account.
--
-- 'description', 'updateKxEnvironmentResponse_description' - The description of the environment.
--
-- 'dnsStatus', 'updateKxEnvironmentResponse_dnsStatus' - The status of DNS configuration.
--
-- 'environmentArn', 'updateKxEnvironmentResponse_environmentArn' - The ARN identifier of the environment.
--
-- 'environmentId', 'updateKxEnvironmentResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorMessage', 'updateKxEnvironmentResponse_errorMessage' - Specifies the error message that appears if a flow fails.
--
-- 'kmsKeyId', 'updateKxEnvironmentResponse_kmsKeyId' - The KMS key ID to encrypt your data in the FinSpace environment.
--
-- 'name', 'updateKxEnvironmentResponse_name' - The name of the kdb environment.
--
-- 'status', 'updateKxEnvironmentResponse_status' - The status of the kdb environment.
--
-- 'tgwStatus', 'updateKxEnvironmentResponse_tgwStatus' - The status of the network configuration.
--
-- 'transitGatewayConfiguration', 'updateKxEnvironmentResponse_transitGatewayConfiguration' - Undocumented member.
--
-- 'updateTimestamp', 'updateKxEnvironmentResponse_updateTimestamp' - The timestamp at which the kdb environment was updated.
--
-- 'httpStatus', 'updateKxEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateKxEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKxEnvironmentResponse
newUpdateKxEnvironmentResponse pHttpStatus_ =
  UpdateKxEnvironmentResponse'
    { availabilityZoneIds =
        Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
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
updateKxEnvironmentResponse_availabilityZoneIds :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe [Prelude.Text])
updateKxEnvironmentResponse_availabilityZoneIds = Lens.lens (\UpdateKxEnvironmentResponse' {availabilityZoneIds} -> availabilityZoneIds) (\s@UpdateKxEnvironmentResponse' {} a -> s {availabilityZoneIds = a} :: UpdateKxEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the AWS account that is used to create the kdb
-- environment.
updateKxEnvironmentResponse_awsAccountId :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_awsAccountId = Lens.lens (\UpdateKxEnvironmentResponse' {awsAccountId} -> awsAccountId) (\s@UpdateKxEnvironmentResponse' {} a -> s {awsAccountId = a} :: UpdateKxEnvironmentResponse)

-- | The timestamp at which the kdb environment was created in FinSpace.
updateKxEnvironmentResponse_creationTimestamp :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
updateKxEnvironmentResponse_creationTimestamp = Lens.lens (\UpdateKxEnvironmentResponse' {creationTimestamp} -> creationTimestamp) (\s@UpdateKxEnvironmentResponse' {} a -> s {creationTimestamp = a} :: UpdateKxEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
updateKxEnvironmentResponse_customDNSConfiguration :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe [CustomDNSServer])
updateKxEnvironmentResponse_customDNSConfiguration = Lens.lens (\UpdateKxEnvironmentResponse' {customDNSConfiguration} -> customDNSConfiguration) (\s@UpdateKxEnvironmentResponse' {} a -> s {customDNSConfiguration = a} :: UpdateKxEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the AWS environment infrastructure account.
updateKxEnvironmentResponse_dedicatedServiceAccountId :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_dedicatedServiceAccountId = Lens.lens (\UpdateKxEnvironmentResponse' {dedicatedServiceAccountId} -> dedicatedServiceAccountId) (\s@UpdateKxEnvironmentResponse' {} a -> s {dedicatedServiceAccountId = a} :: UpdateKxEnvironmentResponse)

-- | The description of the environment.
updateKxEnvironmentResponse_description :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_description = Lens.lens (\UpdateKxEnvironmentResponse' {description} -> description) (\s@UpdateKxEnvironmentResponse' {} a -> s {description = a} :: UpdateKxEnvironmentResponse)

-- | The status of DNS configuration.
updateKxEnvironmentResponse_dnsStatus :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe DnsStatus)
updateKxEnvironmentResponse_dnsStatus = Lens.lens (\UpdateKxEnvironmentResponse' {dnsStatus} -> dnsStatus) (\s@UpdateKxEnvironmentResponse' {} a -> s {dnsStatus = a} :: UpdateKxEnvironmentResponse)

-- | The ARN identifier of the environment.
updateKxEnvironmentResponse_environmentArn :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_environmentArn = Lens.lens (\UpdateKxEnvironmentResponse' {environmentArn} -> environmentArn) (\s@UpdateKxEnvironmentResponse' {} a -> s {environmentArn = a} :: UpdateKxEnvironmentResponse)

-- | A unique identifier for the kdb environment.
updateKxEnvironmentResponse_environmentId :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_environmentId = Lens.lens (\UpdateKxEnvironmentResponse' {environmentId} -> environmentId) (\s@UpdateKxEnvironmentResponse' {} a -> s {environmentId = a} :: UpdateKxEnvironmentResponse)

-- | Specifies the error message that appears if a flow fails.
updateKxEnvironmentResponse_errorMessage :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_errorMessage = Lens.lens (\UpdateKxEnvironmentResponse' {errorMessage} -> errorMessage) (\s@UpdateKxEnvironmentResponse' {} a -> s {errorMessage = a} :: UpdateKxEnvironmentResponse)

-- | The KMS key ID to encrypt your data in the FinSpace environment.
updateKxEnvironmentResponse_kmsKeyId :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_kmsKeyId = Lens.lens (\UpdateKxEnvironmentResponse' {kmsKeyId} -> kmsKeyId) (\s@UpdateKxEnvironmentResponse' {} a -> s {kmsKeyId = a} :: UpdateKxEnvironmentResponse)

-- | The name of the kdb environment.
updateKxEnvironmentResponse_name :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentResponse_name = Lens.lens (\UpdateKxEnvironmentResponse' {name} -> name) (\s@UpdateKxEnvironmentResponse' {} a -> s {name = a} :: UpdateKxEnvironmentResponse)

-- | The status of the kdb environment.
updateKxEnvironmentResponse_status :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe EnvironmentStatus)
updateKxEnvironmentResponse_status = Lens.lens (\UpdateKxEnvironmentResponse' {status} -> status) (\s@UpdateKxEnvironmentResponse' {} a -> s {status = a} :: UpdateKxEnvironmentResponse)

-- | The status of the network configuration.
updateKxEnvironmentResponse_tgwStatus :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe TgwStatus)
updateKxEnvironmentResponse_tgwStatus = Lens.lens (\UpdateKxEnvironmentResponse' {tgwStatus} -> tgwStatus) (\s@UpdateKxEnvironmentResponse' {} a -> s {tgwStatus = a} :: UpdateKxEnvironmentResponse)

-- | Undocumented member.
updateKxEnvironmentResponse_transitGatewayConfiguration :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe TransitGatewayConfiguration)
updateKxEnvironmentResponse_transitGatewayConfiguration = Lens.lens (\UpdateKxEnvironmentResponse' {transitGatewayConfiguration} -> transitGatewayConfiguration) (\s@UpdateKxEnvironmentResponse' {} a -> s {transitGatewayConfiguration = a} :: UpdateKxEnvironmentResponse)

-- | The timestamp at which the kdb environment was updated.
updateKxEnvironmentResponse_updateTimestamp :: Lens.Lens' UpdateKxEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
updateKxEnvironmentResponse_updateTimestamp = Lens.lens (\UpdateKxEnvironmentResponse' {updateTimestamp} -> updateTimestamp) (\s@UpdateKxEnvironmentResponse' {} a -> s {updateTimestamp = a} :: UpdateKxEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateKxEnvironmentResponse_httpStatus :: Lens.Lens' UpdateKxEnvironmentResponse Prelude.Int
updateKxEnvironmentResponse_httpStatus = Lens.lens (\UpdateKxEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateKxEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateKxEnvironmentResponse)

instance Prelude.NFData UpdateKxEnvironmentResponse where
  rnf UpdateKxEnvironmentResponse' {..} =
    Prelude.rnf availabilityZoneIds
      `Prelude.seq` Prelude.rnf awsAccountId
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
      `Prelude.seq` Prelude.rnf transitGatewayConfiguration
      `Prelude.seq` Prelude.rnf updateTimestamp
      `Prelude.seq` Prelude.rnf httpStatus
