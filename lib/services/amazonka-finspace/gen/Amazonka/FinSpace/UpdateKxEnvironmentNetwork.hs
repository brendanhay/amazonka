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
-- Module      : Amazonka.FinSpace.UpdateKxEnvironmentNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates environment network to connect to your internal network by using
-- a transit gateway. This API supports request to create a transit gateway
-- attachment from FinSpace VPC to your transit gateway ID and create a
-- custom Route-53 outbound resolvers.
--
-- Once you send a request to update a network, you cannot change it again.
-- Network update might require termination of any clusters that are
-- running in the existing network.
module Amazonka.FinSpace.UpdateKxEnvironmentNetwork
  ( -- * Creating a Request
    UpdateKxEnvironmentNetwork (..),
    newUpdateKxEnvironmentNetwork,

    -- * Request Lenses
    updateKxEnvironmentNetwork_clientToken,
    updateKxEnvironmentNetwork_customDNSConfiguration,
    updateKxEnvironmentNetwork_transitGatewayConfiguration,
    updateKxEnvironmentNetwork_environmentId,

    -- * Destructuring the Response
    UpdateKxEnvironmentNetworkResponse (..),
    newUpdateKxEnvironmentNetworkResponse,

    -- * Response Lenses
    updateKxEnvironmentNetworkResponse_availabilityZoneIds,
    updateKxEnvironmentNetworkResponse_awsAccountId,
    updateKxEnvironmentNetworkResponse_creationTimestamp,
    updateKxEnvironmentNetworkResponse_customDNSConfiguration,
    updateKxEnvironmentNetworkResponse_dedicatedServiceAccountId,
    updateKxEnvironmentNetworkResponse_description,
    updateKxEnvironmentNetworkResponse_dnsStatus,
    updateKxEnvironmentNetworkResponse_environmentArn,
    updateKxEnvironmentNetworkResponse_environmentId,
    updateKxEnvironmentNetworkResponse_errorMessage,
    updateKxEnvironmentNetworkResponse_kmsKeyId,
    updateKxEnvironmentNetworkResponse_name,
    updateKxEnvironmentNetworkResponse_status,
    updateKxEnvironmentNetworkResponse_tgwStatus,
    updateKxEnvironmentNetworkResponse_transitGatewayConfiguration,
    updateKxEnvironmentNetworkResponse_updateTimestamp,
    updateKxEnvironmentNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKxEnvironmentNetwork' smart constructor.
data UpdateKxEnvironmentNetwork = UpdateKxEnvironmentNetwork'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A list of DNS server name and server IP. This is used to set up Route-53
    -- outbound resolvers.
    customDNSConfiguration :: Prelude.Maybe [CustomDNSServer],
    -- | Specifies the transit gateway and network configuration to connect the
    -- kdb environment to an internal network.
    transitGatewayConfiguration :: Prelude.Maybe TransitGatewayConfiguration,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKxEnvironmentNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateKxEnvironmentNetwork_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'customDNSConfiguration', 'updateKxEnvironmentNetwork_customDNSConfiguration' - A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- 'transitGatewayConfiguration', 'updateKxEnvironmentNetwork_transitGatewayConfiguration' - Specifies the transit gateway and network configuration to connect the
-- kdb environment to an internal network.
--
-- 'environmentId', 'updateKxEnvironmentNetwork_environmentId' - A unique identifier for the kdb environment.
newUpdateKxEnvironmentNetwork ::
  -- | 'environmentId'
  Prelude.Text ->
  UpdateKxEnvironmentNetwork
newUpdateKxEnvironmentNetwork pEnvironmentId_ =
  UpdateKxEnvironmentNetwork'
    { clientToken =
        Prelude.Nothing,
      customDNSConfiguration = Prelude.Nothing,
      transitGatewayConfiguration = Prelude.Nothing,
      environmentId = pEnvironmentId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
updateKxEnvironmentNetwork_clientToken :: Lens.Lens' UpdateKxEnvironmentNetwork (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetwork_clientToken = Lens.lens (\UpdateKxEnvironmentNetwork' {clientToken} -> clientToken) (\s@UpdateKxEnvironmentNetwork' {} a -> s {clientToken = a} :: UpdateKxEnvironmentNetwork)

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
updateKxEnvironmentNetwork_customDNSConfiguration :: Lens.Lens' UpdateKxEnvironmentNetwork (Prelude.Maybe [CustomDNSServer])
updateKxEnvironmentNetwork_customDNSConfiguration = Lens.lens (\UpdateKxEnvironmentNetwork' {customDNSConfiguration} -> customDNSConfiguration) (\s@UpdateKxEnvironmentNetwork' {} a -> s {customDNSConfiguration = a} :: UpdateKxEnvironmentNetwork) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the transit gateway and network configuration to connect the
-- kdb environment to an internal network.
updateKxEnvironmentNetwork_transitGatewayConfiguration :: Lens.Lens' UpdateKxEnvironmentNetwork (Prelude.Maybe TransitGatewayConfiguration)
updateKxEnvironmentNetwork_transitGatewayConfiguration = Lens.lens (\UpdateKxEnvironmentNetwork' {transitGatewayConfiguration} -> transitGatewayConfiguration) (\s@UpdateKxEnvironmentNetwork' {} a -> s {transitGatewayConfiguration = a} :: UpdateKxEnvironmentNetwork)

-- | A unique identifier for the kdb environment.
updateKxEnvironmentNetwork_environmentId :: Lens.Lens' UpdateKxEnvironmentNetwork Prelude.Text
updateKxEnvironmentNetwork_environmentId = Lens.lens (\UpdateKxEnvironmentNetwork' {environmentId} -> environmentId) (\s@UpdateKxEnvironmentNetwork' {} a -> s {environmentId = a} :: UpdateKxEnvironmentNetwork)

instance Core.AWSRequest UpdateKxEnvironmentNetwork where
  type
    AWSResponse UpdateKxEnvironmentNetwork =
      UpdateKxEnvironmentNetworkResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKxEnvironmentNetworkResponse'
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

instance Prelude.Hashable UpdateKxEnvironmentNetwork where
  hashWithSalt _salt UpdateKxEnvironmentNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` customDNSConfiguration
      `Prelude.hashWithSalt` transitGatewayConfiguration
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData UpdateKxEnvironmentNetwork where
  rnf UpdateKxEnvironmentNetwork' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf customDNSConfiguration
      `Prelude.seq` Prelude.rnf transitGatewayConfiguration
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders UpdateKxEnvironmentNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKxEnvironmentNetwork where
  toJSON UpdateKxEnvironmentNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("customDNSConfiguration" Data..=)
              Prelude.<$> customDNSConfiguration,
            ("transitGatewayConfiguration" Data..=)
              Prelude.<$> transitGatewayConfiguration
          ]
      )

instance Data.ToPath UpdateKxEnvironmentNetwork where
  toPath UpdateKxEnvironmentNetwork' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/network"
      ]

instance Data.ToQuery UpdateKxEnvironmentNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKxEnvironmentNetworkResponse' smart constructor.
data UpdateKxEnvironmentNetworkResponse = UpdateKxEnvironmentNetworkResponse'
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
-- Create a value of 'UpdateKxEnvironmentNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneIds', 'updateKxEnvironmentNetworkResponse_availabilityZoneIds' - The identifier of the availability zones where subnets for the
-- environment are created.
--
-- 'awsAccountId', 'updateKxEnvironmentNetworkResponse_awsAccountId' - The unique identifier of the AWS account that is used to create the kdb
-- environment.
--
-- 'creationTimestamp', 'updateKxEnvironmentNetworkResponse_creationTimestamp' - The timestamp at which the kdb environment was created in FinSpace.
--
-- 'customDNSConfiguration', 'updateKxEnvironmentNetworkResponse_customDNSConfiguration' - A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- 'dedicatedServiceAccountId', 'updateKxEnvironmentNetworkResponse_dedicatedServiceAccountId' - A unique identifier for the AWS environment infrastructure account.
--
-- 'description', 'updateKxEnvironmentNetworkResponse_description' - The description of the environment.
--
-- 'dnsStatus', 'updateKxEnvironmentNetworkResponse_dnsStatus' - The status of DNS configuration.
--
-- 'environmentArn', 'updateKxEnvironmentNetworkResponse_environmentArn' - The ARN identifier of the environment.
--
-- 'environmentId', 'updateKxEnvironmentNetworkResponse_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorMessage', 'updateKxEnvironmentNetworkResponse_errorMessage' - Specifies the error message that appears if a flow fails.
--
-- 'kmsKeyId', 'updateKxEnvironmentNetworkResponse_kmsKeyId' - The KMS key ID to encrypt your data in the FinSpace environment.
--
-- 'name', 'updateKxEnvironmentNetworkResponse_name' - The name of the kdb environment.
--
-- 'status', 'updateKxEnvironmentNetworkResponse_status' - The status of the kdb environment.
--
-- 'tgwStatus', 'updateKxEnvironmentNetworkResponse_tgwStatus' - The status of the network configuration.
--
-- 'transitGatewayConfiguration', 'updateKxEnvironmentNetworkResponse_transitGatewayConfiguration' - Undocumented member.
--
-- 'updateTimestamp', 'updateKxEnvironmentNetworkResponse_updateTimestamp' - The timestamp at which the kdb environment was updated.
--
-- 'httpStatus', 'updateKxEnvironmentNetworkResponse_httpStatus' - The response's http status code.
newUpdateKxEnvironmentNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKxEnvironmentNetworkResponse
newUpdateKxEnvironmentNetworkResponse pHttpStatus_ =
  UpdateKxEnvironmentNetworkResponse'
    { availabilityZoneIds =
        Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      customDNSConfiguration =
        Prelude.Nothing,
      dedicatedServiceAccountId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      dnsStatus = Prelude.Nothing,
      environmentArn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      tgwStatus = Prelude.Nothing,
      transitGatewayConfiguration =
        Prelude.Nothing,
      updateTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the availability zones where subnets for the
-- environment are created.
updateKxEnvironmentNetworkResponse_availabilityZoneIds :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe [Prelude.Text])
updateKxEnvironmentNetworkResponse_availabilityZoneIds = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {availabilityZoneIds} -> availabilityZoneIds) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {availabilityZoneIds = a} :: UpdateKxEnvironmentNetworkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the AWS account that is used to create the kdb
-- environment.
updateKxEnvironmentNetworkResponse_awsAccountId :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_awsAccountId = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {awsAccountId} -> awsAccountId) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {awsAccountId = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The timestamp at which the kdb environment was created in FinSpace.
updateKxEnvironmentNetworkResponse_creationTimestamp :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.UTCTime)
updateKxEnvironmentNetworkResponse_creationTimestamp = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {creationTimestamp} -> creationTimestamp) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {creationTimestamp = a} :: UpdateKxEnvironmentNetworkResponse) Prelude.. Lens.mapping Data._Time

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
updateKxEnvironmentNetworkResponse_customDNSConfiguration :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe [CustomDNSServer])
updateKxEnvironmentNetworkResponse_customDNSConfiguration = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {customDNSConfiguration} -> customDNSConfiguration) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {customDNSConfiguration = a} :: UpdateKxEnvironmentNetworkResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the AWS environment infrastructure account.
updateKxEnvironmentNetworkResponse_dedicatedServiceAccountId :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_dedicatedServiceAccountId = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {dedicatedServiceAccountId} -> dedicatedServiceAccountId) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {dedicatedServiceAccountId = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The description of the environment.
updateKxEnvironmentNetworkResponse_description :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_description = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {description} -> description) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {description = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The status of DNS configuration.
updateKxEnvironmentNetworkResponse_dnsStatus :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe DnsStatus)
updateKxEnvironmentNetworkResponse_dnsStatus = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {dnsStatus} -> dnsStatus) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {dnsStatus = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The ARN identifier of the environment.
updateKxEnvironmentNetworkResponse_environmentArn :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_environmentArn = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {environmentArn} -> environmentArn) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {environmentArn = a} :: UpdateKxEnvironmentNetworkResponse)

-- | A unique identifier for the kdb environment.
updateKxEnvironmentNetworkResponse_environmentId :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_environmentId = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {environmentId} -> environmentId) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {environmentId = a} :: UpdateKxEnvironmentNetworkResponse)

-- | Specifies the error message that appears if a flow fails.
updateKxEnvironmentNetworkResponse_errorMessage :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_errorMessage = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {errorMessage} -> errorMessage) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {errorMessage = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The KMS key ID to encrypt your data in the FinSpace environment.
updateKxEnvironmentNetworkResponse_kmsKeyId :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_kmsKeyId = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {kmsKeyId} -> kmsKeyId) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {kmsKeyId = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The name of the kdb environment.
updateKxEnvironmentNetworkResponse_name :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.Text)
updateKxEnvironmentNetworkResponse_name = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {name} -> name) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {name = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The status of the kdb environment.
updateKxEnvironmentNetworkResponse_status :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe EnvironmentStatus)
updateKxEnvironmentNetworkResponse_status = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {status} -> status) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {status = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The status of the network configuration.
updateKxEnvironmentNetworkResponse_tgwStatus :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe TgwStatus)
updateKxEnvironmentNetworkResponse_tgwStatus = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {tgwStatus} -> tgwStatus) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {tgwStatus = a} :: UpdateKxEnvironmentNetworkResponse)

-- | Undocumented member.
updateKxEnvironmentNetworkResponse_transitGatewayConfiguration :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe TransitGatewayConfiguration)
updateKxEnvironmentNetworkResponse_transitGatewayConfiguration = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {transitGatewayConfiguration} -> transitGatewayConfiguration) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {transitGatewayConfiguration = a} :: UpdateKxEnvironmentNetworkResponse)

-- | The timestamp at which the kdb environment was updated.
updateKxEnvironmentNetworkResponse_updateTimestamp :: Lens.Lens' UpdateKxEnvironmentNetworkResponse (Prelude.Maybe Prelude.UTCTime)
updateKxEnvironmentNetworkResponse_updateTimestamp = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {updateTimestamp} -> updateTimestamp) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {updateTimestamp = a} :: UpdateKxEnvironmentNetworkResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateKxEnvironmentNetworkResponse_httpStatus :: Lens.Lens' UpdateKxEnvironmentNetworkResponse Prelude.Int
updateKxEnvironmentNetworkResponse_httpStatus = Lens.lens (\UpdateKxEnvironmentNetworkResponse' {httpStatus} -> httpStatus) (\s@UpdateKxEnvironmentNetworkResponse' {} a -> s {httpStatus = a} :: UpdateKxEnvironmentNetworkResponse)

instance
  Prelude.NFData
    UpdateKxEnvironmentNetworkResponse
  where
  rnf UpdateKxEnvironmentNetworkResponse' {..} =
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
