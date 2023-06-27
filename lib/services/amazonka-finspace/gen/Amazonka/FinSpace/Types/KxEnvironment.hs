{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Types.KxEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxEnvironment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types.CustomDNSServer
import Amazonka.FinSpace.Types.DnsStatus
import Amazonka.FinSpace.Types.EnvironmentStatus
import Amazonka.FinSpace.Types.TgwStatus
import Amazonka.FinSpace.Types.TransitGatewayConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The details of a kdb environment.
--
-- /See:/ 'newKxEnvironment' smart constructor.
data KxEnvironment = KxEnvironment'
  { -- | The identifier of the availability zones where subnets for the
    -- environment are created.
    availabilityZoneIds :: Prelude.Maybe [Prelude.Text],
    -- | The unique identifier of the AWS account in which you create the kdb
    -- environment.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate authority:
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the kdb environment was created in FinSpace. The
    -- value is determined as epoch time in milliseconds. For example, the
    -- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
    -- 1635768000000.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | A list of DNS server name and server IP. This is used to set up Route-53
    -- outbound resolvers.
    customDNSConfiguration :: Prelude.Maybe [CustomDNSServer],
    -- | A unique identifier for the AWS environment infrastructure account.
    dedicatedServiceAccountId :: Prelude.Maybe Prelude.Text,
    -- | A description of the kdb environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of DNS configuration.
    dnsStatus :: Prelude.Maybe DnsStatus,
    -- | The Amazon Resource Name (ARN) of your kdb environment.
    environmentArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the error message that appears if a flow fails.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the kdb environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the environment creation.
    --
    -- -   CREATE_REQUESTED – Environment creation has been requested.
    --
    -- -   CREATING – Environment is in the process of being created.
    --
    -- -   FAILED_CREATION – Environment creation has failed.
    --
    -- -   CREATED – Environment is successfully created and is currently
    --     active.
    --
    -- -   DELETE REQUESTED – Environment deletion has been requested.
    --
    -- -   DELETING – Environment is in the process of being deleted.
    --
    -- -   RETRY_DELETION – Initial environment deletion failed, system is
    --     reattempting delete.
    --
    -- -   DELETED – Environment has been deleted.
    --
    -- -   FAILED_DELETION – Environment deletion has failed.
    status :: Prelude.Maybe EnvironmentStatus,
    -- | The status of the network configuration.
    tgwStatus :: Prelude.Maybe TgwStatus,
    -- | Specifies the transit gateway and network configuration to connect the
    -- kdb environment to an internal network.
    transitGatewayConfiguration :: Prelude.Maybe TransitGatewayConfiguration,
    -- | The timestamp at which the kdb environment was modified in FinSpace. The
    -- value is determined as epoch time in milliseconds. For example, the
    -- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
    -- 1635768000000.
    updateTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneIds', 'kxEnvironment_availabilityZoneIds' - The identifier of the availability zones where subnets for the
-- environment are created.
--
-- 'awsAccountId', 'kxEnvironment_awsAccountId' - The unique identifier of the AWS account in which you create the kdb
-- environment.
--
-- 'certificateAuthorityArn', 'kxEnvironment_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the certificate authority:
--
-- 'creationTimestamp', 'kxEnvironment_creationTimestamp' - The timestamp at which the kdb environment was created in FinSpace. The
-- value is determined as epoch time in milliseconds. For example, the
-- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
-- 1635768000000.
--
-- 'customDNSConfiguration', 'kxEnvironment_customDNSConfiguration' - A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- 'dedicatedServiceAccountId', 'kxEnvironment_dedicatedServiceAccountId' - A unique identifier for the AWS environment infrastructure account.
--
-- 'description', 'kxEnvironment_description' - A description of the kdb environment.
--
-- 'dnsStatus', 'kxEnvironment_dnsStatus' - The status of DNS configuration.
--
-- 'environmentArn', 'kxEnvironment_environmentArn' - The Amazon Resource Name (ARN) of your kdb environment.
--
-- 'environmentId', 'kxEnvironment_environmentId' - A unique identifier for the kdb environment.
--
-- 'errorMessage', 'kxEnvironment_errorMessage' - Specifies the error message that appears if a flow fails.
--
-- 'kmsKeyId', 'kxEnvironment_kmsKeyId' - The unique identifier of the KMS key.
--
-- 'name', 'kxEnvironment_name' - The name of the kdb environment.
--
-- 'status', 'kxEnvironment_status' - The status of the environment creation.
--
-- -   CREATE_REQUESTED – Environment creation has been requested.
--
-- -   CREATING – Environment is in the process of being created.
--
-- -   FAILED_CREATION – Environment creation has failed.
--
-- -   CREATED – Environment is successfully created and is currently
--     active.
--
-- -   DELETE REQUESTED – Environment deletion has been requested.
--
-- -   DELETING – Environment is in the process of being deleted.
--
-- -   RETRY_DELETION – Initial environment deletion failed, system is
--     reattempting delete.
--
-- -   DELETED – Environment has been deleted.
--
-- -   FAILED_DELETION – Environment deletion has failed.
--
-- 'tgwStatus', 'kxEnvironment_tgwStatus' - The status of the network configuration.
--
-- 'transitGatewayConfiguration', 'kxEnvironment_transitGatewayConfiguration' - Specifies the transit gateway and network configuration to connect the
-- kdb environment to an internal network.
--
-- 'updateTimestamp', 'kxEnvironment_updateTimestamp' - The timestamp at which the kdb environment was modified in FinSpace. The
-- value is determined as epoch time in milliseconds. For example, the
-- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
-- 1635768000000.
newKxEnvironment ::
  KxEnvironment
newKxEnvironment =
  KxEnvironment'
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
      updateTimestamp = Prelude.Nothing
    }

-- | The identifier of the availability zones where subnets for the
-- environment are created.
kxEnvironment_availabilityZoneIds :: Lens.Lens' KxEnvironment (Prelude.Maybe [Prelude.Text])
kxEnvironment_availabilityZoneIds = Lens.lens (\KxEnvironment' {availabilityZoneIds} -> availabilityZoneIds) (\s@KxEnvironment' {} a -> s {availabilityZoneIds = a} :: KxEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the AWS account in which you create the kdb
-- environment.
kxEnvironment_awsAccountId :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_awsAccountId = Lens.lens (\KxEnvironment' {awsAccountId} -> awsAccountId) (\s@KxEnvironment' {} a -> s {awsAccountId = a} :: KxEnvironment)

-- | The Amazon Resource Name (ARN) of the certificate authority:
kxEnvironment_certificateAuthorityArn :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_certificateAuthorityArn = Lens.lens (\KxEnvironment' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@KxEnvironment' {} a -> s {certificateAuthorityArn = a} :: KxEnvironment)

-- | The timestamp at which the kdb environment was created in FinSpace. The
-- value is determined as epoch time in milliseconds. For example, the
-- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
-- 1635768000000.
kxEnvironment_creationTimestamp :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.UTCTime)
kxEnvironment_creationTimestamp = Lens.lens (\KxEnvironment' {creationTimestamp} -> creationTimestamp) (\s@KxEnvironment' {} a -> s {creationTimestamp = a} :: KxEnvironment) Prelude.. Lens.mapping Data._Time

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
kxEnvironment_customDNSConfiguration :: Lens.Lens' KxEnvironment (Prelude.Maybe [CustomDNSServer])
kxEnvironment_customDNSConfiguration = Lens.lens (\KxEnvironment' {customDNSConfiguration} -> customDNSConfiguration) (\s@KxEnvironment' {} a -> s {customDNSConfiguration = a} :: KxEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the AWS environment infrastructure account.
kxEnvironment_dedicatedServiceAccountId :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_dedicatedServiceAccountId = Lens.lens (\KxEnvironment' {dedicatedServiceAccountId} -> dedicatedServiceAccountId) (\s@KxEnvironment' {} a -> s {dedicatedServiceAccountId = a} :: KxEnvironment)

-- | A description of the kdb environment.
kxEnvironment_description :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_description = Lens.lens (\KxEnvironment' {description} -> description) (\s@KxEnvironment' {} a -> s {description = a} :: KxEnvironment)

-- | The status of DNS configuration.
kxEnvironment_dnsStatus :: Lens.Lens' KxEnvironment (Prelude.Maybe DnsStatus)
kxEnvironment_dnsStatus = Lens.lens (\KxEnvironment' {dnsStatus} -> dnsStatus) (\s@KxEnvironment' {} a -> s {dnsStatus = a} :: KxEnvironment)

-- | The Amazon Resource Name (ARN) of your kdb environment.
kxEnvironment_environmentArn :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_environmentArn = Lens.lens (\KxEnvironment' {environmentArn} -> environmentArn) (\s@KxEnvironment' {} a -> s {environmentArn = a} :: KxEnvironment)

-- | A unique identifier for the kdb environment.
kxEnvironment_environmentId :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_environmentId = Lens.lens (\KxEnvironment' {environmentId} -> environmentId) (\s@KxEnvironment' {} a -> s {environmentId = a} :: KxEnvironment)

-- | Specifies the error message that appears if a flow fails.
kxEnvironment_errorMessage :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_errorMessage = Lens.lens (\KxEnvironment' {errorMessage} -> errorMessage) (\s@KxEnvironment' {} a -> s {errorMessage = a} :: KxEnvironment)

-- | The unique identifier of the KMS key.
kxEnvironment_kmsKeyId :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_kmsKeyId = Lens.lens (\KxEnvironment' {kmsKeyId} -> kmsKeyId) (\s@KxEnvironment' {} a -> s {kmsKeyId = a} :: KxEnvironment)

-- | The name of the kdb environment.
kxEnvironment_name :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.Text)
kxEnvironment_name = Lens.lens (\KxEnvironment' {name} -> name) (\s@KxEnvironment' {} a -> s {name = a} :: KxEnvironment)

-- | The status of the environment creation.
--
-- -   CREATE_REQUESTED – Environment creation has been requested.
--
-- -   CREATING – Environment is in the process of being created.
--
-- -   FAILED_CREATION – Environment creation has failed.
--
-- -   CREATED – Environment is successfully created and is currently
--     active.
--
-- -   DELETE REQUESTED – Environment deletion has been requested.
--
-- -   DELETING – Environment is in the process of being deleted.
--
-- -   RETRY_DELETION – Initial environment deletion failed, system is
--     reattempting delete.
--
-- -   DELETED – Environment has been deleted.
--
-- -   FAILED_DELETION – Environment deletion has failed.
kxEnvironment_status :: Lens.Lens' KxEnvironment (Prelude.Maybe EnvironmentStatus)
kxEnvironment_status = Lens.lens (\KxEnvironment' {status} -> status) (\s@KxEnvironment' {} a -> s {status = a} :: KxEnvironment)

-- | The status of the network configuration.
kxEnvironment_tgwStatus :: Lens.Lens' KxEnvironment (Prelude.Maybe TgwStatus)
kxEnvironment_tgwStatus = Lens.lens (\KxEnvironment' {tgwStatus} -> tgwStatus) (\s@KxEnvironment' {} a -> s {tgwStatus = a} :: KxEnvironment)

-- | Specifies the transit gateway and network configuration to connect the
-- kdb environment to an internal network.
kxEnvironment_transitGatewayConfiguration :: Lens.Lens' KxEnvironment (Prelude.Maybe TransitGatewayConfiguration)
kxEnvironment_transitGatewayConfiguration = Lens.lens (\KxEnvironment' {transitGatewayConfiguration} -> transitGatewayConfiguration) (\s@KxEnvironment' {} a -> s {transitGatewayConfiguration = a} :: KxEnvironment)

-- | The timestamp at which the kdb environment was modified in FinSpace. The
-- value is determined as epoch time in milliseconds. For example, the
-- value for Monday, November 1, 2021 12:00:00 PM UTC is specified as
-- 1635768000000.
kxEnvironment_updateTimestamp :: Lens.Lens' KxEnvironment (Prelude.Maybe Prelude.UTCTime)
kxEnvironment_updateTimestamp = Lens.lens (\KxEnvironment' {updateTimestamp} -> updateTimestamp) (\s@KxEnvironment' {} a -> s {updateTimestamp = a} :: KxEnvironment) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON KxEnvironment where
  parseJSON =
    Data.withObject
      "KxEnvironment"
      ( \x ->
          KxEnvironment'
            Prelude.<$> ( x
                            Data..:? "availabilityZoneIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "certificateAuthorityArn")
            Prelude.<*> (x Data..:? "creationTimestamp")
            Prelude.<*> ( x
                            Data..:? "customDNSConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "dedicatedServiceAccountId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "dnsStatus")
            Prelude.<*> (x Data..:? "environmentArn")
            Prelude.<*> (x Data..:? "environmentId")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tgwStatus")
            Prelude.<*> (x Data..:? "transitGatewayConfiguration")
            Prelude.<*> (x Data..:? "updateTimestamp")
      )

instance Prelude.Hashable KxEnvironment where
  hashWithSalt _salt KxEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneIds
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` customDNSConfiguration
      `Prelude.hashWithSalt` dedicatedServiceAccountId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dnsStatus
      `Prelude.hashWithSalt` environmentArn
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tgwStatus
      `Prelude.hashWithSalt` transitGatewayConfiguration
      `Prelude.hashWithSalt` updateTimestamp

instance Prelude.NFData KxEnvironment where
  rnf KxEnvironment' {..} =
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
