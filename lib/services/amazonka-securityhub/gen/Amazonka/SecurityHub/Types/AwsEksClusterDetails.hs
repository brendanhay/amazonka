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
-- Module      : Amazonka.SecurityHub.Types.AwsEksClusterDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEksClusterLoggingDetails
import Amazonka.SecurityHub.Types.AwsEksClusterResourcesVpcConfigDetails

-- | Provides details about an Amazon EKS cluster.
--
-- /See:/ 'newAwsEksClusterDetails' smart constructor.
data AwsEksClusterDetails = AwsEksClusterDetails'
  { -- | The ARN of the cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The certificate authority data for the cluster.
    certificateAuthorityData :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster. Valid values are as follows:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATING@
    --
    -- -   @DELETING@
    --
    -- -   @FAILED@
    --
    -- -   @PENDING@
    --
    -- -   @UPDATING@
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the Amazon EKS API server.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The logging configuration for the cluster.
    logging :: Prelude.Maybe AwsEksClusterLoggingDetails,
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration used by the cluster control plane.
    resourcesVpcConfig :: Prelude.Maybe AwsEksClusterResourcesVpcConfigDetails,
    -- | The ARN of the IAM role that provides permissions for the Amazon EKS
    -- control plane to make calls to Amazon Web Services API operations on
    -- your behalf.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EKS server version for the cluster.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEksClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsEksClusterDetails_arn' - The ARN of the cluster.
--
-- 'certificateAuthorityData', 'awsEksClusterDetails_certificateAuthorityData' - The certificate authority data for the cluster.
--
-- 'clusterStatus', 'awsEksClusterDetails_clusterStatus' - The status of the cluster. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @FAILED@
--
-- -   @PENDING@
--
-- -   @UPDATING@
--
-- 'endpoint', 'awsEksClusterDetails_endpoint' - The endpoint for the Amazon EKS API server.
--
-- 'logging', 'awsEksClusterDetails_logging' - The logging configuration for the cluster.
--
-- 'name', 'awsEksClusterDetails_name' - The name of the cluster.
--
-- 'resourcesVpcConfig', 'awsEksClusterDetails_resourcesVpcConfig' - The VPC configuration used by the cluster control plane.
--
-- 'roleArn', 'awsEksClusterDetails_roleArn' - The ARN of the IAM role that provides permissions for the Amazon EKS
-- control plane to make calls to Amazon Web Services API operations on
-- your behalf.
--
-- 'version', 'awsEksClusterDetails_version' - The Amazon EKS server version for the cluster.
newAwsEksClusterDetails ::
  AwsEksClusterDetails
newAwsEksClusterDetails =
  AwsEksClusterDetails'
    { arn = Prelude.Nothing,
      certificateAuthorityData = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      logging = Prelude.Nothing,
      name = Prelude.Nothing,
      resourcesVpcConfig = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The ARN of the cluster.
awsEksClusterDetails_arn :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_arn = Lens.lens (\AwsEksClusterDetails' {arn} -> arn) (\s@AwsEksClusterDetails' {} a -> s {arn = a} :: AwsEksClusterDetails)

-- | The certificate authority data for the cluster.
awsEksClusterDetails_certificateAuthorityData :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_certificateAuthorityData = Lens.lens (\AwsEksClusterDetails' {certificateAuthorityData} -> certificateAuthorityData) (\s@AwsEksClusterDetails' {} a -> s {certificateAuthorityData = a} :: AwsEksClusterDetails)

-- | The status of the cluster. Valid values are as follows:
--
-- -   @ACTIVE@
--
-- -   @CREATING@
--
-- -   @DELETING@
--
-- -   @FAILED@
--
-- -   @PENDING@
--
-- -   @UPDATING@
awsEksClusterDetails_clusterStatus :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_clusterStatus = Lens.lens (\AwsEksClusterDetails' {clusterStatus} -> clusterStatus) (\s@AwsEksClusterDetails' {} a -> s {clusterStatus = a} :: AwsEksClusterDetails)

-- | The endpoint for the Amazon EKS API server.
awsEksClusterDetails_endpoint :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_endpoint = Lens.lens (\AwsEksClusterDetails' {endpoint} -> endpoint) (\s@AwsEksClusterDetails' {} a -> s {endpoint = a} :: AwsEksClusterDetails)

-- | The logging configuration for the cluster.
awsEksClusterDetails_logging :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe AwsEksClusterLoggingDetails)
awsEksClusterDetails_logging = Lens.lens (\AwsEksClusterDetails' {logging} -> logging) (\s@AwsEksClusterDetails' {} a -> s {logging = a} :: AwsEksClusterDetails)

-- | The name of the cluster.
awsEksClusterDetails_name :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_name = Lens.lens (\AwsEksClusterDetails' {name} -> name) (\s@AwsEksClusterDetails' {} a -> s {name = a} :: AwsEksClusterDetails)

-- | The VPC configuration used by the cluster control plane.
awsEksClusterDetails_resourcesVpcConfig :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe AwsEksClusterResourcesVpcConfigDetails)
awsEksClusterDetails_resourcesVpcConfig = Lens.lens (\AwsEksClusterDetails' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@AwsEksClusterDetails' {} a -> s {resourcesVpcConfig = a} :: AwsEksClusterDetails)

-- | The ARN of the IAM role that provides permissions for the Amazon EKS
-- control plane to make calls to Amazon Web Services API operations on
-- your behalf.
awsEksClusterDetails_roleArn :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_roleArn = Lens.lens (\AwsEksClusterDetails' {roleArn} -> roleArn) (\s@AwsEksClusterDetails' {} a -> s {roleArn = a} :: AwsEksClusterDetails)

-- | The Amazon EKS server version for the cluster.
awsEksClusterDetails_version :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_version = Lens.lens (\AwsEksClusterDetails' {version} -> version) (\s@AwsEksClusterDetails' {} a -> s {version = a} :: AwsEksClusterDetails)

instance Data.FromJSON AwsEksClusterDetails where
  parseJSON =
    Data.withObject
      "AwsEksClusterDetails"
      ( \x ->
          AwsEksClusterDetails'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CertificateAuthorityData")
            Prelude.<*> (x Data..:? "ClusterStatus")
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "Logging")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ResourcesVpcConfig")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable AwsEksClusterDetails where
  hashWithSalt _salt AwsEksClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` certificateAuthorityData
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourcesVpcConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` version

instance Prelude.NFData AwsEksClusterDetails where
  rnf AwsEksClusterDetails' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf certificateAuthorityData
      `Prelude.seq` Prelude.rnf clusterStatus
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourcesVpcConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON AwsEksClusterDetails where
  toJSON AwsEksClusterDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("CertificateAuthorityData" Data..=)
              Prelude.<$> certificateAuthorityData,
            ("ClusterStatus" Data..=) Prelude.<$> clusterStatus,
            ("Endpoint" Data..=) Prelude.<$> endpoint,
            ("Logging" Data..=) Prelude.<$> logging,
            ("Name" Data..=) Prelude.<$> name,
            ("ResourcesVpcConfig" Data..=)
              Prelude.<$> resourcesVpcConfig,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
