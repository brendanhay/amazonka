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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEksClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    -- | The name of the cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EKS server version for the cluster.
    version :: Prelude.Maybe Prelude.Text,
    -- | The logging configuration for the cluster.
    logging :: Prelude.Maybe AwsEksClusterLoggingDetails,
    -- | The endpoint for the Amazon EKS API server.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration used by the cluster control plane.
    resourcesVpcConfig :: Prelude.Maybe AwsEksClusterResourcesVpcConfigDetails,
    -- | The status of the cluster.
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that provides permissions for the Amazon EKS
    -- control plane to make calls to Amazon Web Services API operations on
    -- your behalf.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'name', 'awsEksClusterDetails_name' - The name of the cluster.
--
-- 'version', 'awsEksClusterDetails_version' - The Amazon EKS server version for the cluster.
--
-- 'logging', 'awsEksClusterDetails_logging' - The logging configuration for the cluster.
--
-- 'endpoint', 'awsEksClusterDetails_endpoint' - The endpoint for the Amazon EKS API server.
--
-- 'resourcesVpcConfig', 'awsEksClusterDetails_resourcesVpcConfig' - The VPC configuration used by the cluster control plane.
--
-- 'clusterStatus', 'awsEksClusterDetails_clusterStatus' - The status of the cluster.
--
-- 'roleArn', 'awsEksClusterDetails_roleArn' - The ARN of the IAM role that provides permissions for the Amazon EKS
-- control plane to make calls to Amazon Web Services API operations on
-- your behalf.
newAwsEksClusterDetails ::
  AwsEksClusterDetails
newAwsEksClusterDetails =
  AwsEksClusterDetails'
    { arn = Prelude.Nothing,
      certificateAuthorityData = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      logging = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      resourcesVpcConfig = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The ARN of the cluster.
awsEksClusterDetails_arn :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_arn = Lens.lens (\AwsEksClusterDetails' {arn} -> arn) (\s@AwsEksClusterDetails' {} a -> s {arn = a} :: AwsEksClusterDetails)

-- | The certificate authority data for the cluster.
awsEksClusterDetails_certificateAuthorityData :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_certificateAuthorityData = Lens.lens (\AwsEksClusterDetails' {certificateAuthorityData} -> certificateAuthorityData) (\s@AwsEksClusterDetails' {} a -> s {certificateAuthorityData = a} :: AwsEksClusterDetails)

-- | The name of the cluster.
awsEksClusterDetails_name :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_name = Lens.lens (\AwsEksClusterDetails' {name} -> name) (\s@AwsEksClusterDetails' {} a -> s {name = a} :: AwsEksClusterDetails)

-- | The Amazon EKS server version for the cluster.
awsEksClusterDetails_version :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_version = Lens.lens (\AwsEksClusterDetails' {version} -> version) (\s@AwsEksClusterDetails' {} a -> s {version = a} :: AwsEksClusterDetails)

-- | The logging configuration for the cluster.
awsEksClusterDetails_logging :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe AwsEksClusterLoggingDetails)
awsEksClusterDetails_logging = Lens.lens (\AwsEksClusterDetails' {logging} -> logging) (\s@AwsEksClusterDetails' {} a -> s {logging = a} :: AwsEksClusterDetails)

-- | The endpoint for the Amazon EKS API server.
awsEksClusterDetails_endpoint :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_endpoint = Lens.lens (\AwsEksClusterDetails' {endpoint} -> endpoint) (\s@AwsEksClusterDetails' {} a -> s {endpoint = a} :: AwsEksClusterDetails)

-- | The VPC configuration used by the cluster control plane.
awsEksClusterDetails_resourcesVpcConfig :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe AwsEksClusterResourcesVpcConfigDetails)
awsEksClusterDetails_resourcesVpcConfig = Lens.lens (\AwsEksClusterDetails' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@AwsEksClusterDetails' {} a -> s {resourcesVpcConfig = a} :: AwsEksClusterDetails)

-- | The status of the cluster.
awsEksClusterDetails_clusterStatus :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_clusterStatus = Lens.lens (\AwsEksClusterDetails' {clusterStatus} -> clusterStatus) (\s@AwsEksClusterDetails' {} a -> s {clusterStatus = a} :: AwsEksClusterDetails)

-- | The ARN of the IAM role that provides permissions for the Amazon EKS
-- control plane to make calls to Amazon Web Services API operations on
-- your behalf.
awsEksClusterDetails_roleArn :: Lens.Lens' AwsEksClusterDetails (Prelude.Maybe Prelude.Text)
awsEksClusterDetails_roleArn = Lens.lens (\AwsEksClusterDetails' {roleArn} -> roleArn) (\s@AwsEksClusterDetails' {} a -> s {roleArn = a} :: AwsEksClusterDetails)

instance Core.FromJSON AwsEksClusterDetails where
  parseJSON =
    Core.withObject
      "AwsEksClusterDetails"
      ( \x ->
          AwsEksClusterDetails'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CertificateAuthorityData")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Logging")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "ResourcesVpcConfig")
            Prelude.<*> (x Core..:? "ClusterStatus")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable AwsEksClusterDetails where
  hashWithSalt salt' AwsEksClusterDetails' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` resourcesVpcConfig
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` certificateAuthorityData
      `Prelude.hashWithSalt` arn

instance Prelude.NFData AwsEksClusterDetails where
  rnf AwsEksClusterDetails' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf clusterStatus
      `Prelude.seq` Prelude.rnf resourcesVpcConfig
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf certificateAuthorityData

instance Core.ToJSON AwsEksClusterDetails where
  toJSON AwsEksClusterDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Arn" Core..=) Prelude.<$> arn,
            ("CertificateAuthorityData" Core..=)
              Prelude.<$> certificateAuthorityData,
            ("Name" Core..=) Prelude.<$> name,
            ("Version" Core..=) Prelude.<$> version,
            ("Logging" Core..=) Prelude.<$> logging,
            ("Endpoint" Core..=) Prelude.<$> endpoint,
            ("ResourcesVpcConfig" Core..=)
              Prelude.<$> resourcesVpcConfig,
            ("ClusterStatus" Core..=) Prelude.<$> clusterStatus,
            ("RoleArn" Core..=) Prelude.<$> roleArn
          ]
      )
