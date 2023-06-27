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
-- Module      : Amazonka.GuardDuty.Types.Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.AccessKeyDetails
import Amazonka.GuardDuty.Types.Container
import Amazonka.GuardDuty.Types.EbsVolumeDetails
import Amazonka.GuardDuty.Types.EcsClusterDetails
import Amazonka.GuardDuty.Types.EksClusterDetails
import Amazonka.GuardDuty.Types.InstanceDetails
import Amazonka.GuardDuty.Types.KubernetesDetails
import Amazonka.GuardDuty.Types.LambdaDetails
import Amazonka.GuardDuty.Types.RdsDbInstanceDetails
import Amazonka.GuardDuty.Types.RdsDbUserDetails
import Amazonka.GuardDuty.Types.S3BucketDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Web Services resource associated
-- with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The IAM access key details (user information) of a user that engaged in
    -- the activity that prompted GuardDuty to generate a finding.
    accessKeyDetails :: Prelude.Maybe AccessKeyDetails,
    containerDetails :: Prelude.Maybe Container,
    -- | Contains list of scanned and skipped EBS volumes with details.
    ebsVolumeDetails :: Prelude.Maybe EbsVolumeDetails,
    -- | Contains information about the details of the ECS Cluster.
    ecsClusterDetails :: Prelude.Maybe EcsClusterDetails,
    -- | Details about the EKS cluster involved in a Kubernetes finding.
    eksClusterDetails :: Prelude.Maybe EksClusterDetails,
    -- | The information about the EC2 instance associated with the activity that
    -- prompted GuardDuty to generate a finding.
    instanceDetails :: Prelude.Maybe InstanceDetails,
    -- | Details about the Kubernetes user and workload involved in a Kubernetes
    -- finding.
    kubernetesDetails :: Prelude.Maybe KubernetesDetails,
    -- | Contains information about the Lambda function that was involved in a
    -- finding.
    lambdaDetails :: Prelude.Maybe LambdaDetails,
    -- | Contains information about the database instance to which an anomalous
    -- login attempt was made.
    rdsDbInstanceDetails :: Prelude.Maybe RdsDbInstanceDetails,
    -- | Contains information about the user details through which anomalous
    -- login attempt was made.
    rdsDbUserDetails :: Prelude.Maybe RdsDbUserDetails,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Contains information on the S3 bucket.
    s3BucketDetails :: Prelude.Maybe [S3BucketDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyDetails', 'resource_accessKeyDetails' - The IAM access key details (user information) of a user that engaged in
-- the activity that prompted GuardDuty to generate a finding.
--
-- 'containerDetails', 'resource_containerDetails' - Undocumented member.
--
-- 'ebsVolumeDetails', 'resource_ebsVolumeDetails' - Contains list of scanned and skipped EBS volumes with details.
--
-- 'ecsClusterDetails', 'resource_ecsClusterDetails' - Contains information about the details of the ECS Cluster.
--
-- 'eksClusterDetails', 'resource_eksClusterDetails' - Details about the EKS cluster involved in a Kubernetes finding.
--
-- 'instanceDetails', 'resource_instanceDetails' - The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
--
-- 'kubernetesDetails', 'resource_kubernetesDetails' - Details about the Kubernetes user and workload involved in a Kubernetes
-- finding.
--
-- 'lambdaDetails', 'resource_lambdaDetails' - Contains information about the Lambda function that was involved in a
-- finding.
--
-- 'rdsDbInstanceDetails', 'resource_rdsDbInstanceDetails' - Contains information about the database instance to which an anomalous
-- login attempt was made.
--
-- 'rdsDbUserDetails', 'resource_rdsDbUserDetails' - Contains information about the user details through which anomalous
-- login attempt was made.
--
-- 'resourceType', 'resource_resourceType' - The type of Amazon Web Services resource.
--
-- 's3BucketDetails', 'resource_s3BucketDetails' - Contains information on the S3 bucket.
newResource ::
  Resource
newResource =
  Resource'
    { accessKeyDetails = Prelude.Nothing,
      containerDetails = Prelude.Nothing,
      ebsVolumeDetails = Prelude.Nothing,
      ecsClusterDetails = Prelude.Nothing,
      eksClusterDetails = Prelude.Nothing,
      instanceDetails = Prelude.Nothing,
      kubernetesDetails = Prelude.Nothing,
      lambdaDetails = Prelude.Nothing,
      rdsDbInstanceDetails = Prelude.Nothing,
      rdsDbUserDetails = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      s3BucketDetails = Prelude.Nothing
    }

-- | The IAM access key details (user information) of a user that engaged in
-- the activity that prompted GuardDuty to generate a finding.
resource_accessKeyDetails :: Lens.Lens' Resource (Prelude.Maybe AccessKeyDetails)
resource_accessKeyDetails = Lens.lens (\Resource' {accessKeyDetails} -> accessKeyDetails) (\s@Resource' {} a -> s {accessKeyDetails = a} :: Resource)

-- | Undocumented member.
resource_containerDetails :: Lens.Lens' Resource (Prelude.Maybe Container)
resource_containerDetails = Lens.lens (\Resource' {containerDetails} -> containerDetails) (\s@Resource' {} a -> s {containerDetails = a} :: Resource)

-- | Contains list of scanned and skipped EBS volumes with details.
resource_ebsVolumeDetails :: Lens.Lens' Resource (Prelude.Maybe EbsVolumeDetails)
resource_ebsVolumeDetails = Lens.lens (\Resource' {ebsVolumeDetails} -> ebsVolumeDetails) (\s@Resource' {} a -> s {ebsVolumeDetails = a} :: Resource)

-- | Contains information about the details of the ECS Cluster.
resource_ecsClusterDetails :: Lens.Lens' Resource (Prelude.Maybe EcsClusterDetails)
resource_ecsClusterDetails = Lens.lens (\Resource' {ecsClusterDetails} -> ecsClusterDetails) (\s@Resource' {} a -> s {ecsClusterDetails = a} :: Resource)

-- | Details about the EKS cluster involved in a Kubernetes finding.
resource_eksClusterDetails :: Lens.Lens' Resource (Prelude.Maybe EksClusterDetails)
resource_eksClusterDetails = Lens.lens (\Resource' {eksClusterDetails} -> eksClusterDetails) (\s@Resource' {} a -> s {eksClusterDetails = a} :: Resource)

-- | The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
resource_instanceDetails :: Lens.Lens' Resource (Prelude.Maybe InstanceDetails)
resource_instanceDetails = Lens.lens (\Resource' {instanceDetails} -> instanceDetails) (\s@Resource' {} a -> s {instanceDetails = a} :: Resource)

-- | Details about the Kubernetes user and workload involved in a Kubernetes
-- finding.
resource_kubernetesDetails :: Lens.Lens' Resource (Prelude.Maybe KubernetesDetails)
resource_kubernetesDetails = Lens.lens (\Resource' {kubernetesDetails} -> kubernetesDetails) (\s@Resource' {} a -> s {kubernetesDetails = a} :: Resource)

-- | Contains information about the Lambda function that was involved in a
-- finding.
resource_lambdaDetails :: Lens.Lens' Resource (Prelude.Maybe LambdaDetails)
resource_lambdaDetails = Lens.lens (\Resource' {lambdaDetails} -> lambdaDetails) (\s@Resource' {} a -> s {lambdaDetails = a} :: Resource)

-- | Contains information about the database instance to which an anomalous
-- login attempt was made.
resource_rdsDbInstanceDetails :: Lens.Lens' Resource (Prelude.Maybe RdsDbInstanceDetails)
resource_rdsDbInstanceDetails = Lens.lens (\Resource' {rdsDbInstanceDetails} -> rdsDbInstanceDetails) (\s@Resource' {} a -> s {rdsDbInstanceDetails = a} :: Resource)

-- | Contains information about the user details through which anomalous
-- login attempt was made.
resource_rdsDbUserDetails :: Lens.Lens' Resource (Prelude.Maybe RdsDbUserDetails)
resource_rdsDbUserDetails = Lens.lens (\Resource' {rdsDbUserDetails} -> rdsDbUserDetails) (\s@Resource' {} a -> s {rdsDbUserDetails = a} :: Resource)

-- | The type of Amazon Web Services resource.
resource_resourceType :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceType = Lens.lens (\Resource' {resourceType} -> resourceType) (\s@Resource' {} a -> s {resourceType = a} :: Resource)

-- | Contains information on the S3 bucket.
resource_s3BucketDetails :: Lens.Lens' Resource (Prelude.Maybe [S3BucketDetail])
resource_s3BucketDetails = Lens.lens (\Resource' {s3BucketDetails} -> s3BucketDetails) (\s@Resource' {} a -> s {s3BucketDetails = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "accessKeyDetails")
            Prelude.<*> (x Data..:? "containerDetails")
            Prelude.<*> (x Data..:? "ebsVolumeDetails")
            Prelude.<*> (x Data..:? "ecsClusterDetails")
            Prelude.<*> (x Data..:? "eksClusterDetails")
            Prelude.<*> (x Data..:? "instanceDetails")
            Prelude.<*> (x Data..:? "kubernetesDetails")
            Prelude.<*> (x Data..:? "lambdaDetails")
            Prelude.<*> (x Data..:? "rdsDbInstanceDetails")
            Prelude.<*> (x Data..:? "rdsDbUserDetails")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> ( x
                            Data..:? "s3BucketDetails"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt
      `Prelude.hashWithSalt` accessKeyDetails
      `Prelude.hashWithSalt` containerDetails
      `Prelude.hashWithSalt` ebsVolumeDetails
      `Prelude.hashWithSalt` ecsClusterDetails
      `Prelude.hashWithSalt` eksClusterDetails
      `Prelude.hashWithSalt` instanceDetails
      `Prelude.hashWithSalt` kubernetesDetails
      `Prelude.hashWithSalt` lambdaDetails
      `Prelude.hashWithSalt` rdsDbInstanceDetails
      `Prelude.hashWithSalt` rdsDbUserDetails
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` s3BucketDetails

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf accessKeyDetails
      `Prelude.seq` Prelude.rnf containerDetails
      `Prelude.seq` Prelude.rnf ebsVolumeDetails
      `Prelude.seq` Prelude.rnf ecsClusterDetails
      `Prelude.seq` Prelude.rnf eksClusterDetails
      `Prelude.seq` Prelude.rnf instanceDetails
      `Prelude.seq` Prelude.rnf kubernetesDetails
      `Prelude.seq` Prelude.rnf lambdaDetails
      `Prelude.seq` Prelude.rnf rdsDbInstanceDetails
      `Prelude.seq` Prelude.rnf rdsDbUserDetails
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf s3BucketDetails
