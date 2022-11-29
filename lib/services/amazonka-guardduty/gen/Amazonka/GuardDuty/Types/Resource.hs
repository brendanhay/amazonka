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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.AccessKeyDetails
import Amazonka.GuardDuty.Types.Container
import Amazonka.GuardDuty.Types.EbsVolumeDetails
import Amazonka.GuardDuty.Types.EcsClusterDetails
import Amazonka.GuardDuty.Types.EksClusterDetails
import Amazonka.GuardDuty.Types.InstanceDetails
import Amazonka.GuardDuty.Types.KubernetesDetails
import Amazonka.GuardDuty.Types.S3BucketDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the Amazon Web Services resource associated
-- with the activity that prompted GuardDuty to generate a finding.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The information about the EC2 instance associated with the activity that
    -- prompted GuardDuty to generate a finding.
    instanceDetails :: Prelude.Maybe InstanceDetails,
    -- | Contains information on the S3 bucket.
    s3BucketDetails :: Prelude.Maybe [S3BucketDetail],
    -- | Contains information about the details of the ECS Cluster.
    ecsClusterDetails :: Prelude.Maybe EcsClusterDetails,
    containerDetails :: Prelude.Maybe Container,
    -- | The IAM access key details (IAM user information) of a user that engaged
    -- in the activity that prompted GuardDuty to generate a finding.
    accessKeyDetails :: Prelude.Maybe AccessKeyDetails,
    -- | Details about the EKS cluster involved in a Kubernetes finding.
    eksClusterDetails :: Prelude.Maybe EksClusterDetails,
    -- | Details about the Kubernetes user and workload involved in a Kubernetes
    -- finding.
    kubernetesDetails :: Prelude.Maybe KubernetesDetails,
    -- | Contains list of scanned and skipped EBS volumes with details.
    ebsVolumeDetails :: Prelude.Maybe EbsVolumeDetails
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
-- 'resourceType', 'resource_resourceType' - The type of Amazon Web Services resource.
--
-- 'instanceDetails', 'resource_instanceDetails' - The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
--
-- 's3BucketDetails', 'resource_s3BucketDetails' - Contains information on the S3 bucket.
--
-- 'ecsClusterDetails', 'resource_ecsClusterDetails' - Contains information about the details of the ECS Cluster.
--
-- 'containerDetails', 'resource_containerDetails' - Undocumented member.
--
-- 'accessKeyDetails', 'resource_accessKeyDetails' - The IAM access key details (IAM user information) of a user that engaged
-- in the activity that prompted GuardDuty to generate a finding.
--
-- 'eksClusterDetails', 'resource_eksClusterDetails' - Details about the EKS cluster involved in a Kubernetes finding.
--
-- 'kubernetesDetails', 'resource_kubernetesDetails' - Details about the Kubernetes user and workload involved in a Kubernetes
-- finding.
--
-- 'ebsVolumeDetails', 'resource_ebsVolumeDetails' - Contains list of scanned and skipped EBS volumes with details.
newResource ::
  Resource
newResource =
  Resource'
    { resourceType = Prelude.Nothing,
      instanceDetails = Prelude.Nothing,
      s3BucketDetails = Prelude.Nothing,
      ecsClusterDetails = Prelude.Nothing,
      containerDetails = Prelude.Nothing,
      accessKeyDetails = Prelude.Nothing,
      eksClusterDetails = Prelude.Nothing,
      kubernetesDetails = Prelude.Nothing,
      ebsVolumeDetails = Prelude.Nothing
    }

-- | The type of Amazon Web Services resource.
resource_resourceType :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_resourceType = Lens.lens (\Resource' {resourceType} -> resourceType) (\s@Resource' {} a -> s {resourceType = a} :: Resource)

-- | The information about the EC2 instance associated with the activity that
-- prompted GuardDuty to generate a finding.
resource_instanceDetails :: Lens.Lens' Resource (Prelude.Maybe InstanceDetails)
resource_instanceDetails = Lens.lens (\Resource' {instanceDetails} -> instanceDetails) (\s@Resource' {} a -> s {instanceDetails = a} :: Resource)

-- | Contains information on the S3 bucket.
resource_s3BucketDetails :: Lens.Lens' Resource (Prelude.Maybe [S3BucketDetail])
resource_s3BucketDetails = Lens.lens (\Resource' {s3BucketDetails} -> s3BucketDetails) (\s@Resource' {} a -> s {s3BucketDetails = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the details of the ECS Cluster.
resource_ecsClusterDetails :: Lens.Lens' Resource (Prelude.Maybe EcsClusterDetails)
resource_ecsClusterDetails = Lens.lens (\Resource' {ecsClusterDetails} -> ecsClusterDetails) (\s@Resource' {} a -> s {ecsClusterDetails = a} :: Resource)

-- | Undocumented member.
resource_containerDetails :: Lens.Lens' Resource (Prelude.Maybe Container)
resource_containerDetails = Lens.lens (\Resource' {containerDetails} -> containerDetails) (\s@Resource' {} a -> s {containerDetails = a} :: Resource)

-- | The IAM access key details (IAM user information) of a user that engaged
-- in the activity that prompted GuardDuty to generate a finding.
resource_accessKeyDetails :: Lens.Lens' Resource (Prelude.Maybe AccessKeyDetails)
resource_accessKeyDetails = Lens.lens (\Resource' {accessKeyDetails} -> accessKeyDetails) (\s@Resource' {} a -> s {accessKeyDetails = a} :: Resource)

-- | Details about the EKS cluster involved in a Kubernetes finding.
resource_eksClusterDetails :: Lens.Lens' Resource (Prelude.Maybe EksClusterDetails)
resource_eksClusterDetails = Lens.lens (\Resource' {eksClusterDetails} -> eksClusterDetails) (\s@Resource' {} a -> s {eksClusterDetails = a} :: Resource)

-- | Details about the Kubernetes user and workload involved in a Kubernetes
-- finding.
resource_kubernetesDetails :: Lens.Lens' Resource (Prelude.Maybe KubernetesDetails)
resource_kubernetesDetails = Lens.lens (\Resource' {kubernetesDetails} -> kubernetesDetails) (\s@Resource' {} a -> s {kubernetesDetails = a} :: Resource)

-- | Contains list of scanned and skipped EBS volumes with details.
resource_ebsVolumeDetails :: Lens.Lens' Resource (Prelude.Maybe EbsVolumeDetails)
resource_ebsVolumeDetails = Lens.lens (\Resource' {ebsVolumeDetails} -> ebsVolumeDetails) (\s@Resource' {} a -> s {ebsVolumeDetails = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "instanceDetails")
            Prelude.<*> ( x Core..:? "s3BucketDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ecsClusterDetails")
            Prelude.<*> (x Core..:? "containerDetails")
            Prelude.<*> (x Core..:? "accessKeyDetails")
            Prelude.<*> (x Core..:? "eksClusterDetails")
            Prelude.<*> (x Core..:? "kubernetesDetails")
            Prelude.<*> (x Core..:? "ebsVolumeDetails")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` instanceDetails
      `Prelude.hashWithSalt` s3BucketDetails
      `Prelude.hashWithSalt` ecsClusterDetails
      `Prelude.hashWithSalt` containerDetails
      `Prelude.hashWithSalt` accessKeyDetails
      `Prelude.hashWithSalt` eksClusterDetails
      `Prelude.hashWithSalt` kubernetesDetails
      `Prelude.hashWithSalt` ebsVolumeDetails

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf instanceDetails
      `Prelude.seq` Prelude.rnf s3BucketDetails
      `Prelude.seq` Prelude.rnf ecsClusterDetails
      `Prelude.seq` Prelude.rnf containerDetails
      `Prelude.seq` Prelude.rnf accessKeyDetails
      `Prelude.seq` Prelude.rnf eksClusterDetails
      `Prelude.seq` Prelude.rnf kubernetesDetails
      `Prelude.seq` Prelude.rnf ebsVolumeDetails
