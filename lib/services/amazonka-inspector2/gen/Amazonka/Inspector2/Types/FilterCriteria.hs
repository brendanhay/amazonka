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
-- Module      : Amazonka.Inspector2.Types.FilterCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.DateFilter
import Amazonka.Inspector2.Types.MapFilter
import Amazonka.Inspector2.Types.NumberFilter
import Amazonka.Inspector2.Types.PackageFilter
import Amazonka.Inspector2.Types.PortRangeFilter
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | Details on the criteria used to define the filter.
--
-- /See:/ 'newFilterCriteria' smart constructor.
data FilterCriteria = FilterCriteria'
  { -- | Details of the Amazon Web Services account IDs used to filter findings.
    awsAccountId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon EC2 instance image IDs used to filter findings.
    ec2InstanceImageId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the ingress source addresses used to filter findings.
    networkProtocol :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the resource IDs used to filter findings.
    resourceId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the resource types used to filter findings.
    resourceType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the Amazon ECR registry used to filter findings.
    ecrImageRegistry :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the severity used to filter findings.
    severity :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the port ranges used to filter findings.
    portRange :: Prelude.Maybe (Prelude.NonEmpty PortRangeFilter),
    -- | Details of the Amazon ECR image architecture types used to filter
    -- findings.
    ecrImageArchitecture :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding status types used to filter findings.
    findingStatus :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the vulnerable packages used to filter findings.
    vulnerablePackages :: Prelude.Maybe (Prelude.NonEmpty PackageFilter),
    -- | Details on the vulnerability type used to filter findings.
    vulnerabilitySource :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the name of the Amazon ECR repository used to filter
    -- findings.
    ecrImageRepositoryName :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The Amazon Inspector score to filter on.
    inspectorScore :: Prelude.Maybe (Prelude.NonEmpty NumberFilter),
    -- | The tags attached to the Amazon ECR container image.
    ecrImageTags :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the resource tags used to filter findings.
    resourceTags :: Prelude.Maybe (Prelude.NonEmpty MapFilter),
    -- | Details of the Amazon ECR image hashes used to filter findings.
    ecrImageHash :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding title used to filter findings.
    title :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was first seen used to filter
    -- findings.
    firstObservedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on the Amazon ECR image push date and time used to filter
    -- findings.
    ecrImagePushedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on the vendor severity used to filter findings.
    vendorSeverity :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was last seen used to filter
    -- findings.
    lastObservedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details of the Amazon EC2 instance VPC IDs used to filter findings.
    ec2InstanceVpcId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the component IDs used to filter findings.
    componentId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the related vulnerabilities used to filter findings.
    relatedVulnerabilities :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding types used to filter findings.
    findingType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the component types used to filter findings.
    componentType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the vulnerability ID used to filter findings.
    vulnerabilityId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on whether a fix is available through a version update. This
    -- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
    -- but not all, of the packages identified in the finding have fixes
    -- available through updated versions.
    fixAvailable :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding ARNs used to filter findings.
    findingArn :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon EC2 instance subnet IDs used to filter findings.
    ec2InstanceSubnetId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was last updated at used to
    -- filter findings.
    updatedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'filterCriteria_awsAccountId' - Details of the Amazon Web Services account IDs used to filter findings.
--
-- 'ec2InstanceImageId', 'filterCriteria_ec2InstanceImageId' - Details of the Amazon EC2 instance image IDs used to filter findings.
--
-- 'networkProtocol', 'filterCriteria_networkProtocol' - Details on the ingress source addresses used to filter findings.
--
-- 'resourceId', 'filterCriteria_resourceId' - Details on the resource IDs used to filter findings.
--
-- 'resourceType', 'filterCriteria_resourceType' - Details on the resource types used to filter findings.
--
-- 'ecrImageRegistry', 'filterCriteria_ecrImageRegistry' - Details on the Amazon ECR registry used to filter findings.
--
-- 'severity', 'filterCriteria_severity' - Details on the severity used to filter findings.
--
-- 'portRange', 'filterCriteria_portRange' - Details on the port ranges used to filter findings.
--
-- 'ecrImageArchitecture', 'filterCriteria_ecrImageArchitecture' - Details of the Amazon ECR image architecture types used to filter
-- findings.
--
-- 'findingStatus', 'filterCriteria_findingStatus' - Details on the finding status types used to filter findings.
--
-- 'vulnerablePackages', 'filterCriteria_vulnerablePackages' - Details on the vulnerable packages used to filter findings.
--
-- 'vulnerabilitySource', 'filterCriteria_vulnerabilitySource' - Details on the vulnerability type used to filter findings.
--
-- 'ecrImageRepositoryName', 'filterCriteria_ecrImageRepositoryName' - Details on the name of the Amazon ECR repository used to filter
-- findings.
--
-- 'inspectorScore', 'filterCriteria_inspectorScore' - The Amazon Inspector score to filter on.
--
-- 'ecrImageTags', 'filterCriteria_ecrImageTags' - The tags attached to the Amazon ECR container image.
--
-- 'resourceTags', 'filterCriteria_resourceTags' - Details on the resource tags used to filter findings.
--
-- 'ecrImageHash', 'filterCriteria_ecrImageHash' - Details of the Amazon ECR image hashes used to filter findings.
--
-- 'title', 'filterCriteria_title' - Details on the finding title used to filter findings.
--
-- 'firstObservedAt', 'filterCriteria_firstObservedAt' - Details on the date and time a finding was first seen used to filter
-- findings.
--
-- 'ecrImagePushedAt', 'filterCriteria_ecrImagePushedAt' - Details on the Amazon ECR image push date and time used to filter
-- findings.
--
-- 'vendorSeverity', 'filterCriteria_vendorSeverity' - Details on the vendor severity used to filter findings.
--
-- 'lastObservedAt', 'filterCriteria_lastObservedAt' - Details on the date and time a finding was last seen used to filter
-- findings.
--
-- 'ec2InstanceVpcId', 'filterCriteria_ec2InstanceVpcId' - Details of the Amazon EC2 instance VPC IDs used to filter findings.
--
-- 'componentId', 'filterCriteria_componentId' - Details of the component IDs used to filter findings.
--
-- 'relatedVulnerabilities', 'filterCriteria_relatedVulnerabilities' - Details on the related vulnerabilities used to filter findings.
--
-- 'findingType', 'filterCriteria_findingType' - Details on the finding types used to filter findings.
--
-- 'componentType', 'filterCriteria_componentType' - Details of the component types used to filter findings.
--
-- 'vulnerabilityId', 'filterCriteria_vulnerabilityId' - Details on the vulnerability ID used to filter findings.
--
-- 'fixAvailable', 'filterCriteria_fixAvailable' - Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
--
-- 'findingArn', 'filterCriteria_findingArn' - Details on the finding ARNs used to filter findings.
--
-- 'ec2InstanceSubnetId', 'filterCriteria_ec2InstanceSubnetId' - Details of the Amazon EC2 instance subnet IDs used to filter findings.
--
-- 'updatedAt', 'filterCriteria_updatedAt' - Details on the date and time a finding was last updated at used to
-- filter findings.
newFilterCriteria ::
  FilterCriteria
newFilterCriteria =
  FilterCriteria'
    { awsAccountId = Prelude.Nothing,
      ec2InstanceImageId = Prelude.Nothing,
      networkProtocol = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      ecrImageRegistry = Prelude.Nothing,
      severity = Prelude.Nothing,
      portRange = Prelude.Nothing,
      ecrImageArchitecture = Prelude.Nothing,
      findingStatus = Prelude.Nothing,
      vulnerablePackages = Prelude.Nothing,
      vulnerabilitySource = Prelude.Nothing,
      ecrImageRepositoryName = Prelude.Nothing,
      inspectorScore = Prelude.Nothing,
      ecrImageTags = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      ecrImageHash = Prelude.Nothing,
      title = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      ecrImagePushedAt = Prelude.Nothing,
      vendorSeverity = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      ec2InstanceVpcId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      relatedVulnerabilities = Prelude.Nothing,
      findingType = Prelude.Nothing,
      componentType = Prelude.Nothing,
      vulnerabilityId = Prelude.Nothing,
      fixAvailable = Prelude.Nothing,
      findingArn = Prelude.Nothing,
      ec2InstanceSubnetId = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Details of the Amazon Web Services account IDs used to filter findings.
filterCriteria_awsAccountId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_awsAccountId = Lens.lens (\FilterCriteria' {awsAccountId} -> awsAccountId) (\s@FilterCriteria' {} a -> s {awsAccountId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance image IDs used to filter findings.
filterCriteria_ec2InstanceImageId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceImageId = Lens.lens (\FilterCriteria' {ec2InstanceImageId} -> ec2InstanceImageId) (\s@FilterCriteria' {} a -> s {ec2InstanceImageId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the ingress source addresses used to filter findings.
filterCriteria_networkProtocol :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_networkProtocol = Lens.lens (\FilterCriteria' {networkProtocol} -> networkProtocol) (\s@FilterCriteria' {} a -> s {networkProtocol = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource IDs used to filter findings.
filterCriteria_resourceId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_resourceId = Lens.lens (\FilterCriteria' {resourceId} -> resourceId) (\s@FilterCriteria' {} a -> s {resourceId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource types used to filter findings.
filterCriteria_resourceType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_resourceType = Lens.lens (\FilterCriteria' {resourceType} -> resourceType) (\s@FilterCriteria' {} a -> s {resourceType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the Amazon ECR registry used to filter findings.
filterCriteria_ecrImageRegistry :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageRegistry = Lens.lens (\FilterCriteria' {ecrImageRegistry} -> ecrImageRegistry) (\s@FilterCriteria' {} a -> s {ecrImageRegistry = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the severity used to filter findings.
filterCriteria_severity :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_severity = Lens.lens (\FilterCriteria' {severity} -> severity) (\s@FilterCriteria' {} a -> s {severity = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the port ranges used to filter findings.
filterCriteria_portRange :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty PortRangeFilter))
filterCriteria_portRange = Lens.lens (\FilterCriteria' {portRange} -> portRange) (\s@FilterCriteria' {} a -> s {portRange = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon ECR image architecture types used to filter
-- findings.
filterCriteria_ecrImageArchitecture :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageArchitecture = Lens.lens (\FilterCriteria' {ecrImageArchitecture} -> ecrImageArchitecture) (\s@FilterCriteria' {} a -> s {ecrImageArchitecture = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding status types used to filter findings.
filterCriteria_findingStatus :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingStatus = Lens.lens (\FilterCriteria' {findingStatus} -> findingStatus) (\s@FilterCriteria' {} a -> s {findingStatus = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerable packages used to filter findings.
filterCriteria_vulnerablePackages :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty PackageFilter))
filterCriteria_vulnerablePackages = Lens.lens (\FilterCriteria' {vulnerablePackages} -> vulnerablePackages) (\s@FilterCriteria' {} a -> s {vulnerablePackages = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerability type used to filter findings.
filterCriteria_vulnerabilitySource :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vulnerabilitySource = Lens.lens (\FilterCriteria' {vulnerabilitySource} -> vulnerabilitySource) (\s@FilterCriteria' {} a -> s {vulnerabilitySource = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the name of the Amazon ECR repository used to filter
-- findings.
filterCriteria_ecrImageRepositoryName :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageRepositoryName = Lens.lens (\FilterCriteria' {ecrImageRepositoryName} -> ecrImageRepositoryName) (\s@FilterCriteria' {} a -> s {ecrImageRepositoryName = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Inspector score to filter on.
filterCriteria_inspectorScore :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty NumberFilter))
filterCriteria_inspectorScore = Lens.lens (\FilterCriteria' {inspectorScore} -> inspectorScore) (\s@FilterCriteria' {} a -> s {inspectorScore = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The tags attached to the Amazon ECR container image.
filterCriteria_ecrImageTags :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageTags = Lens.lens (\FilterCriteria' {ecrImageTags} -> ecrImageTags) (\s@FilterCriteria' {} a -> s {ecrImageTags = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource tags used to filter findings.
filterCriteria_resourceTags :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty MapFilter))
filterCriteria_resourceTags = Lens.lens (\FilterCriteria' {resourceTags} -> resourceTags) (\s@FilterCriteria' {} a -> s {resourceTags = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon ECR image hashes used to filter findings.
filterCriteria_ecrImageHash :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageHash = Lens.lens (\FilterCriteria' {ecrImageHash} -> ecrImageHash) (\s@FilterCriteria' {} a -> s {ecrImageHash = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding title used to filter findings.
filterCriteria_title :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_title = Lens.lens (\FilterCriteria' {title} -> title) (\s@FilterCriteria' {} a -> s {title = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was first seen used to filter
-- findings.
filterCriteria_firstObservedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_firstObservedAt = Lens.lens (\FilterCriteria' {firstObservedAt} -> firstObservedAt) (\s@FilterCriteria' {} a -> s {firstObservedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the Amazon ECR image push date and time used to filter
-- findings.
filterCriteria_ecrImagePushedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_ecrImagePushedAt = Lens.lens (\FilterCriteria' {ecrImagePushedAt} -> ecrImagePushedAt) (\s@FilterCriteria' {} a -> s {ecrImagePushedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vendor severity used to filter findings.
filterCriteria_vendorSeverity :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vendorSeverity = Lens.lens (\FilterCriteria' {vendorSeverity} -> vendorSeverity) (\s@FilterCriteria' {} a -> s {vendorSeverity = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was last seen used to filter
-- findings.
filterCriteria_lastObservedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_lastObservedAt = Lens.lens (\FilterCriteria' {lastObservedAt} -> lastObservedAt) (\s@FilterCriteria' {} a -> s {lastObservedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance VPC IDs used to filter findings.
filterCriteria_ec2InstanceVpcId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceVpcId = Lens.lens (\FilterCriteria' {ec2InstanceVpcId} -> ec2InstanceVpcId) (\s@FilterCriteria' {} a -> s {ec2InstanceVpcId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the component IDs used to filter findings.
filterCriteria_componentId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_componentId = Lens.lens (\FilterCriteria' {componentId} -> componentId) (\s@FilterCriteria' {} a -> s {componentId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the related vulnerabilities used to filter findings.
filterCriteria_relatedVulnerabilities :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_relatedVulnerabilities = Lens.lens (\FilterCriteria' {relatedVulnerabilities} -> relatedVulnerabilities) (\s@FilterCriteria' {} a -> s {relatedVulnerabilities = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding types used to filter findings.
filterCriteria_findingType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingType = Lens.lens (\FilterCriteria' {findingType} -> findingType) (\s@FilterCriteria' {} a -> s {findingType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the component types used to filter findings.
filterCriteria_componentType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_componentType = Lens.lens (\FilterCriteria' {componentType} -> componentType) (\s@FilterCriteria' {} a -> s {componentType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerability ID used to filter findings.
filterCriteria_vulnerabilityId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vulnerabilityId = Lens.lens (\FilterCriteria' {vulnerabilityId} -> vulnerabilityId) (\s@FilterCriteria' {} a -> s {vulnerabilityId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
filterCriteria_fixAvailable :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_fixAvailable = Lens.lens (\FilterCriteria' {fixAvailable} -> fixAvailable) (\s@FilterCriteria' {} a -> s {fixAvailable = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding ARNs used to filter findings.
filterCriteria_findingArn :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingArn = Lens.lens (\FilterCriteria' {findingArn} -> findingArn) (\s@FilterCriteria' {} a -> s {findingArn = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance subnet IDs used to filter findings.
filterCriteria_ec2InstanceSubnetId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceSubnetId = Lens.lens (\FilterCriteria' {ec2InstanceSubnetId} -> ec2InstanceSubnetId) (\s@FilterCriteria' {} a -> s {ec2InstanceSubnetId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was last updated at used to
-- filter findings.
filterCriteria_updatedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_updatedAt = Lens.lens (\FilterCriteria' {updatedAt} -> updatedAt) (\s@FilterCriteria' {} a -> s {updatedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FilterCriteria where
  parseJSON =
    Core.withObject
      "FilterCriteria"
      ( \x ->
          FilterCriteria'
            Prelude.<$> (x Core..:? "awsAccountId")
            Prelude.<*> (x Core..:? "ec2InstanceImageId")
            Prelude.<*> (x Core..:? "networkProtocol")
            Prelude.<*> (x Core..:? "resourceId")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "ecrImageRegistry")
            Prelude.<*> (x Core..:? "severity")
            Prelude.<*> (x Core..:? "portRange")
            Prelude.<*> (x Core..:? "ecrImageArchitecture")
            Prelude.<*> (x Core..:? "findingStatus")
            Prelude.<*> (x Core..:? "vulnerablePackages")
            Prelude.<*> (x Core..:? "vulnerabilitySource")
            Prelude.<*> (x Core..:? "ecrImageRepositoryName")
            Prelude.<*> (x Core..:? "inspectorScore")
            Prelude.<*> (x Core..:? "ecrImageTags")
            Prelude.<*> (x Core..:? "resourceTags")
            Prelude.<*> (x Core..:? "ecrImageHash")
            Prelude.<*> (x Core..:? "title")
            Prelude.<*> (x Core..:? "firstObservedAt")
            Prelude.<*> (x Core..:? "ecrImagePushedAt")
            Prelude.<*> (x Core..:? "vendorSeverity")
            Prelude.<*> (x Core..:? "lastObservedAt")
            Prelude.<*> (x Core..:? "ec2InstanceVpcId")
            Prelude.<*> (x Core..:? "componentId")
            Prelude.<*> (x Core..:? "relatedVulnerabilities")
            Prelude.<*> (x Core..:? "findingType")
            Prelude.<*> (x Core..:? "componentType")
            Prelude.<*> (x Core..:? "vulnerabilityId")
            Prelude.<*> (x Core..:? "fixAvailable")
            Prelude.<*> (x Core..:? "findingArn")
            Prelude.<*> (x Core..:? "ec2InstanceSubnetId")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable FilterCriteria where
  hashWithSalt _salt FilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` ec2InstanceImageId
      `Prelude.hashWithSalt` networkProtocol
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` ecrImageRegistry
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` ecrImageArchitecture
      `Prelude.hashWithSalt` findingStatus
      `Prelude.hashWithSalt` vulnerablePackages
      `Prelude.hashWithSalt` vulnerabilitySource
      `Prelude.hashWithSalt` ecrImageRepositoryName
      `Prelude.hashWithSalt` inspectorScore
      `Prelude.hashWithSalt` ecrImageTags
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` ecrImageHash
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` ecrImagePushedAt
      `Prelude.hashWithSalt` vendorSeverity
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` ec2InstanceVpcId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` relatedVulnerabilities
      `Prelude.hashWithSalt` findingType
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` vulnerabilityId
      `Prelude.hashWithSalt` fixAvailable
      `Prelude.hashWithSalt` findingArn
      `Prelude.hashWithSalt` ec2InstanceSubnetId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData FilterCriteria where
  rnf FilterCriteria' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf ec2InstanceImageId
      `Prelude.seq` Prelude.rnf networkProtocol
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf ecrImageRegistry
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf portRange
      `Prelude.seq` Prelude.rnf ecrImageArchitecture
      `Prelude.seq` Prelude.rnf findingStatus
      `Prelude.seq` Prelude.rnf vulnerablePackages
      `Prelude.seq` Prelude.rnf vulnerabilitySource
      `Prelude.seq` Prelude.rnf ecrImageRepositoryName
      `Prelude.seq` Prelude.rnf inspectorScore
      `Prelude.seq` Prelude.rnf ecrImageTags
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf ecrImageHash
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf ecrImagePushedAt
      `Prelude.seq` Prelude.rnf vendorSeverity
      `Prelude.seq` Prelude.rnf lastObservedAt
      `Prelude.seq` Prelude.rnf
        ec2InstanceVpcId
      `Prelude.seq` Prelude.rnf
        componentId
      `Prelude.seq` Prelude.rnf
        relatedVulnerabilities
      `Prelude.seq` Prelude.rnf
        findingType
      `Prelude.seq` Prelude.rnf
        componentType
      `Prelude.seq` Prelude.rnf
        vulnerabilityId
      `Prelude.seq` Prelude.rnf
        fixAvailable
      `Prelude.seq` Prelude.rnf
        findingArn
      `Prelude.seq` Prelude.rnf
        ec2InstanceSubnetId
      `Prelude.seq` Prelude.rnf
        updatedAt

instance Core.ToJSON FilterCriteria where
  toJSON FilterCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Core..=) Prelude.<$> awsAccountId,
            ("ec2InstanceImageId" Core..=)
              Prelude.<$> ec2InstanceImageId,
            ("networkProtocol" Core..=)
              Prelude.<$> networkProtocol,
            ("resourceId" Core..=) Prelude.<$> resourceId,
            ("resourceType" Core..=) Prelude.<$> resourceType,
            ("ecrImageRegistry" Core..=)
              Prelude.<$> ecrImageRegistry,
            ("severity" Core..=) Prelude.<$> severity,
            ("portRange" Core..=) Prelude.<$> portRange,
            ("ecrImageArchitecture" Core..=)
              Prelude.<$> ecrImageArchitecture,
            ("findingStatus" Core..=) Prelude.<$> findingStatus,
            ("vulnerablePackages" Core..=)
              Prelude.<$> vulnerablePackages,
            ("vulnerabilitySource" Core..=)
              Prelude.<$> vulnerabilitySource,
            ("ecrImageRepositoryName" Core..=)
              Prelude.<$> ecrImageRepositoryName,
            ("inspectorScore" Core..=)
              Prelude.<$> inspectorScore,
            ("ecrImageTags" Core..=) Prelude.<$> ecrImageTags,
            ("resourceTags" Core..=) Prelude.<$> resourceTags,
            ("ecrImageHash" Core..=) Prelude.<$> ecrImageHash,
            ("title" Core..=) Prelude.<$> title,
            ("firstObservedAt" Core..=)
              Prelude.<$> firstObservedAt,
            ("ecrImagePushedAt" Core..=)
              Prelude.<$> ecrImagePushedAt,
            ("vendorSeverity" Core..=)
              Prelude.<$> vendorSeverity,
            ("lastObservedAt" Core..=)
              Prelude.<$> lastObservedAt,
            ("ec2InstanceVpcId" Core..=)
              Prelude.<$> ec2InstanceVpcId,
            ("componentId" Core..=) Prelude.<$> componentId,
            ("relatedVulnerabilities" Core..=)
              Prelude.<$> relatedVulnerabilities,
            ("findingType" Core..=) Prelude.<$> findingType,
            ("componentType" Core..=) Prelude.<$> componentType,
            ("vulnerabilityId" Core..=)
              Prelude.<$> vulnerabilityId,
            ("fixAvailable" Core..=) Prelude.<$> fixAvailable,
            ("findingArn" Core..=) Prelude.<$> findingArn,
            ("ec2InstanceSubnetId" Core..=)
              Prelude.<$> ec2InstanceSubnetId,
            ("updatedAt" Core..=) Prelude.<$> updatedAt
          ]
      )
