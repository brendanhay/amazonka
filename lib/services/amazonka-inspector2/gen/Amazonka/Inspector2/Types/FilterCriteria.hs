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
import qualified Amazonka.Data as Data
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
    -- | Details of the component IDs used to filter findings.
    componentId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the component types used to filter findings.
    componentType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon EC2 instance image IDs used to filter findings.
    ec2InstanceImageId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon EC2 instance subnet IDs used to filter findings.
    ec2InstanceSubnetId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon EC2 instance VPC IDs used to filter findings.
    ec2InstanceVpcId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon ECR image architecture types used to filter
    -- findings.
    ecrImageArchitecture :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the Amazon ECR image hashes used to filter findings.
    ecrImageHash :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the Amazon ECR image push date and time used to filter
    -- findings.
    ecrImagePushedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on the Amazon ECR registry used to filter findings.
    ecrImageRegistry :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the name of the Amazon ECR repository used to filter
    -- findings.
    ecrImageRepositoryName :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The tags attached to the Amazon ECR container image.
    ecrImageTags :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Filters the list of AWS Lambda findings by the availability of exploits.
    exploitAvailable :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding ARNs used to filter findings.
    findingArn :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding status types used to filter findings.
    findingStatus :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding types used to filter findings.
    findingType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was first seen used to filter
    -- findings.
    firstObservedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on whether a fix is available through a version update. This
    -- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
    -- but not all, of the packages identified in the finding have fixes
    -- available through updated versions.
    fixAvailable :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The Amazon Inspector score to filter on.
    inspectorScore :: Prelude.Maybe (Prelude.NonEmpty NumberFilter),
    -- | Filters the list of AWS Lambda functions by execution role.
    lambdaFunctionExecutionRoleArn :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Filters the list of AWS Lambda functions by the date and time that a
    -- user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
    lambdaFunctionLastModifiedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Filters the list of AWS Lambda functions by the function\'s
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
    -- A Lambda function can have up to five layers.
    lambdaFunctionLayers :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Filters the list of AWS Lambda functions by the name of the function.
    lambdaFunctionName :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Filters the list of AWS Lambda functions by the runtime environment for
    -- the Lambda function.
    lambdaFunctionRuntime :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was last seen used to filter
    -- findings.
    lastObservedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on the ingress source addresses used to filter findings.
    networkProtocol :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the port ranges used to filter findings.
    portRange :: Prelude.Maybe (Prelude.NonEmpty PortRangeFilter),
    -- | Details on the related vulnerabilities used to filter findings.
    relatedVulnerabilities :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the resource IDs used to filter findings.
    resourceId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the resource tags used to filter findings.
    resourceTags :: Prelude.Maybe (Prelude.NonEmpty MapFilter),
    -- | Details on the resource types used to filter findings.
    resourceType :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the severity used to filter findings.
    severity :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the finding title used to filter findings.
    title :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the date and time a finding was last updated at used to
    -- filter findings.
    updatedAt :: Prelude.Maybe (Prelude.NonEmpty DateFilter),
    -- | Details on the vendor severity used to filter findings.
    vendorSeverity :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the vulnerability ID used to filter findings.
    vulnerabilityId :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the vulnerability type used to filter findings.
    vulnerabilitySource :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details on the vulnerable packages used to filter findings.
    vulnerablePackages :: Prelude.Maybe (Prelude.NonEmpty PackageFilter)
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
-- 'componentId', 'filterCriteria_componentId' - Details of the component IDs used to filter findings.
--
-- 'componentType', 'filterCriteria_componentType' - Details of the component types used to filter findings.
--
-- 'ec2InstanceImageId', 'filterCriteria_ec2InstanceImageId' - Details of the Amazon EC2 instance image IDs used to filter findings.
--
-- 'ec2InstanceSubnetId', 'filterCriteria_ec2InstanceSubnetId' - Details of the Amazon EC2 instance subnet IDs used to filter findings.
--
-- 'ec2InstanceVpcId', 'filterCriteria_ec2InstanceVpcId' - Details of the Amazon EC2 instance VPC IDs used to filter findings.
--
-- 'ecrImageArchitecture', 'filterCriteria_ecrImageArchitecture' - Details of the Amazon ECR image architecture types used to filter
-- findings.
--
-- 'ecrImageHash', 'filterCriteria_ecrImageHash' - Details of the Amazon ECR image hashes used to filter findings.
--
-- 'ecrImagePushedAt', 'filterCriteria_ecrImagePushedAt' - Details on the Amazon ECR image push date and time used to filter
-- findings.
--
-- 'ecrImageRegistry', 'filterCriteria_ecrImageRegistry' - Details on the Amazon ECR registry used to filter findings.
--
-- 'ecrImageRepositoryName', 'filterCriteria_ecrImageRepositoryName' - Details on the name of the Amazon ECR repository used to filter
-- findings.
--
-- 'ecrImageTags', 'filterCriteria_ecrImageTags' - The tags attached to the Amazon ECR container image.
--
-- 'exploitAvailable', 'filterCriteria_exploitAvailable' - Filters the list of AWS Lambda findings by the availability of exploits.
--
-- 'findingArn', 'filterCriteria_findingArn' - Details on the finding ARNs used to filter findings.
--
-- 'findingStatus', 'filterCriteria_findingStatus' - Details on the finding status types used to filter findings.
--
-- 'findingType', 'filterCriteria_findingType' - Details on the finding types used to filter findings.
--
-- 'firstObservedAt', 'filterCriteria_firstObservedAt' - Details on the date and time a finding was first seen used to filter
-- findings.
--
-- 'fixAvailable', 'filterCriteria_fixAvailable' - Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
--
-- 'inspectorScore', 'filterCriteria_inspectorScore' - The Amazon Inspector score to filter on.
--
-- 'lambdaFunctionExecutionRoleArn', 'filterCriteria_lambdaFunctionExecutionRoleArn' - Filters the list of AWS Lambda functions by execution role.
--
-- 'lambdaFunctionLastModifiedAt', 'filterCriteria_lambdaFunctionLastModifiedAt' - Filters the list of AWS Lambda functions by the date and time that a
-- user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
--
-- 'lambdaFunctionLayers', 'filterCriteria_lambdaFunctionLayers' - Filters the list of AWS Lambda functions by the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
-- A Lambda function can have up to five layers.
--
-- 'lambdaFunctionName', 'filterCriteria_lambdaFunctionName' - Filters the list of AWS Lambda functions by the name of the function.
--
-- 'lambdaFunctionRuntime', 'filterCriteria_lambdaFunctionRuntime' - Filters the list of AWS Lambda functions by the runtime environment for
-- the Lambda function.
--
-- 'lastObservedAt', 'filterCriteria_lastObservedAt' - Details on the date and time a finding was last seen used to filter
-- findings.
--
-- 'networkProtocol', 'filterCriteria_networkProtocol' - Details on the ingress source addresses used to filter findings.
--
-- 'portRange', 'filterCriteria_portRange' - Details on the port ranges used to filter findings.
--
-- 'relatedVulnerabilities', 'filterCriteria_relatedVulnerabilities' - Details on the related vulnerabilities used to filter findings.
--
-- 'resourceId', 'filterCriteria_resourceId' - Details on the resource IDs used to filter findings.
--
-- 'resourceTags', 'filterCriteria_resourceTags' - Details on the resource tags used to filter findings.
--
-- 'resourceType', 'filterCriteria_resourceType' - Details on the resource types used to filter findings.
--
-- 'severity', 'filterCriteria_severity' - Details on the severity used to filter findings.
--
-- 'title', 'filterCriteria_title' - Details on the finding title used to filter findings.
--
-- 'updatedAt', 'filterCriteria_updatedAt' - Details on the date and time a finding was last updated at used to
-- filter findings.
--
-- 'vendorSeverity', 'filterCriteria_vendorSeverity' - Details on the vendor severity used to filter findings.
--
-- 'vulnerabilityId', 'filterCriteria_vulnerabilityId' - Details on the vulnerability ID used to filter findings.
--
-- 'vulnerabilitySource', 'filterCriteria_vulnerabilitySource' - Details on the vulnerability type used to filter findings.
--
-- 'vulnerablePackages', 'filterCriteria_vulnerablePackages' - Details on the vulnerable packages used to filter findings.
newFilterCriteria ::
  FilterCriteria
newFilterCriteria =
  FilterCriteria'
    { awsAccountId = Prelude.Nothing,
      componentId = Prelude.Nothing,
      componentType = Prelude.Nothing,
      ec2InstanceImageId = Prelude.Nothing,
      ec2InstanceSubnetId = Prelude.Nothing,
      ec2InstanceVpcId = Prelude.Nothing,
      ecrImageArchitecture = Prelude.Nothing,
      ecrImageHash = Prelude.Nothing,
      ecrImagePushedAt = Prelude.Nothing,
      ecrImageRegistry = Prelude.Nothing,
      ecrImageRepositoryName = Prelude.Nothing,
      ecrImageTags = Prelude.Nothing,
      exploitAvailable = Prelude.Nothing,
      findingArn = Prelude.Nothing,
      findingStatus = Prelude.Nothing,
      findingType = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      fixAvailable = Prelude.Nothing,
      inspectorScore = Prelude.Nothing,
      lambdaFunctionExecutionRoleArn = Prelude.Nothing,
      lambdaFunctionLastModifiedAt = Prelude.Nothing,
      lambdaFunctionLayers = Prelude.Nothing,
      lambdaFunctionName = Prelude.Nothing,
      lambdaFunctionRuntime = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      networkProtocol = Prelude.Nothing,
      portRange = Prelude.Nothing,
      relatedVulnerabilities = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      severity = Prelude.Nothing,
      title = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      vendorSeverity = Prelude.Nothing,
      vulnerabilityId = Prelude.Nothing,
      vulnerabilitySource = Prelude.Nothing,
      vulnerablePackages = Prelude.Nothing
    }

-- | Details of the Amazon Web Services account IDs used to filter findings.
filterCriteria_awsAccountId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_awsAccountId = Lens.lens (\FilterCriteria' {awsAccountId} -> awsAccountId) (\s@FilterCriteria' {} a -> s {awsAccountId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the component IDs used to filter findings.
filterCriteria_componentId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_componentId = Lens.lens (\FilterCriteria' {componentId} -> componentId) (\s@FilterCriteria' {} a -> s {componentId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the component types used to filter findings.
filterCriteria_componentType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_componentType = Lens.lens (\FilterCriteria' {componentType} -> componentType) (\s@FilterCriteria' {} a -> s {componentType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance image IDs used to filter findings.
filterCriteria_ec2InstanceImageId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceImageId = Lens.lens (\FilterCriteria' {ec2InstanceImageId} -> ec2InstanceImageId) (\s@FilterCriteria' {} a -> s {ec2InstanceImageId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance subnet IDs used to filter findings.
filterCriteria_ec2InstanceSubnetId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceSubnetId = Lens.lens (\FilterCriteria' {ec2InstanceSubnetId} -> ec2InstanceSubnetId) (\s@FilterCriteria' {} a -> s {ec2InstanceSubnetId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon EC2 instance VPC IDs used to filter findings.
filterCriteria_ec2InstanceVpcId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ec2InstanceVpcId = Lens.lens (\FilterCriteria' {ec2InstanceVpcId} -> ec2InstanceVpcId) (\s@FilterCriteria' {} a -> s {ec2InstanceVpcId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon ECR image architecture types used to filter
-- findings.
filterCriteria_ecrImageArchitecture :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageArchitecture = Lens.lens (\FilterCriteria' {ecrImageArchitecture} -> ecrImageArchitecture) (\s@FilterCriteria' {} a -> s {ecrImageArchitecture = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details of the Amazon ECR image hashes used to filter findings.
filterCriteria_ecrImageHash :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageHash = Lens.lens (\FilterCriteria' {ecrImageHash} -> ecrImageHash) (\s@FilterCriteria' {} a -> s {ecrImageHash = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the Amazon ECR image push date and time used to filter
-- findings.
filterCriteria_ecrImagePushedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_ecrImagePushedAt = Lens.lens (\FilterCriteria' {ecrImagePushedAt} -> ecrImagePushedAt) (\s@FilterCriteria' {} a -> s {ecrImagePushedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the Amazon ECR registry used to filter findings.
filterCriteria_ecrImageRegistry :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageRegistry = Lens.lens (\FilterCriteria' {ecrImageRegistry} -> ecrImageRegistry) (\s@FilterCriteria' {} a -> s {ecrImageRegistry = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the name of the Amazon ECR repository used to filter
-- findings.
filterCriteria_ecrImageRepositoryName :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageRepositoryName = Lens.lens (\FilterCriteria' {ecrImageRepositoryName} -> ecrImageRepositoryName) (\s@FilterCriteria' {} a -> s {ecrImageRepositoryName = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The tags attached to the Amazon ECR container image.
filterCriteria_ecrImageTags :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_ecrImageTags = Lens.lens (\FilterCriteria' {ecrImageTags} -> ecrImageTags) (\s@FilterCriteria' {} a -> s {ecrImageTags = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda findings by the availability of exploits.
filterCriteria_exploitAvailable :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_exploitAvailable = Lens.lens (\FilterCriteria' {exploitAvailable} -> exploitAvailable) (\s@FilterCriteria' {} a -> s {exploitAvailable = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding ARNs used to filter findings.
filterCriteria_findingArn :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingArn = Lens.lens (\FilterCriteria' {findingArn} -> findingArn) (\s@FilterCriteria' {} a -> s {findingArn = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding status types used to filter findings.
filterCriteria_findingStatus :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingStatus = Lens.lens (\FilterCriteria' {findingStatus} -> findingStatus) (\s@FilterCriteria' {} a -> s {findingStatus = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding types used to filter findings.
filterCriteria_findingType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_findingType = Lens.lens (\FilterCriteria' {findingType} -> findingType) (\s@FilterCriteria' {} a -> s {findingType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was first seen used to filter
-- findings.
filterCriteria_firstObservedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_firstObservedAt = Lens.lens (\FilterCriteria' {firstObservedAt} -> firstObservedAt) (\s@FilterCriteria' {} a -> s {firstObservedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on whether a fix is available through a version update. This
-- value can be @YES@, @NO@, or @PARTIAL@. A @PARTIAL@ fix means that some,
-- but not all, of the packages identified in the finding have fixes
-- available through updated versions.
filterCriteria_fixAvailable :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_fixAvailable = Lens.lens (\FilterCriteria' {fixAvailable} -> fixAvailable) (\s@FilterCriteria' {} a -> s {fixAvailable = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Inspector score to filter on.
filterCriteria_inspectorScore :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty NumberFilter))
filterCriteria_inspectorScore = Lens.lens (\FilterCriteria' {inspectorScore} -> inspectorScore) (\s@FilterCriteria' {} a -> s {inspectorScore = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda functions by execution role.
filterCriteria_lambdaFunctionExecutionRoleArn :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_lambdaFunctionExecutionRoleArn = Lens.lens (\FilterCriteria' {lambdaFunctionExecutionRoleArn} -> lambdaFunctionExecutionRoleArn) (\s@FilterCriteria' {} a -> s {lambdaFunctionExecutionRoleArn = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda functions by the date and time that a
-- user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>
filterCriteria_lambdaFunctionLastModifiedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_lambdaFunctionLastModifiedAt = Lens.lens (\FilterCriteria' {lambdaFunctionLastModifiedAt} -> lambdaFunctionLastModifiedAt) (\s@FilterCriteria' {} a -> s {lambdaFunctionLastModifiedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda functions by the function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html layers>.
-- A Lambda function can have up to five layers.
filterCriteria_lambdaFunctionLayers :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_lambdaFunctionLayers = Lens.lens (\FilterCriteria' {lambdaFunctionLayers} -> lambdaFunctionLayers) (\s@FilterCriteria' {} a -> s {lambdaFunctionLayers = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda functions by the name of the function.
filterCriteria_lambdaFunctionName :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_lambdaFunctionName = Lens.lens (\FilterCriteria' {lambdaFunctionName} -> lambdaFunctionName) (\s@FilterCriteria' {} a -> s {lambdaFunctionName = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Filters the list of AWS Lambda functions by the runtime environment for
-- the Lambda function.
filterCriteria_lambdaFunctionRuntime :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_lambdaFunctionRuntime = Lens.lens (\FilterCriteria' {lambdaFunctionRuntime} -> lambdaFunctionRuntime) (\s@FilterCriteria' {} a -> s {lambdaFunctionRuntime = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was last seen used to filter
-- findings.
filterCriteria_lastObservedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_lastObservedAt = Lens.lens (\FilterCriteria' {lastObservedAt} -> lastObservedAt) (\s@FilterCriteria' {} a -> s {lastObservedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the ingress source addresses used to filter findings.
filterCriteria_networkProtocol :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_networkProtocol = Lens.lens (\FilterCriteria' {networkProtocol} -> networkProtocol) (\s@FilterCriteria' {} a -> s {networkProtocol = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the port ranges used to filter findings.
filterCriteria_portRange :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty PortRangeFilter))
filterCriteria_portRange = Lens.lens (\FilterCriteria' {portRange} -> portRange) (\s@FilterCriteria' {} a -> s {portRange = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the related vulnerabilities used to filter findings.
filterCriteria_relatedVulnerabilities :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_relatedVulnerabilities = Lens.lens (\FilterCriteria' {relatedVulnerabilities} -> relatedVulnerabilities) (\s@FilterCriteria' {} a -> s {relatedVulnerabilities = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource IDs used to filter findings.
filterCriteria_resourceId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_resourceId = Lens.lens (\FilterCriteria' {resourceId} -> resourceId) (\s@FilterCriteria' {} a -> s {resourceId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource tags used to filter findings.
filterCriteria_resourceTags :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty MapFilter))
filterCriteria_resourceTags = Lens.lens (\FilterCriteria' {resourceTags} -> resourceTags) (\s@FilterCriteria' {} a -> s {resourceTags = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the resource types used to filter findings.
filterCriteria_resourceType :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_resourceType = Lens.lens (\FilterCriteria' {resourceType} -> resourceType) (\s@FilterCriteria' {} a -> s {resourceType = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the severity used to filter findings.
filterCriteria_severity :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_severity = Lens.lens (\FilterCriteria' {severity} -> severity) (\s@FilterCriteria' {} a -> s {severity = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the finding title used to filter findings.
filterCriteria_title :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_title = Lens.lens (\FilterCriteria' {title} -> title) (\s@FilterCriteria' {} a -> s {title = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the date and time a finding was last updated at used to
-- filter findings.
filterCriteria_updatedAt :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty DateFilter))
filterCriteria_updatedAt = Lens.lens (\FilterCriteria' {updatedAt} -> updatedAt) (\s@FilterCriteria' {} a -> s {updatedAt = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vendor severity used to filter findings.
filterCriteria_vendorSeverity :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vendorSeverity = Lens.lens (\FilterCriteria' {vendorSeverity} -> vendorSeverity) (\s@FilterCriteria' {} a -> s {vendorSeverity = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerability ID used to filter findings.
filterCriteria_vulnerabilityId :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vulnerabilityId = Lens.lens (\FilterCriteria' {vulnerabilityId} -> vulnerabilityId) (\s@FilterCriteria' {} a -> s {vulnerabilityId = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerability type used to filter findings.
filterCriteria_vulnerabilitySource :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty StringFilter))
filterCriteria_vulnerabilitySource = Lens.lens (\FilterCriteria' {vulnerabilitySource} -> vulnerabilitySource) (\s@FilterCriteria' {} a -> s {vulnerabilitySource = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Details on the vulnerable packages used to filter findings.
filterCriteria_vulnerablePackages :: Lens.Lens' FilterCriteria (Prelude.Maybe (Prelude.NonEmpty PackageFilter))
filterCriteria_vulnerablePackages = Lens.lens (\FilterCriteria' {vulnerablePackages} -> vulnerablePackages) (\s@FilterCriteria' {} a -> s {vulnerablePackages = a} :: FilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilterCriteria where
  parseJSON =
    Data.withObject
      "FilterCriteria"
      ( \x ->
          FilterCriteria'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "componentId")
            Prelude.<*> (x Data..:? "componentType")
            Prelude.<*> (x Data..:? "ec2InstanceImageId")
            Prelude.<*> (x Data..:? "ec2InstanceSubnetId")
            Prelude.<*> (x Data..:? "ec2InstanceVpcId")
            Prelude.<*> (x Data..:? "ecrImageArchitecture")
            Prelude.<*> (x Data..:? "ecrImageHash")
            Prelude.<*> (x Data..:? "ecrImagePushedAt")
            Prelude.<*> (x Data..:? "ecrImageRegistry")
            Prelude.<*> (x Data..:? "ecrImageRepositoryName")
            Prelude.<*> (x Data..:? "ecrImageTags")
            Prelude.<*> (x Data..:? "exploitAvailable")
            Prelude.<*> (x Data..:? "findingArn")
            Prelude.<*> (x Data..:? "findingStatus")
            Prelude.<*> (x Data..:? "findingType")
            Prelude.<*> (x Data..:? "firstObservedAt")
            Prelude.<*> (x Data..:? "fixAvailable")
            Prelude.<*> (x Data..:? "inspectorScore")
            Prelude.<*> (x Data..:? "lambdaFunctionExecutionRoleArn")
            Prelude.<*> (x Data..:? "lambdaFunctionLastModifiedAt")
            Prelude.<*> (x Data..:? "lambdaFunctionLayers")
            Prelude.<*> (x Data..:? "lambdaFunctionName")
            Prelude.<*> (x Data..:? "lambdaFunctionRuntime")
            Prelude.<*> (x Data..:? "lastObservedAt")
            Prelude.<*> (x Data..:? "networkProtocol")
            Prelude.<*> (x Data..:? "portRange")
            Prelude.<*> (x Data..:? "relatedVulnerabilities")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceTags")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "severity")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..:? "vendorSeverity")
            Prelude.<*> (x Data..:? "vulnerabilityId")
            Prelude.<*> (x Data..:? "vulnerabilitySource")
            Prelude.<*> (x Data..:? "vulnerablePackages")
      )

instance Prelude.Hashable FilterCriteria where
  hashWithSalt _salt FilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` ec2InstanceImageId
      `Prelude.hashWithSalt` ec2InstanceSubnetId
      `Prelude.hashWithSalt` ec2InstanceVpcId
      `Prelude.hashWithSalt` ecrImageArchitecture
      `Prelude.hashWithSalt` ecrImageHash
      `Prelude.hashWithSalt` ecrImagePushedAt
      `Prelude.hashWithSalt` ecrImageRegistry
      `Prelude.hashWithSalt` ecrImageRepositoryName
      `Prelude.hashWithSalt` ecrImageTags
      `Prelude.hashWithSalt` exploitAvailable
      `Prelude.hashWithSalt` findingArn
      `Prelude.hashWithSalt` findingStatus
      `Prelude.hashWithSalt` findingType
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` fixAvailable
      `Prelude.hashWithSalt` inspectorScore
      `Prelude.hashWithSalt` lambdaFunctionExecutionRoleArn
      `Prelude.hashWithSalt` lambdaFunctionLastModifiedAt
      `Prelude.hashWithSalt` lambdaFunctionLayers
      `Prelude.hashWithSalt` lambdaFunctionName
      `Prelude.hashWithSalt` lambdaFunctionRuntime
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` networkProtocol
      `Prelude.hashWithSalt` portRange
      `Prelude.hashWithSalt` relatedVulnerabilities
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` vendorSeverity
      `Prelude.hashWithSalt` vulnerabilityId
      `Prelude.hashWithSalt` vulnerabilitySource
      `Prelude.hashWithSalt` vulnerablePackages

instance Prelude.NFData FilterCriteria where
  rnf FilterCriteria' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf ec2InstanceImageId
      `Prelude.seq` Prelude.rnf ec2InstanceSubnetId
      `Prelude.seq` Prelude.rnf ec2InstanceVpcId
      `Prelude.seq` Prelude.rnf ecrImageArchitecture
      `Prelude.seq` Prelude.rnf ecrImageHash
      `Prelude.seq` Prelude.rnf ecrImagePushedAt
      `Prelude.seq` Prelude.rnf ecrImageRegistry
      `Prelude.seq` Prelude.rnf ecrImageRepositoryName
      `Prelude.seq` Prelude.rnf ecrImageTags
      `Prelude.seq` Prelude.rnf exploitAvailable
      `Prelude.seq` Prelude.rnf findingArn
      `Prelude.seq` Prelude.rnf findingStatus
      `Prelude.seq` Prelude.rnf findingType
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf fixAvailable
      `Prelude.seq` Prelude.rnf inspectorScore
      `Prelude.seq` Prelude.rnf
        lambdaFunctionExecutionRoleArn
      `Prelude.seq` Prelude.rnf
        lambdaFunctionLastModifiedAt
      `Prelude.seq` Prelude.rnf
        lambdaFunctionLayers
      `Prelude.seq` Prelude.rnf
        lambdaFunctionName
      `Prelude.seq` Prelude.rnf
        lambdaFunctionRuntime
      `Prelude.seq` Prelude.rnf
        lastObservedAt
      `Prelude.seq` Prelude.rnf
        networkProtocol
      `Prelude.seq` Prelude.rnf
        portRange
      `Prelude.seq` Prelude.rnf
        relatedVulnerabilities
      `Prelude.seq` Prelude.rnf
        resourceId
      `Prelude.seq` Prelude.rnf
        resourceTags
      `Prelude.seq` Prelude.rnf
        resourceType
      `Prelude.seq` Prelude.rnf
        severity
      `Prelude.seq` Prelude.rnf
        title
      `Prelude.seq` Prelude.rnf
        updatedAt
      `Prelude.seq` Prelude.rnf
        vendorSeverity
      `Prelude.seq` Prelude.rnf
        vulnerabilityId
      `Prelude.seq` Prelude.rnf
        vulnerabilitySource
      `Prelude.seq` Prelude.rnf
        vulnerablePackages

instance Data.ToJSON FilterCriteria where
  toJSON FilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("componentId" Data..=) Prelude.<$> componentId,
            ("componentType" Data..=) Prelude.<$> componentType,
            ("ec2InstanceImageId" Data..=)
              Prelude.<$> ec2InstanceImageId,
            ("ec2InstanceSubnetId" Data..=)
              Prelude.<$> ec2InstanceSubnetId,
            ("ec2InstanceVpcId" Data..=)
              Prelude.<$> ec2InstanceVpcId,
            ("ecrImageArchitecture" Data..=)
              Prelude.<$> ecrImageArchitecture,
            ("ecrImageHash" Data..=) Prelude.<$> ecrImageHash,
            ("ecrImagePushedAt" Data..=)
              Prelude.<$> ecrImagePushedAt,
            ("ecrImageRegistry" Data..=)
              Prelude.<$> ecrImageRegistry,
            ("ecrImageRepositoryName" Data..=)
              Prelude.<$> ecrImageRepositoryName,
            ("ecrImageTags" Data..=) Prelude.<$> ecrImageTags,
            ("exploitAvailable" Data..=)
              Prelude.<$> exploitAvailable,
            ("findingArn" Data..=) Prelude.<$> findingArn,
            ("findingStatus" Data..=) Prelude.<$> findingStatus,
            ("findingType" Data..=) Prelude.<$> findingType,
            ("firstObservedAt" Data..=)
              Prelude.<$> firstObservedAt,
            ("fixAvailable" Data..=) Prelude.<$> fixAvailable,
            ("inspectorScore" Data..=)
              Prelude.<$> inspectorScore,
            ("lambdaFunctionExecutionRoleArn" Data..=)
              Prelude.<$> lambdaFunctionExecutionRoleArn,
            ("lambdaFunctionLastModifiedAt" Data..=)
              Prelude.<$> lambdaFunctionLastModifiedAt,
            ("lambdaFunctionLayers" Data..=)
              Prelude.<$> lambdaFunctionLayers,
            ("lambdaFunctionName" Data..=)
              Prelude.<$> lambdaFunctionName,
            ("lambdaFunctionRuntime" Data..=)
              Prelude.<$> lambdaFunctionRuntime,
            ("lastObservedAt" Data..=)
              Prelude.<$> lastObservedAt,
            ("networkProtocol" Data..=)
              Prelude.<$> networkProtocol,
            ("portRange" Data..=) Prelude.<$> portRange,
            ("relatedVulnerabilities" Data..=)
              Prelude.<$> relatedVulnerabilities,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("resourceTags" Data..=) Prelude.<$> resourceTags,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            ("severity" Data..=) Prelude.<$> severity,
            ("title" Data..=) Prelude.<$> title,
            ("updatedAt" Data..=) Prelude.<$> updatedAt,
            ("vendorSeverity" Data..=)
              Prelude.<$> vendorSeverity,
            ("vulnerabilityId" Data..=)
              Prelude.<$> vulnerabilityId,
            ("vulnerabilitySource" Data..=)
              Prelude.<$> vulnerabilitySource,
            ("vulnerablePackages" Data..=)
              Prelude.<$> vulnerablePackages
          ]
      )
