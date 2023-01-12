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
-- Module      : Amazonka.Inspector2.Types.CoverageFilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CoverageFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CoverageMapFilter
import Amazonka.Inspector2.Types.CoverageStringFilter
import qualified Amazonka.Prelude as Prelude

-- | A structure that identifies filter criteria for @GetCoverageStatistics@.
--
-- /See:/ 'newCoverageFilterCriteria' smart constructor.
data CoverageFilterCriteria = CoverageFilterCriteria'
  { -- | An array of Amazon Web Services account IDs to return coverage
    -- statistics for.
    accountId :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | The Amazon EC2 instance tags to filter on.
    ec2InstanceTags :: Prelude.Maybe (Prelude.NonEmpty CoverageMapFilter),
    -- | The Amazon ECR image tags to filter on.
    ecrImageTags :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | The Amazon ECR repository name to filter on.
    ecrRepositoryName :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | Returns coverage statistics for AWS Lambda functions filtered by
    -- function names.
    lambdaFunctionName :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | Returns coverage statistics for AWS Lambda functions filtered by
    -- runtime.
    lambdaFunctionRuntime :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | Returns coverage statistics for AWS Lambda functions filtered by tag.
    lambdaFunctionTags :: Prelude.Maybe (Prelude.NonEmpty CoverageMapFilter),
    -- | An array of Amazon Web Services resource IDs to return coverage
    -- statistics for.
    resourceId :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | An array of Amazon Web Services resource types to return coverage
    -- statistics for. The values can be @AWS_EC2_INSTANCE@ or
    -- @AWS_ECR_REPOSITORY@.
    resourceType :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | The scan status code to filter on.
    scanStatusCode :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | The scan status reason to filter on.
    scanStatusReason :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter),
    -- | An array of Amazon Inspector scan types to return coverage statistics
    -- for.
    scanType :: Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'coverageFilterCriteria_accountId' - An array of Amazon Web Services account IDs to return coverage
-- statistics for.
--
-- 'ec2InstanceTags', 'coverageFilterCriteria_ec2InstanceTags' - The Amazon EC2 instance tags to filter on.
--
-- 'ecrImageTags', 'coverageFilterCriteria_ecrImageTags' - The Amazon ECR image tags to filter on.
--
-- 'ecrRepositoryName', 'coverageFilterCriteria_ecrRepositoryName' - The Amazon ECR repository name to filter on.
--
-- 'lambdaFunctionName', 'coverageFilterCriteria_lambdaFunctionName' - Returns coverage statistics for AWS Lambda functions filtered by
-- function names.
--
-- 'lambdaFunctionRuntime', 'coverageFilterCriteria_lambdaFunctionRuntime' - Returns coverage statistics for AWS Lambda functions filtered by
-- runtime.
--
-- 'lambdaFunctionTags', 'coverageFilterCriteria_lambdaFunctionTags' - Returns coverage statistics for AWS Lambda functions filtered by tag.
--
-- 'resourceId', 'coverageFilterCriteria_resourceId' - An array of Amazon Web Services resource IDs to return coverage
-- statistics for.
--
-- 'resourceType', 'coverageFilterCriteria_resourceType' - An array of Amazon Web Services resource types to return coverage
-- statistics for. The values can be @AWS_EC2_INSTANCE@ or
-- @AWS_ECR_REPOSITORY@.
--
-- 'scanStatusCode', 'coverageFilterCriteria_scanStatusCode' - The scan status code to filter on.
--
-- 'scanStatusReason', 'coverageFilterCriteria_scanStatusReason' - The scan status reason to filter on.
--
-- 'scanType', 'coverageFilterCriteria_scanType' - An array of Amazon Inspector scan types to return coverage statistics
-- for.
newCoverageFilterCriteria ::
  CoverageFilterCriteria
newCoverageFilterCriteria =
  CoverageFilterCriteria'
    { accountId =
        Prelude.Nothing,
      ec2InstanceTags = Prelude.Nothing,
      ecrImageTags = Prelude.Nothing,
      ecrRepositoryName = Prelude.Nothing,
      lambdaFunctionName = Prelude.Nothing,
      lambdaFunctionRuntime = Prelude.Nothing,
      lambdaFunctionTags = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      scanStatusCode = Prelude.Nothing,
      scanStatusReason = Prelude.Nothing,
      scanType = Prelude.Nothing
    }

-- | An array of Amazon Web Services account IDs to return coverage
-- statistics for.
coverageFilterCriteria_accountId :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_accountId = Lens.lens (\CoverageFilterCriteria' {accountId} -> accountId) (\s@CoverageFilterCriteria' {} a -> s {accountId = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon EC2 instance tags to filter on.
coverageFilterCriteria_ec2InstanceTags :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageMapFilter))
coverageFilterCriteria_ec2InstanceTags = Lens.lens (\CoverageFilterCriteria' {ec2InstanceTags} -> ec2InstanceTags) (\s@CoverageFilterCriteria' {} a -> s {ec2InstanceTags = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon ECR image tags to filter on.
coverageFilterCriteria_ecrImageTags :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_ecrImageTags = Lens.lens (\CoverageFilterCriteria' {ecrImageTags} -> ecrImageTags) (\s@CoverageFilterCriteria' {} a -> s {ecrImageTags = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon ECR repository name to filter on.
coverageFilterCriteria_ecrRepositoryName :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_ecrRepositoryName = Lens.lens (\CoverageFilterCriteria' {ecrRepositoryName} -> ecrRepositoryName) (\s@CoverageFilterCriteria' {} a -> s {ecrRepositoryName = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Returns coverage statistics for AWS Lambda functions filtered by
-- function names.
coverageFilterCriteria_lambdaFunctionName :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_lambdaFunctionName = Lens.lens (\CoverageFilterCriteria' {lambdaFunctionName} -> lambdaFunctionName) (\s@CoverageFilterCriteria' {} a -> s {lambdaFunctionName = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Returns coverage statistics for AWS Lambda functions filtered by
-- runtime.
coverageFilterCriteria_lambdaFunctionRuntime :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_lambdaFunctionRuntime = Lens.lens (\CoverageFilterCriteria' {lambdaFunctionRuntime} -> lambdaFunctionRuntime) (\s@CoverageFilterCriteria' {} a -> s {lambdaFunctionRuntime = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Returns coverage statistics for AWS Lambda functions filtered by tag.
coverageFilterCriteria_lambdaFunctionTags :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageMapFilter))
coverageFilterCriteria_lambdaFunctionTags = Lens.lens (\CoverageFilterCriteria' {lambdaFunctionTags} -> lambdaFunctionTags) (\s@CoverageFilterCriteria' {} a -> s {lambdaFunctionTags = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Web Services resource IDs to return coverage
-- statistics for.
coverageFilterCriteria_resourceId :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_resourceId = Lens.lens (\CoverageFilterCriteria' {resourceId} -> resourceId) (\s@CoverageFilterCriteria' {} a -> s {resourceId = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Web Services resource types to return coverage
-- statistics for. The values can be @AWS_EC2_INSTANCE@ or
-- @AWS_ECR_REPOSITORY@.
coverageFilterCriteria_resourceType :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_resourceType = Lens.lens (\CoverageFilterCriteria' {resourceType} -> resourceType) (\s@CoverageFilterCriteria' {} a -> s {resourceType = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The scan status code to filter on.
coverageFilterCriteria_scanStatusCode :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_scanStatusCode = Lens.lens (\CoverageFilterCriteria' {scanStatusCode} -> scanStatusCode) (\s@CoverageFilterCriteria' {} a -> s {scanStatusCode = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The scan status reason to filter on.
coverageFilterCriteria_scanStatusReason :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_scanStatusReason = Lens.lens (\CoverageFilterCriteria' {scanStatusReason} -> scanStatusReason) (\s@CoverageFilterCriteria' {} a -> s {scanStatusReason = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | An array of Amazon Inspector scan types to return coverage statistics
-- for.
coverageFilterCriteria_scanType :: Lens.Lens' CoverageFilterCriteria (Prelude.Maybe (Prelude.NonEmpty CoverageStringFilter))
coverageFilterCriteria_scanType = Lens.lens (\CoverageFilterCriteria' {scanType} -> scanType) (\s@CoverageFilterCriteria' {} a -> s {scanType = a} :: CoverageFilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable CoverageFilterCriteria where
  hashWithSalt _salt CoverageFilterCriteria' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` ec2InstanceTags
      `Prelude.hashWithSalt` ecrImageTags
      `Prelude.hashWithSalt` ecrRepositoryName
      `Prelude.hashWithSalt` lambdaFunctionName
      `Prelude.hashWithSalt` lambdaFunctionRuntime
      `Prelude.hashWithSalt` lambdaFunctionTags
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scanStatusCode
      `Prelude.hashWithSalt` scanStatusReason
      `Prelude.hashWithSalt` scanType

instance Prelude.NFData CoverageFilterCriteria where
  rnf CoverageFilterCriteria' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf ec2InstanceTags
      `Prelude.seq` Prelude.rnf ecrImageTags
      `Prelude.seq` Prelude.rnf ecrRepositoryName
      `Prelude.seq` Prelude.rnf lambdaFunctionName
      `Prelude.seq` Prelude.rnf lambdaFunctionRuntime
      `Prelude.seq` Prelude.rnf lambdaFunctionTags
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf scanStatusCode
      `Prelude.seq` Prelude.rnf scanStatusReason
      `Prelude.seq` Prelude.rnf scanType

instance Data.ToJSON CoverageFilterCriteria where
  toJSON CoverageFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountId" Data..=) Prelude.<$> accountId,
            ("ec2InstanceTags" Data..=)
              Prelude.<$> ec2InstanceTags,
            ("ecrImageTags" Data..=) Prelude.<$> ecrImageTags,
            ("ecrRepositoryName" Data..=)
              Prelude.<$> ecrRepositoryName,
            ("lambdaFunctionName" Data..=)
              Prelude.<$> lambdaFunctionName,
            ("lambdaFunctionRuntime" Data..=)
              Prelude.<$> lambdaFunctionRuntime,
            ("lambdaFunctionTags" Data..=)
              Prelude.<$> lambdaFunctionTags,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("resourceType" Data..=) Prelude.<$> resourceType,
            ("scanStatusCode" Data..=)
              Prelude.<$> scanStatusCode,
            ("scanStatusReason" Data..=)
              Prelude.<$> scanStatusReason,
            ("scanType" Data..=) Prelude.<$> scanType
          ]
      )
