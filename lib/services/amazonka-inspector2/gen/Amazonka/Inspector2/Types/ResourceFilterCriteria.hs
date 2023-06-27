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
-- Module      : Amazonka.Inspector2.Types.ResourceFilterCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ResourceMapFilter
import Amazonka.Inspector2.Types.ResourceStringFilter
import qualified Amazonka.Prelude as Prelude

-- | The resource filter criteria for a Software bill of materials (SBOM)
-- report.
--
-- /See:/ 'newResourceFilterCriteria' smart constructor.
data ResourceFilterCriteria = ResourceFilterCriteria'
  { -- | The account IDs used as resource filter criteria.
    accountId :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter),
    -- | The EC2 instance tags used as resource filter criteria.
    ec2InstanceTags :: Prelude.Maybe (Prelude.NonEmpty ResourceMapFilter),
    -- | The ECR image tags used as resource filter criteria.
    ecrImageTags :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter),
    -- | The ECR repository names used as resource filter criteria.
    ecrRepositoryName :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter),
    -- | The AWS Lambda function name used as resource filter criteria.
    lambdaFunctionName :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter),
    -- | The AWS Lambda function tags used as resource filter criteria.
    lambdaFunctionTags :: Prelude.Maybe (Prelude.NonEmpty ResourceMapFilter),
    -- | The resource IDs used as resource filter criteria.
    resourceId :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter),
    -- | The resource types used as resource filter criteria.
    resourceType :: Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'resourceFilterCriteria_accountId' - The account IDs used as resource filter criteria.
--
-- 'ec2InstanceTags', 'resourceFilterCriteria_ec2InstanceTags' - The EC2 instance tags used as resource filter criteria.
--
-- 'ecrImageTags', 'resourceFilterCriteria_ecrImageTags' - The ECR image tags used as resource filter criteria.
--
-- 'ecrRepositoryName', 'resourceFilterCriteria_ecrRepositoryName' - The ECR repository names used as resource filter criteria.
--
-- 'lambdaFunctionName', 'resourceFilterCriteria_lambdaFunctionName' - The AWS Lambda function name used as resource filter criteria.
--
-- 'lambdaFunctionTags', 'resourceFilterCriteria_lambdaFunctionTags' - The AWS Lambda function tags used as resource filter criteria.
--
-- 'resourceId', 'resourceFilterCriteria_resourceId' - The resource IDs used as resource filter criteria.
--
-- 'resourceType', 'resourceFilterCriteria_resourceType' - The resource types used as resource filter criteria.
newResourceFilterCriteria ::
  ResourceFilterCriteria
newResourceFilterCriteria =
  ResourceFilterCriteria'
    { accountId =
        Prelude.Nothing,
      ec2InstanceTags = Prelude.Nothing,
      ecrImageTags = Prelude.Nothing,
      ecrRepositoryName = Prelude.Nothing,
      lambdaFunctionName = Prelude.Nothing,
      lambdaFunctionTags = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The account IDs used as resource filter criteria.
resourceFilterCriteria_accountId :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_accountId = Lens.lens (\ResourceFilterCriteria' {accountId} -> accountId) (\s@ResourceFilterCriteria' {} a -> s {accountId = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The EC2 instance tags used as resource filter criteria.
resourceFilterCriteria_ec2InstanceTags :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceMapFilter))
resourceFilterCriteria_ec2InstanceTags = Lens.lens (\ResourceFilterCriteria' {ec2InstanceTags} -> ec2InstanceTags) (\s@ResourceFilterCriteria' {} a -> s {ec2InstanceTags = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The ECR image tags used as resource filter criteria.
resourceFilterCriteria_ecrImageTags :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_ecrImageTags = Lens.lens (\ResourceFilterCriteria' {ecrImageTags} -> ecrImageTags) (\s@ResourceFilterCriteria' {} a -> s {ecrImageTags = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The ECR repository names used as resource filter criteria.
resourceFilterCriteria_ecrRepositoryName :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_ecrRepositoryName = Lens.lens (\ResourceFilterCriteria' {ecrRepositoryName} -> ecrRepositoryName) (\s@ResourceFilterCriteria' {} a -> s {ecrRepositoryName = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Lambda function name used as resource filter criteria.
resourceFilterCriteria_lambdaFunctionName :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_lambdaFunctionName = Lens.lens (\ResourceFilterCriteria' {lambdaFunctionName} -> lambdaFunctionName) (\s@ResourceFilterCriteria' {} a -> s {lambdaFunctionName = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The AWS Lambda function tags used as resource filter criteria.
resourceFilterCriteria_lambdaFunctionTags :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceMapFilter))
resourceFilterCriteria_lambdaFunctionTags = Lens.lens (\ResourceFilterCriteria' {lambdaFunctionTags} -> lambdaFunctionTags) (\s@ResourceFilterCriteria' {} a -> s {lambdaFunctionTags = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The resource IDs used as resource filter criteria.
resourceFilterCriteria_resourceId :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_resourceId = Lens.lens (\ResourceFilterCriteria' {resourceId} -> resourceId) (\s@ResourceFilterCriteria' {} a -> s {resourceId = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The resource types used as resource filter criteria.
resourceFilterCriteria_resourceType :: Lens.Lens' ResourceFilterCriteria (Prelude.Maybe (Prelude.NonEmpty ResourceStringFilter))
resourceFilterCriteria_resourceType = Lens.lens (\ResourceFilterCriteria' {resourceType} -> resourceType) (\s@ResourceFilterCriteria' {} a -> s {resourceType = a} :: ResourceFilterCriteria) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ResourceFilterCriteria where
  parseJSON =
    Data.withObject
      "ResourceFilterCriteria"
      ( \x ->
          ResourceFilterCriteria'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "ec2InstanceTags")
            Prelude.<*> (x Data..:? "ecrImageTags")
            Prelude.<*> (x Data..:? "ecrRepositoryName")
            Prelude.<*> (x Data..:? "lambdaFunctionName")
            Prelude.<*> (x Data..:? "lambdaFunctionTags")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable ResourceFilterCriteria where
  hashWithSalt _salt ResourceFilterCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` ec2InstanceTags
      `Prelude.hashWithSalt` ecrImageTags
      `Prelude.hashWithSalt` ecrRepositoryName
      `Prelude.hashWithSalt` lambdaFunctionName
      `Prelude.hashWithSalt` lambdaFunctionTags
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceFilterCriteria where
  rnf ResourceFilterCriteria' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf ec2InstanceTags
      `Prelude.seq` Prelude.rnf ecrImageTags
      `Prelude.seq` Prelude.rnf ecrRepositoryName
      `Prelude.seq` Prelude.rnf lambdaFunctionName
      `Prelude.seq` Prelude.rnf lambdaFunctionTags
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToJSON ResourceFilterCriteria where
  toJSON ResourceFilterCriteria' {..} =
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
            ("lambdaFunctionTags" Data..=)
              Prelude.<$> lambdaFunctionTags,
            ("resourceId" Data..=) Prelude.<$> resourceId,
            ("resourceType" Data..=) Prelude.<$> resourceType
          ]
      )
