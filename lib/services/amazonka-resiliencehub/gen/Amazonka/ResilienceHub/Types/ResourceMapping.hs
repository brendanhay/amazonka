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
-- Module      : Amazonka.ResilienceHub.Types.ResourceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.PhysicalResourceId
import Amazonka.ResilienceHub.Types.ResourceMappingType

-- | Defines a resource mapping.
--
-- /See:/ 'newResourceMapping' smart constructor.
data ResourceMapping = ResourceMapping'
  { -- | The name of the application this resource is mapped to.
    appRegistryAppName :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudFormation stack this resource is mapped to.
    logicalStackName :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group this resource is mapped to.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource this resource is mapped to.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The short name of the Terraform source.
    terraformSourceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of resource mapping.
    --
    -- [AppRegistryApp]
    --     The resource is mapped to another application. The name of the
    --     application is contained in the @appRegistryAppName@ property.
    --
    -- [CfnStack]
    --     The resource is mapped to a CloudFormation stack. The name of the
    --     CloudFormation stack is contained in the @logicalStackName@
    --     property.
    --
    -- [Resource]
    --     The resource is mapped to another resource. The name of the resource
    --     is contained in the @resourceName@ property.
    --
    -- [ResourceGroup]
    --     The resource is mapped to a resource group. The name of the resource
    --     group is contained in the @resourceGroupName@ property.
    mappingType :: ResourceMappingType,
    -- | The identifier of this resource.
    physicalResourceId :: PhysicalResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appRegistryAppName', 'resourceMapping_appRegistryAppName' - The name of the application this resource is mapped to.
--
-- 'logicalStackName', 'resourceMapping_logicalStackName' - The name of the CloudFormation stack this resource is mapped to.
--
-- 'resourceGroupName', 'resourceMapping_resourceGroupName' - The name of the resource group this resource is mapped to.
--
-- 'resourceName', 'resourceMapping_resourceName' - The name of the resource this resource is mapped to.
--
-- 'terraformSourceName', 'resourceMapping_terraformSourceName' - The short name of the Terraform source.
--
-- 'mappingType', 'resourceMapping_mappingType' - Specifies the type of resource mapping.
--
-- [AppRegistryApp]
--     The resource is mapped to another application. The name of the
--     application is contained in the @appRegistryAppName@ property.
--
-- [CfnStack]
--     The resource is mapped to a CloudFormation stack. The name of the
--     CloudFormation stack is contained in the @logicalStackName@
--     property.
--
-- [Resource]
--     The resource is mapped to another resource. The name of the resource
--     is contained in the @resourceName@ property.
--
-- [ResourceGroup]
--     The resource is mapped to a resource group. The name of the resource
--     group is contained in the @resourceGroupName@ property.
--
-- 'physicalResourceId', 'resourceMapping_physicalResourceId' - The identifier of this resource.
newResourceMapping ::
  -- | 'mappingType'
  ResourceMappingType ->
  -- | 'physicalResourceId'
  PhysicalResourceId ->
  ResourceMapping
newResourceMapping pMappingType_ pPhysicalResourceId_ =
  ResourceMapping'
    { appRegistryAppName =
        Prelude.Nothing,
      logicalStackName = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      terraformSourceName = Prelude.Nothing,
      mappingType = pMappingType_,
      physicalResourceId = pPhysicalResourceId_
    }

-- | The name of the application this resource is mapped to.
resourceMapping_appRegistryAppName :: Lens.Lens' ResourceMapping (Prelude.Maybe Prelude.Text)
resourceMapping_appRegistryAppName = Lens.lens (\ResourceMapping' {appRegistryAppName} -> appRegistryAppName) (\s@ResourceMapping' {} a -> s {appRegistryAppName = a} :: ResourceMapping)

-- | The name of the CloudFormation stack this resource is mapped to.
resourceMapping_logicalStackName :: Lens.Lens' ResourceMapping (Prelude.Maybe Prelude.Text)
resourceMapping_logicalStackName = Lens.lens (\ResourceMapping' {logicalStackName} -> logicalStackName) (\s@ResourceMapping' {} a -> s {logicalStackName = a} :: ResourceMapping)

-- | The name of the resource group this resource is mapped to.
resourceMapping_resourceGroupName :: Lens.Lens' ResourceMapping (Prelude.Maybe Prelude.Text)
resourceMapping_resourceGroupName = Lens.lens (\ResourceMapping' {resourceGroupName} -> resourceGroupName) (\s@ResourceMapping' {} a -> s {resourceGroupName = a} :: ResourceMapping)

-- | The name of the resource this resource is mapped to.
resourceMapping_resourceName :: Lens.Lens' ResourceMapping (Prelude.Maybe Prelude.Text)
resourceMapping_resourceName = Lens.lens (\ResourceMapping' {resourceName} -> resourceName) (\s@ResourceMapping' {} a -> s {resourceName = a} :: ResourceMapping)

-- | The short name of the Terraform source.
resourceMapping_terraformSourceName :: Lens.Lens' ResourceMapping (Prelude.Maybe Prelude.Text)
resourceMapping_terraformSourceName = Lens.lens (\ResourceMapping' {terraformSourceName} -> terraformSourceName) (\s@ResourceMapping' {} a -> s {terraformSourceName = a} :: ResourceMapping)

-- | Specifies the type of resource mapping.
--
-- [AppRegistryApp]
--     The resource is mapped to another application. The name of the
--     application is contained in the @appRegistryAppName@ property.
--
-- [CfnStack]
--     The resource is mapped to a CloudFormation stack. The name of the
--     CloudFormation stack is contained in the @logicalStackName@
--     property.
--
-- [Resource]
--     The resource is mapped to another resource. The name of the resource
--     is contained in the @resourceName@ property.
--
-- [ResourceGroup]
--     The resource is mapped to a resource group. The name of the resource
--     group is contained in the @resourceGroupName@ property.
resourceMapping_mappingType :: Lens.Lens' ResourceMapping ResourceMappingType
resourceMapping_mappingType = Lens.lens (\ResourceMapping' {mappingType} -> mappingType) (\s@ResourceMapping' {} a -> s {mappingType = a} :: ResourceMapping)

-- | The identifier of this resource.
resourceMapping_physicalResourceId :: Lens.Lens' ResourceMapping PhysicalResourceId
resourceMapping_physicalResourceId = Lens.lens (\ResourceMapping' {physicalResourceId} -> physicalResourceId) (\s@ResourceMapping' {} a -> s {physicalResourceId = a} :: ResourceMapping)

instance Data.FromJSON ResourceMapping where
  parseJSON =
    Data.withObject
      "ResourceMapping"
      ( \x ->
          ResourceMapping'
            Prelude.<$> (x Data..:? "appRegistryAppName")
            Prelude.<*> (x Data..:? "logicalStackName")
            Prelude.<*> (x Data..:? "resourceGroupName")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "terraformSourceName")
            Prelude.<*> (x Data..: "mappingType")
            Prelude.<*> (x Data..: "physicalResourceId")
      )

instance Prelude.Hashable ResourceMapping where
  hashWithSalt _salt ResourceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` appRegistryAppName
      `Prelude.hashWithSalt` logicalStackName
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` terraformSourceName
      `Prelude.hashWithSalt` mappingType
      `Prelude.hashWithSalt` physicalResourceId

instance Prelude.NFData ResourceMapping where
  rnf ResourceMapping' {..} =
    Prelude.rnf appRegistryAppName
      `Prelude.seq` Prelude.rnf logicalStackName
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf terraformSourceName
      `Prelude.seq` Prelude.rnf mappingType
      `Prelude.seq` Prelude.rnf physicalResourceId

instance Data.ToJSON ResourceMapping where
  toJSON ResourceMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appRegistryAppName" Data..=)
              Prelude.<$> appRegistryAppName,
            ("logicalStackName" Data..=)
              Prelude.<$> logicalStackName,
            ("resourceGroupName" Data..=)
              Prelude.<$> resourceGroupName,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            ("terraformSourceName" Data..=)
              Prelude.<$> terraformSourceName,
            Prelude.Just ("mappingType" Data..= mappingType),
            Prelude.Just
              ("physicalResourceId" Data..= physicalResourceId)
          ]
      )
