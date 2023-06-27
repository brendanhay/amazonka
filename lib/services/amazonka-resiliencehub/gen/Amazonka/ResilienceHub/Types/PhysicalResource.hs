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
-- Module      : Amazonka.ResilienceHub.Types.PhysicalResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.PhysicalResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.AppComponent
import Amazonka.ResilienceHub.Types.LogicalResourceId
import Amazonka.ResilienceHub.Types.PhysicalResourceId
import Amazonka.ResilienceHub.Types.ResourceSourceType

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or an Resilience Hub-native identifier.
--
-- /See:/ 'newPhysicalResource' smart constructor.
data PhysicalResource = PhysicalResource'
  { -- | Additional configuration parameters for an Resilience Hub application.
    -- If you want to implement @additionalInfo@ through the Resilience Hub
    -- console rather than using an API call, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    -- Currently, this parameter accepts a key-value mapping (in a string
    -- format) of only one failover region and one associated account.
    --
    -- Key: @\"failover-regions\"@
    --
    -- Value:
    -- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | The application components that belong to this resource.
    appComponents :: Prelude.Maybe [AppComponent],
    -- | Indicates if a resource is included or excluded from the assessment.
    excluded :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parent resource.
    parentResourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of input source.
    sourceType :: Prelude.Maybe ResourceSourceType,
    -- | The logical identifier of the resource.
    logicalResourceId :: LogicalResourceId,
    -- | The physical identifier of the resource.
    physicalResourceId :: PhysicalResourceId,
    -- | The type of resource.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'physicalResource_additionalInfo' - Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- 'appComponents', 'physicalResource_appComponents' - The application components that belong to this resource.
--
-- 'excluded', 'physicalResource_excluded' - Indicates if a resource is included or excluded from the assessment.
--
-- 'parentResourceName', 'physicalResource_parentResourceName' - The name of the parent resource.
--
-- 'resourceName', 'physicalResource_resourceName' - The name of the resource.
--
-- 'sourceType', 'physicalResource_sourceType' - The type of input source.
--
-- 'logicalResourceId', 'physicalResource_logicalResourceId' - The logical identifier of the resource.
--
-- 'physicalResourceId', 'physicalResource_physicalResourceId' - The physical identifier of the resource.
--
-- 'resourceType', 'physicalResource_resourceType' - The type of resource.
newPhysicalResource ::
  -- | 'logicalResourceId'
  LogicalResourceId ->
  -- | 'physicalResourceId'
  PhysicalResourceId ->
  -- | 'resourceType'
  Prelude.Text ->
  PhysicalResource
newPhysicalResource
  pLogicalResourceId_
  pPhysicalResourceId_
  pResourceType_ =
    PhysicalResource'
      { additionalInfo = Prelude.Nothing,
        appComponents = Prelude.Nothing,
        excluded = Prelude.Nothing,
        parentResourceName = Prelude.Nothing,
        resourceName = Prelude.Nothing,
        sourceType = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        physicalResourceId = pPhysicalResourceId_,
        resourceType = pResourceType_
      }

-- | Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
physicalResource_additionalInfo :: Lens.Lens' PhysicalResource (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
physicalResource_additionalInfo = Lens.lens (\PhysicalResource' {additionalInfo} -> additionalInfo) (\s@PhysicalResource' {} a -> s {additionalInfo = a} :: PhysicalResource) Prelude.. Lens.mapping Lens.coerced

-- | The application components that belong to this resource.
physicalResource_appComponents :: Lens.Lens' PhysicalResource (Prelude.Maybe [AppComponent])
physicalResource_appComponents = Lens.lens (\PhysicalResource' {appComponents} -> appComponents) (\s@PhysicalResource' {} a -> s {appComponents = a} :: PhysicalResource) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if a resource is included or excluded from the assessment.
physicalResource_excluded :: Lens.Lens' PhysicalResource (Prelude.Maybe Prelude.Bool)
physicalResource_excluded = Lens.lens (\PhysicalResource' {excluded} -> excluded) (\s@PhysicalResource' {} a -> s {excluded = a} :: PhysicalResource)

-- | The name of the parent resource.
physicalResource_parentResourceName :: Lens.Lens' PhysicalResource (Prelude.Maybe Prelude.Text)
physicalResource_parentResourceName = Lens.lens (\PhysicalResource' {parentResourceName} -> parentResourceName) (\s@PhysicalResource' {} a -> s {parentResourceName = a} :: PhysicalResource)

-- | The name of the resource.
physicalResource_resourceName :: Lens.Lens' PhysicalResource (Prelude.Maybe Prelude.Text)
physicalResource_resourceName = Lens.lens (\PhysicalResource' {resourceName} -> resourceName) (\s@PhysicalResource' {} a -> s {resourceName = a} :: PhysicalResource)

-- | The type of input source.
physicalResource_sourceType :: Lens.Lens' PhysicalResource (Prelude.Maybe ResourceSourceType)
physicalResource_sourceType = Lens.lens (\PhysicalResource' {sourceType} -> sourceType) (\s@PhysicalResource' {} a -> s {sourceType = a} :: PhysicalResource)

-- | The logical identifier of the resource.
physicalResource_logicalResourceId :: Lens.Lens' PhysicalResource LogicalResourceId
physicalResource_logicalResourceId = Lens.lens (\PhysicalResource' {logicalResourceId} -> logicalResourceId) (\s@PhysicalResource' {} a -> s {logicalResourceId = a} :: PhysicalResource)

-- | The physical identifier of the resource.
physicalResource_physicalResourceId :: Lens.Lens' PhysicalResource PhysicalResourceId
physicalResource_physicalResourceId = Lens.lens (\PhysicalResource' {physicalResourceId} -> physicalResourceId) (\s@PhysicalResource' {} a -> s {physicalResourceId = a} :: PhysicalResource)

-- | The type of resource.
physicalResource_resourceType :: Lens.Lens' PhysicalResource Prelude.Text
physicalResource_resourceType = Lens.lens (\PhysicalResource' {resourceType} -> resourceType) (\s@PhysicalResource' {} a -> s {resourceType = a} :: PhysicalResource)

instance Data.FromJSON PhysicalResource where
  parseJSON =
    Data.withObject
      "PhysicalResource"
      ( \x ->
          PhysicalResource'
            Prelude.<$> (x Data..:? "additionalInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "appComponents" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "excluded")
            Prelude.<*> (x Data..:? "parentResourceName")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "sourceType")
            Prelude.<*> (x Data..: "logicalResourceId")
            Prelude.<*> (x Data..: "physicalResourceId")
            Prelude.<*> (x Data..: "resourceType")
      )

instance Prelude.Hashable PhysicalResource where
  hashWithSalt _salt PhysicalResource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` appComponents
      `Prelude.hashWithSalt` excluded
      `Prelude.hashWithSalt` parentResourceName
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData PhysicalResource where
  rnf PhysicalResource' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf appComponents
      `Prelude.seq` Prelude.rnf excluded
      `Prelude.seq` Prelude.rnf parentResourceName
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceType
