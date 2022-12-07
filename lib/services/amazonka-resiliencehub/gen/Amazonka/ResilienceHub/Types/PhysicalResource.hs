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
-- Copyright   : (c) 2013-2022 Brendan Hay
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

-- | Defines a physical resource. A physical resource is a resource that
-- exists in your account. It can be identified using an Amazon Resource
-- Name (ARN) or a Resilience Hub-native identifier.
--
-- /See:/ 'newPhysicalResource' smart constructor.
data PhysicalResource = PhysicalResource'
  { -- | The application components that belong to this resource.
    appComponents :: Prelude.Maybe [AppComponent],
    -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
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
-- 'appComponents', 'physicalResource_appComponents' - The application components that belong to this resource.
--
-- 'resourceName', 'physicalResource_resourceName' - The name of the resource.
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
      { appComponents = Prelude.Nothing,
        resourceName = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        physicalResourceId = pPhysicalResourceId_,
        resourceType = pResourceType_
      }

-- | The application components that belong to this resource.
physicalResource_appComponents :: Lens.Lens' PhysicalResource (Prelude.Maybe [AppComponent])
physicalResource_appComponents = Lens.lens (\PhysicalResource' {appComponents} -> appComponents) (\s@PhysicalResource' {} a -> s {appComponents = a} :: PhysicalResource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the resource.
physicalResource_resourceName :: Lens.Lens' PhysicalResource (Prelude.Maybe Prelude.Text)
physicalResource_resourceName = Lens.lens (\PhysicalResource' {resourceName} -> resourceName) (\s@PhysicalResource' {} a -> s {resourceName = a} :: PhysicalResource)

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
            Prelude.<$> (x Data..:? "appComponents" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..: "logicalResourceId")
            Prelude.<*> (x Data..: "physicalResourceId")
            Prelude.<*> (x Data..: "resourceType")
      )

instance Prelude.Hashable PhysicalResource where
  hashWithSalt _salt PhysicalResource' {..} =
    _salt `Prelude.hashWithSalt` appComponents
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData PhysicalResource where
  rnf PhysicalResource' {..} =
    Prelude.rnf appComponents
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceType
