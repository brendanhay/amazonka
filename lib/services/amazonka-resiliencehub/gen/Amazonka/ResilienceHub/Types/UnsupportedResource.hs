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
-- Module      : Amazonka.ResilienceHub.Types.UnsupportedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.UnsupportedResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.LogicalResourceId
import Amazonka.ResilienceHub.Types.PhysicalResourceId

-- | Defines a resource that is not supported by Resilience Hub.
--
-- /See:/ 'newUnsupportedResource' smart constructor.
data UnsupportedResource = UnsupportedResource'
  { -- | The logical resource identifier for the unsupported resource.
    logicalResourceId :: LogicalResourceId,
    -- | The physical resource identifier for the unsupported resource.
    physicalResourceId :: PhysicalResourceId,
    -- | The type of resource.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsupportedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalResourceId', 'unsupportedResource_logicalResourceId' - The logical resource identifier for the unsupported resource.
--
-- 'physicalResourceId', 'unsupportedResource_physicalResourceId' - The physical resource identifier for the unsupported resource.
--
-- 'resourceType', 'unsupportedResource_resourceType' - The type of resource.
newUnsupportedResource ::
  -- | 'logicalResourceId'
  LogicalResourceId ->
  -- | 'physicalResourceId'
  PhysicalResourceId ->
  -- | 'resourceType'
  Prelude.Text ->
  UnsupportedResource
newUnsupportedResource
  pLogicalResourceId_
  pPhysicalResourceId_
  pResourceType_ =
    UnsupportedResource'
      { logicalResourceId =
          pLogicalResourceId_,
        physicalResourceId = pPhysicalResourceId_,
        resourceType = pResourceType_
      }

-- | The logical resource identifier for the unsupported resource.
unsupportedResource_logicalResourceId :: Lens.Lens' UnsupportedResource LogicalResourceId
unsupportedResource_logicalResourceId = Lens.lens (\UnsupportedResource' {logicalResourceId} -> logicalResourceId) (\s@UnsupportedResource' {} a -> s {logicalResourceId = a} :: UnsupportedResource)

-- | The physical resource identifier for the unsupported resource.
unsupportedResource_physicalResourceId :: Lens.Lens' UnsupportedResource PhysicalResourceId
unsupportedResource_physicalResourceId = Lens.lens (\UnsupportedResource' {physicalResourceId} -> physicalResourceId) (\s@UnsupportedResource' {} a -> s {physicalResourceId = a} :: UnsupportedResource)

-- | The type of resource.
unsupportedResource_resourceType :: Lens.Lens' UnsupportedResource Prelude.Text
unsupportedResource_resourceType = Lens.lens (\UnsupportedResource' {resourceType} -> resourceType) (\s@UnsupportedResource' {} a -> s {resourceType = a} :: UnsupportedResource)

instance Data.FromJSON UnsupportedResource where
  parseJSON =
    Data.withObject
      "UnsupportedResource"
      ( \x ->
          UnsupportedResource'
            Prelude.<$> (x Data..: "logicalResourceId")
            Prelude.<*> (x Data..: "physicalResourceId")
            Prelude.<*> (x Data..: "resourceType")
      )

instance Prelude.Hashable UnsupportedResource where
  hashWithSalt _salt UnsupportedResource' {..} =
    _salt
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData UnsupportedResource where
  rnf UnsupportedResource' {..} =
    Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceType
