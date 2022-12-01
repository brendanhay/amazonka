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
-- Module      : Amazonka.Config.Types.ResourceKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceKey where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details that identify a resource within Config, including the
-- resource type and resource ID.
--
-- /See:/ 'newResourceKey' smart constructor.
data ResourceKey = ResourceKey'
  { -- | The resource type.
    resourceType :: ResourceType,
    -- | The ID of the resource (for example., sg-xxxxxx).
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceKey_resourceType' - The resource type.
--
-- 'resourceId', 'resourceKey_resourceId' - The ID of the resource (for example., sg-xxxxxx).
newResourceKey ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resourceId'
  Prelude.Text ->
  ResourceKey
newResourceKey pResourceType_ pResourceId_ =
  ResourceKey'
    { resourceType = pResourceType_,
      resourceId = pResourceId_
    }

-- | The resource type.
resourceKey_resourceType :: Lens.Lens' ResourceKey ResourceType
resourceKey_resourceType = Lens.lens (\ResourceKey' {resourceType} -> resourceType) (\s@ResourceKey' {} a -> s {resourceType = a} :: ResourceKey)

-- | The ID of the resource (for example., sg-xxxxxx).
resourceKey_resourceId :: Lens.Lens' ResourceKey Prelude.Text
resourceKey_resourceId = Lens.lens (\ResourceKey' {resourceId} -> resourceId) (\s@ResourceKey' {} a -> s {resourceId = a} :: ResourceKey)

instance Core.FromJSON ResourceKey where
  parseJSON =
    Core.withObject
      "ResourceKey"
      ( \x ->
          ResourceKey'
            Prelude.<$> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "resourceId")
      )

instance Prelude.Hashable ResourceKey where
  hashWithSalt _salt ResourceKey' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ResourceKey where
  rnf ResourceKey' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId

instance Core.ToJSON ResourceKey where
  toJSON ResourceKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceType" Core..= resourceType),
            Prelude.Just ("resourceId" Core..= resourceId)
          ]
      )
