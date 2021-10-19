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
-- Module      : Network.AWS.Greengrass.Types.ResourceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDefinitionVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.Resource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a resource definition version.
--
-- /See:/ 'newResourceDefinitionVersion' smart constructor.
data ResourceDefinitionVersion = ResourceDefinitionVersion'
  { -- | A list of resources.
    resources :: Prelude.Maybe [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'resourceDefinitionVersion_resources' - A list of resources.
newResourceDefinitionVersion ::
  ResourceDefinitionVersion
newResourceDefinitionVersion =
  ResourceDefinitionVersion'
    { resources =
        Prelude.Nothing
    }

-- | A list of resources.
resourceDefinitionVersion_resources :: Lens.Lens' ResourceDefinitionVersion (Prelude.Maybe [Resource])
resourceDefinitionVersion_resources = Lens.lens (\ResourceDefinitionVersion' {resources} -> resources) (\s@ResourceDefinitionVersion' {} a -> s {resources = a} :: ResourceDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ResourceDefinitionVersion where
  parseJSON =
    Core.withObject
      "ResourceDefinitionVersion"
      ( \x ->
          ResourceDefinitionVersion'
            Prelude.<$> (x Core..:? "Resources" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceDefinitionVersion

instance Prelude.NFData ResourceDefinitionVersion

instance Core.ToJSON ResourceDefinitionVersion where
  toJSON ResourceDefinitionVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Resources" Core..=) Prelude.<$> resources]
      )
