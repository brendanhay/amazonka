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
-- Module      : Network.AWS.WorkDocs.Types.ResourcePathComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourcePathComponent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the resource path.
--
-- /See:/ 'newResourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { -- | The ID of the resource path.
    id :: Core.Maybe Core.Text,
    -- | The name of the resource path.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResourcePathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'resourcePathComponent_id' - The ID of the resource path.
--
-- 'name', 'resourcePathComponent_name' - The name of the resource path.
newResourcePathComponent ::
  ResourcePathComponent
newResourcePathComponent =
  ResourcePathComponent'
    { id = Core.Nothing,
      name = Core.Nothing
    }

-- | The ID of the resource path.
resourcePathComponent_id :: Lens.Lens' ResourcePathComponent (Core.Maybe Core.Text)
resourcePathComponent_id = Lens.lens (\ResourcePathComponent' {id} -> id) (\s@ResourcePathComponent' {} a -> s {id = a} :: ResourcePathComponent)

-- | The name of the resource path.
resourcePathComponent_name :: Lens.Lens' ResourcePathComponent (Core.Maybe Core.Text)
resourcePathComponent_name = Lens.lens (\ResourcePathComponent' {name} -> name) (\s@ResourcePathComponent' {} a -> s {name = a} :: ResourcePathComponent)

instance Core.FromJSON ResourcePathComponent where
  parseJSON =
    Core.withObject
      "ResourcePathComponent"
      ( \x ->
          ResourcePathComponent'
            Core.<$> (x Core..:? "Id") Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable ResourcePathComponent

instance Core.NFData ResourcePathComponent
