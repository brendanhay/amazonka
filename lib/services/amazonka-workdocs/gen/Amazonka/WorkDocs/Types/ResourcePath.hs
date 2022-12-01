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
-- Module      : Amazonka.WorkDocs.Types.ResourcePath
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResourcePath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.ResourcePathComponent

-- | Describes the path information of a resource.
--
-- /See:/ 'newResourcePath' smart constructor.
data ResourcePath = ResourcePath'
  { -- | The components of the resource path.
    components :: Prelude.Maybe [ResourcePathComponent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'components', 'resourcePath_components' - The components of the resource path.
newResourcePath ::
  ResourcePath
newResourcePath =
  ResourcePath' {components = Prelude.Nothing}

-- | The components of the resource path.
resourcePath_components :: Lens.Lens' ResourcePath (Prelude.Maybe [ResourcePathComponent])
resourcePath_components = Lens.lens (\ResourcePath' {components} -> components) (\s@ResourcePath' {} a -> s {components = a} :: ResourcePath) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ResourcePath where
  parseJSON =
    Core.withObject
      "ResourcePath"
      ( \x ->
          ResourcePath'
            Prelude.<$> (x Core..:? "Components" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourcePath where
  hashWithSalt _salt ResourcePath' {..} =
    _salt `Prelude.hashWithSalt` components

instance Prelude.NFData ResourcePath where
  rnf ResourcePath' {..} = Prelude.rnf components
