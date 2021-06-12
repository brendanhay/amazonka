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
-- Module      : Network.AWS.Greengrass.Types.CoreDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.CoreDefinitionVersion where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.Core
import qualified Network.AWS.Lens as Lens

-- | Information about a core definition version.
--
-- /See:/ 'newCoreDefinitionVersion' smart constructor.
data CoreDefinitionVersion = CoreDefinitionVersion'
  { -- | A list of cores in the core definition version.
    cores :: Core.Maybe [Core]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoreDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cores', 'coreDefinitionVersion_cores' - A list of cores in the core definition version.
newCoreDefinitionVersion ::
  CoreDefinitionVersion
newCoreDefinitionVersion =
  CoreDefinitionVersion' {cores = Core.Nothing}

-- | A list of cores in the core definition version.
coreDefinitionVersion_cores :: Lens.Lens' CoreDefinitionVersion (Core.Maybe [Core])
coreDefinitionVersion_cores = Lens.lens (\CoreDefinitionVersion' {cores} -> cores) (\s@CoreDefinitionVersion' {} a -> s {cores = a} :: CoreDefinitionVersion) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON CoreDefinitionVersion where
  parseJSON =
    Core.withObject
      "CoreDefinitionVersion"
      ( \x ->
          CoreDefinitionVersion'
            Core.<$> (x Core..:? "Cores" Core..!= Core.mempty)
      )

instance Core.Hashable CoreDefinitionVersion

instance Core.NFData CoreDefinitionVersion

instance Core.ToJSON CoreDefinitionVersion where
  toJSON CoreDefinitionVersion' {..} =
    Core.object
      (Core.catMaybes [("Cores" Core..=) Core.<$> cores])
