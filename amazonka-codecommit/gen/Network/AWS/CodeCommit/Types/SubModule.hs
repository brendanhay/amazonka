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
-- Module      : Network.AWS.CodeCommit.Types.SubModule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SubModule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a submodule reference in a repository folder.
--
-- /See:/ 'newSubModule' smart constructor.
data SubModule = SubModule'
  { -- | The commit ID that contains the reference to the submodule.
    commitId :: Core.Maybe Core.Text,
    -- | The fully qualified path to the folder that contains the reference to
    -- the submodule.
    absolutePath :: Core.Maybe Core.Text,
    -- | The relative path of the submodule from the folder where the query
    -- originated.
    relativePath :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'subModule_commitId' - The commit ID that contains the reference to the submodule.
--
-- 'absolutePath', 'subModule_absolutePath' - The fully qualified path to the folder that contains the reference to
-- the submodule.
--
-- 'relativePath', 'subModule_relativePath' - The relative path of the submodule from the folder where the query
-- originated.
newSubModule ::
  SubModule
newSubModule =
  SubModule'
    { commitId = Core.Nothing,
      absolutePath = Core.Nothing,
      relativePath = Core.Nothing
    }

-- | The commit ID that contains the reference to the submodule.
subModule_commitId :: Lens.Lens' SubModule (Core.Maybe Core.Text)
subModule_commitId = Lens.lens (\SubModule' {commitId} -> commitId) (\s@SubModule' {} a -> s {commitId = a} :: SubModule)

-- | The fully qualified path to the folder that contains the reference to
-- the submodule.
subModule_absolutePath :: Lens.Lens' SubModule (Core.Maybe Core.Text)
subModule_absolutePath = Lens.lens (\SubModule' {absolutePath} -> absolutePath) (\s@SubModule' {} a -> s {absolutePath = a} :: SubModule)

-- | The relative path of the submodule from the folder where the query
-- originated.
subModule_relativePath :: Lens.Lens' SubModule (Core.Maybe Core.Text)
subModule_relativePath = Lens.lens (\SubModule' {relativePath} -> relativePath) (\s@SubModule' {} a -> s {relativePath = a} :: SubModule)

instance Core.FromJSON SubModule where
  parseJSON =
    Core.withObject
      "SubModule"
      ( \x ->
          SubModule'
            Core.<$> (x Core..:? "commitId")
            Core.<*> (x Core..:? "absolutePath")
            Core.<*> (x Core..:? "relativePath")
      )

instance Core.Hashable SubModule

instance Core.NFData SubModule
