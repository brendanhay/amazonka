{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a submodule reference in a repository folder.
--
-- /See:/ 'newSubModule' smart constructor.
data SubModule = SubModule'
  { -- | The commit ID that contains the reference to the submodule.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified path to the folder that contains the reference to
    -- the submodule.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the submodule from the folder where the query
    -- originated.
    relativePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { commitId = Prelude.Nothing,
      absolutePath = Prelude.Nothing,
      relativePath = Prelude.Nothing
    }

-- | The commit ID that contains the reference to the submodule.
subModule_commitId :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_commitId = Lens.lens (\SubModule' {commitId} -> commitId) (\s@SubModule' {} a -> s {commitId = a} :: SubModule)

-- | The fully qualified path to the folder that contains the reference to
-- the submodule.
subModule_absolutePath :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_absolutePath = Lens.lens (\SubModule' {absolutePath} -> absolutePath) (\s@SubModule' {} a -> s {absolutePath = a} :: SubModule)

-- | The relative path of the submodule from the folder where the query
-- originated.
subModule_relativePath :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_relativePath = Lens.lens (\SubModule' {relativePath} -> relativePath) (\s@SubModule' {} a -> s {relativePath = a} :: SubModule)

instance Prelude.FromJSON SubModule where
  parseJSON =
    Prelude.withObject
      "SubModule"
      ( \x ->
          SubModule'
            Prelude.<$> (x Prelude..:? "commitId")
            Prelude.<*> (x Prelude..:? "absolutePath")
            Prelude.<*> (x Prelude..:? "relativePath")
      )

instance Prelude.Hashable SubModule

instance Prelude.NFData SubModule
