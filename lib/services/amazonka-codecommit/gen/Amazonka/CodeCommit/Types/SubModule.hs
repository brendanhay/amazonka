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
-- Module      : Amazonka.CodeCommit.Types.SubModule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.SubModule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a submodule reference in a repository folder.
--
-- /See:/ 'newSubModule' smart constructor.
data SubModule = SubModule'
  { -- | The fully qualified path to the folder that contains the reference to
    -- the submodule.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The commit ID that contains the reference to the submodule.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the submodule from the folder where the query
    -- originated.
    relativePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'subModule_absolutePath' - The fully qualified path to the folder that contains the reference to
-- the submodule.
--
-- 'commitId', 'subModule_commitId' - The commit ID that contains the reference to the submodule.
--
-- 'relativePath', 'subModule_relativePath' - The relative path of the submodule from the folder where the query
-- originated.
newSubModule ::
  SubModule
newSubModule =
  SubModule'
    { absolutePath = Prelude.Nothing,
      commitId = Prelude.Nothing,
      relativePath = Prelude.Nothing
    }

-- | The fully qualified path to the folder that contains the reference to
-- the submodule.
subModule_absolutePath :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_absolutePath = Lens.lens (\SubModule' {absolutePath} -> absolutePath) (\s@SubModule' {} a -> s {absolutePath = a} :: SubModule)

-- | The commit ID that contains the reference to the submodule.
subModule_commitId :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_commitId = Lens.lens (\SubModule' {commitId} -> commitId) (\s@SubModule' {} a -> s {commitId = a} :: SubModule)

-- | The relative path of the submodule from the folder where the query
-- originated.
subModule_relativePath :: Lens.Lens' SubModule (Prelude.Maybe Prelude.Text)
subModule_relativePath = Lens.lens (\SubModule' {relativePath} -> relativePath) (\s@SubModule' {} a -> s {relativePath = a} :: SubModule)

instance Data.FromJSON SubModule where
  parseJSON =
    Data.withObject
      "SubModule"
      ( \x ->
          SubModule'
            Prelude.<$> (x Data..:? "absolutePath")
            Prelude.<*> (x Data..:? "commitId")
            Prelude.<*> (x Data..:? "relativePath")
      )

instance Prelude.Hashable SubModule where
  hashWithSalt _salt SubModule' {..} =
    _salt `Prelude.hashWithSalt` absolutePath
      `Prelude.hashWithSalt` commitId
      `Prelude.hashWithSalt` relativePath

instance Prelude.NFData SubModule where
  rnf SubModule' {..} =
    Prelude.rnf absolutePath
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf relativePath
